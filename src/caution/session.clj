(ns caution.session
  (:require [caution.diff :as d]
            [caution.wire :as w]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [org.httpkit.server :as hk])
  (:import [java.util.concurrent Executors ExecutorService TimeUnit
            ScheduledExecutorService]))

(defonce ^:private registry (atom {}))      ; token -> session
(defonce ^:private sid-counter (atom 0))
(defonce ^:private ^ScheduledExecutorService timer
  (Executors/newSingleThreadScheduledExecutor))

(defn- log [& args] (apply println "caution:" args))

(def ^:private debug?
  "CAUTION_DEBUG=1 logs every outgoing message"
  (some? (System/getenv "CAUTION_DEBUG")))

;; -- handler invocation --------------------------------------------------------

(defn- takes-value?
  [f]
  (some (fn [^java.lang.reflect.Method m]
          (and (= "invoke" (.getName m)) (= 2 (alength (.getParameterTypes m)))))
        (.getDeclaredMethods (class f))))

(defn- invoke-handler [f state value]
  (if (takes-value? f) (f state value) (f state)))

;; -- protocol output -----------------------------------------------------------

(defn- send-json! [sess msg]
  (when-let [ch (:channel @sess)]
    (hk/send! ch (json/write-str msg))))

(defn- kname [k]
  (if (or (keyword? k) (symbol? k)) (name k) (str k)))

;; a node is addressed by its :key, or by its "name" from a ui document
(defn- key-id [node k]
  (if (or (= (:key node) k)
          (and (some? (:key node)) (= (kname (:key node)) (kname k)))
          (= (get-in node [:props "name"]) (kname k)))
    (:id node)
    (some #(key-id % k) (:kids node))))

(defn- cmd-ops [tree index cmds]
  (keep (fn [{:keys [k cmd]}]
          (when-let [id (key-id tree k)]
            (cond-> {"op" "cmd" "id" id "cmd" cmd}
              (= cmd "value") (assoc "value" (get-in (index id) [:props "value"])))))
        cmds))

(declare drop-sent-cookies!)

(defn- send-mount! [sess]
  (let [m (swap! sess update :seq inc)
        focus (some->> (:focus-key m) (key-id (:tree m)))]
    (when debug? (log (format "session %d -> mount" (:sid m))))
    (when (send-json! sess (cond-> {"t" "mount" "seq" (:seq m) "sid" (:token m)
                              "root" (w/->wire (:tree m))}
                       focus         (assoc "focus" focus)
                       (:theme m)    (assoc "theme" (:theme m))
                       (:metrics m)  (assoc "metrics" (:metrics m))
                       (:sounds m)   (assoc "sounds" (:sounds m))
                       (seq (:loops m)) (assoc "loops" (:loops m))
                       (:menu m)     (assoc "menu" (:menu m))
                       (:title m)    (assoc "title" (:title m))
                       (:motion m)   (assoc "motion" (name (:motion m)))
                       (seq (:cookies m)) (assoc "cookies" (:cookies m))
                       (:keys m)     (assoc "keys" (:keys m))
                       (or (:preloads m) (:sound-preloads m))
                       (assoc "resources"
                              (cond-> {}
                                (:preloads m)       (assoc "images" (:preloads m))
                                (:sound-preloads m) (assoc "sounds" (:sound-preloads m))))))
      (drop-sent-cookies! sess (count (:cookies m))))))

(defn- send-ops! [sess ops]
  (when (seq ops)
    (let [m (swap! sess update :seq inc)]
      (when debug?
        (log (format "session %d -> patch: %s%s" (:sid m)
                     (frequencies (map #(get % "op") ops))
                     (if-let [r (first (filter #(= "rows" (get % "op")) ops))]
                       (format " start=%s n=%s first=%s"
                               (get r "start") (count (get r "rows"))
                               (first (get r "rows")))
                       ""))))
      (send-json! sess {"t" "patch" "seq" (:seq m) "ops" (vec ops)}))))

;; -- rows (virtualized tables) --------------------------------------------------

(defn- table-rows
  [node state start end]
  (when-let [f (:rows node)]
    (let [raw   (vec (f state start end))
          tree? (map? (first raw))]
      {:rows (mapv (fn [r] (mapv str (if tree? (:cells r) r))) raw)
       :meta (when tree?
               (mapv (fn [{{:keys [key d k x]} :meta}]
                       (cond-> {"key" (str key) "d" (or d 0)}
                         k (assoc "k" true)
                         x (assoc "x" true)))
                     raw))})))

(defn- rows-op [id start reset {:keys [rows meta]}]
  (cond-> {"op" "rows" "id" id "start" start "reset" reset "rows" rows}
    meta (assoc "meta" meta)))

(defn- refresh-rows!
  [sess]
  (let [{:keys [index ranges rows-cache !state]} @sess
        state @!state]
    (doseq [[id [start end]] ranges
            :let [node (get index id)
                  w (table-rows node state start end)]
            :when (and w (not= w (get rows-cache id)))]
      (swap! sess assoc-in [:rows-cache id] w)
      (send-ops! sess [(rows-op id start false w)]))))

;; -- render loop -----------------------------------------------------------------

(declare crash-overlay schedule-render!)

(defn- render! [sess]
  (let [{:keys [view !state tree next-id overlays]} @sess
        [state] (swap-vals! !state dissoc :caution.core/cmds)
        cmds (:caution.core/cmds state)
        app (w/normalize (view state sess))
        ;; Session-managed overlays (crash dialogs) ride along as extra root
        ;; children so the diff treats them like any other node.
        new (update app :kids into overlays)
        {:keys [tree ops next-id remount?]} (d/reconcile tree new next-id)
        index (w/index-tree tree)
        cmd-ops (cmd-ops tree index cmds)]
    (swap! sess assoc :tree tree :next-id next-id :index index)
    (when-let [k (:k (last (filter #(= "focus" (:cmd %)) cmds)))]
      (swap! sess assoc :focus-key k))
    (if remount?
      (do (send-mount! sess) (send-ops! sess cmd-ops))
      (send-ops! sess (into (vec ops) cmd-ops)))
    (refresh-rows! sess)))

(defn- guard
  [sess what f]
  (try
    (f)
    (catch Throwable t
      (log (format "session %d error in %s: %s" (:sid @sess) what (.getMessage t)))
      (.printStackTrace t)
      (try
        (swap! sess update :overlays conj (crash-overlay sess (str t)))
        (render! sess)
        (catch Throwable t2
          (log "failed to show crash dialog:" (.getMessage t2)))))))

(defn- post!
  [sess what f]
  (let [^ExecutorService ex (:executor @sess)]
    (when-not (.isShutdown ex)
      (.execute ex ^Runnable (fn [] (guard sess what f))))))

(defn- schedule-render! [sess]
  (when (compare-and-set! (:dirty @sess) false true)
    (post! sess "render"
           (fn []
             (reset! (:dirty @sess) false)
             (render! sess)))))

;; -- public session API ------------------------------------------------------------

(defn state
  "Current app state."
  [sess] @(:!state @sess))

(defn swap-state!
  "Apply f to the session's state"
  [sess f & args]
  (apply swap! (:!state @sess) f args)
  nil)

(defn set-theme!
  "Push design tokens (a map of token name -> color string)"
  [sess tokens]
  (swap! sess assoc :theme tokens)
  (send-ops! sess [{"op" "theme" "tokens" tokens}])
  nil)

(defn set-metrics!
  "Push metric tokens (a map of token name -> logical-px number)"
  [sess tokens]
  (swap! sess assoc :metrics tokens)
  (send-ops! sess [{"op" "metrics" "metrics" tokens}])
  nil)

(defn set-motion!
  "How much decorative motion the client plays (the insert fade, the reflow
  slide, the theme crossfade): :system follows the OS reduce-motion
  preference and is the default, :full and :reduced override it"
  [sess motion]
  (swap! sess assoc :motion motion)
  (send-ops! sess [{"op" "motion" "motion" (name motion)}])
  nil)

;; -- cookies -----------------------------------------------------------------------

(defn- drop-sent-cookies! [sess n]
  (swap! sess update :cookies #(vec (drop n %))))

;; unlike ops these wait out a disconnect, a resume can't rebuild them from
;; the tree. the seq only moves when something is actually sent so a resume
;; ack still matches
(defn- flush-cookies! [sess]
  (let [cs (:cookies @sess)]
    (when (and (seq cs) (:channel @sess))
      (let [m (swap! sess update :seq inc)
            ops (mapv (fn [c] {"op" "cookie" "cookie" c}) cs)]
        (when (send-json! sess {"t" "patch" "seq" (:seq m) "ops" ops})
          (drop-sent-cookies! sess (count cs)))))))

(defn- set-cookie-str [{:keys [name value path domain max-age secure same-site]}]
  (let [name (kname name)
        value (str value)]
    (when (and (re-matches #"[!#$%&'*+\-.^_`|~0-9A-Za-z]+" name)
               (re-matches #"[\x21\x23-\x2B\x2D-\x3A\x3C-\x5B\x5D-\x7E]*" value))
      (cond-> (str name "=" value "; Path=" (or path "/"))
        domain    (str "; Domain=" domain)
        max-age   (str "; Max-Age=" (max 0 max-age))
        secure    (str "; Secure")
        same-site (str "; SameSite=" (str/capitalize (kname same-site)))))))

(defn set-cookie!
  "Store a cookie on the client, kept by the browser or in a per-app file
  natively and sent on every later connection, so the next session's
  (request sess) and :authorize see it. c is {:name :value :max-age :path
  :domain :secure :same-site}, a negative :max-age deletes. Path defaults to /"
  [sess c]
  (if-let [raw (set-cookie-str c)]
    (do (swap! sess update :cookies (fnil conj []) raw)
        (flush-cookies! sess))
    (log (format "session %d dropped invalid cookie %s" (:sid @sess) (pr-str (:name c)))))
  nil)

(defn set-title!
  "Name the window: the browser tab's title, the native titlebar."
  [sess title]
  (swap! sess assoc :title title)
  (send-ops! sess [{"op" "title" "title" title}])
  nil)

(defn preload!
  "Warm the client's resource caches ahead of first use"
  [sess & srcs]
  (let [known (set (:preloads @sess))
        fresh (vec (remove known srcs))]
    (when (seq fresh)
      (swap! sess update :preloads (fnil into []) fresh)
      (send-ops! sess [{"op" "resource" "images" fresh}])))
  nil)

(defn set-sounds!
  "Set the gesture table: token -> WAV source, fired by the client at the
  moment of the gesture. Tokens are press, toggle, select, open, close and
  type; the ones left out are silent"
  [sess tokens]
  (swap! sess assoc :sounds tokens)
  (send-ops! sess [{"op" "sounds" "tokens" tokens}])
  nil)

(defn preload-sounds!
  "Decode WAV sources on the client ahead of their first play!"
  [sess & srcs]
  (let [known (set (:sound-preloads @sess))
        fresh (vec (remove known srcs))]
    (when (seq fresh)
      (swap! sess update :sound-preloads (fnil into []) fresh)
      (send-ops! sess [{"op" "resource" "sounds" fresh}])))
  nil)

(defn play!
  "Play a WAV once on the client, by URL or data: URI. Fire and forget: a
  browser drops sounds until the user has clicked or typed in the page"
  [sess src]
  (send-ops! sess [{"op" "play" "src" src}])
  nil)

(defn loop!
  "Play src on repeat until stop!"
  [sess src]
  (when-not (some #{src} (:loops @sess))
    (swap! sess update :loops (fnil conj []) src)
    (send-ops! sess [{"op" "play" "src" src "loop" true}]))
  nil)

(defn stop!
  "Stop every playing instance of src, looping or not"
  [sess src]
  (swap! sess update :loops (fn [ls] (vec (remove #{src} ls))))
  (send-ops! sess [{"op" "stop" "src" src}])
  nil)

(defn stop-all!
  "Stop every sound"
  [sess]
  (swap! sess assoc :loops nil)
  (send-ops! sess [{"op" "stop"}])
  nil)

(defn set-keys!
  "Register session-wide shortcuts: a vector of {:key \"cmd+j\" :handler f}
  where f is (fn [state])"
  [sess combos]
  (let [ks (vec (map-indexed (fn [i c] {"id" (inc i) "key" (:key c)}) combos))
        hs (into {} (map-indexed (fn [i c] [(inc i) (:handler c)]) combos))]
    (swap! sess assoc :keys ks :key-handlers hs)
    (send-ops! sess [{"op" "keys" "keys" ks}]))
  nil)

(defn set-aux!
  "Handle the mouse's extra buttons, 4 (back) and 5 (forward), where f is
  (fn [state button]). Both terminals swallow the press whether or not an app
  takes it, so the browser never navigates its history out from under it"
  [sess f]
  (swap! sess assoc :aux-handler f)
  nil)

(defn set-fullscreen!
  "Handle the window entering and leaving the system's own fullscreen, where f
  is (fn [state on?]). A custom titlebar usually wants to restyle there, since
  fullscreen has no traffic lights to leave room for"
  [sess f]
  (swap! sess assoc :fullscreen-handler f)
  nil)

(defn set-focus!
  "Handle the client window gaining and losing focus, where f is
  (fn [state focused?]). A hidden browser tab counts as unfocused"
  [sess f]
  (swap! sess assoc :focus-handler f)
  nil)

(defn focused?
  "Last known client window focus, true until the client says otherwise"
  [sess] (not (:unfocused @sess false)))

(defn set-menu!
  "Replace the session's menu bar (a vector of menu maps; see
  caution.core/set-menu! for the shape)"
  [sess menus]
  (let [{:keys [menu handlers]} (w/menus->wire menus)]
    (swap! sess assoc :menu menu :menu-handlers handlers)
    (send-ops! sess [{"op" "menu" "menu" menu}]))
  nil)

(defn identity-of
  "Whatever :authorize returned for the connection that opened this session"
  [sess] (:identity @sess))

(defn request
  "The HTTP request that opened the session (cookies, headers, remote addr)"
  [sess] (:request @sess))

(defn viewport
  "The client's last reported window size in logical px, as [w h]"
  [sess] [(:vw @sess) (:vh @sess)])

(defn closed?
  "True while no client is attached (may still be within the resume grace)"
  [sess] (:closed @sess false))

(defn alive?
  "False once the session has expired"
  [sess] (not (:expired @sess false)))

(defn rerender-all!
  "Re-render every live session against its current state"
  []
  (let [sessions (vals @registry)]
    (doseq [sess sessions] (schedule-render! sess))
    (count sessions)))

(defn- crash-overlay [sess msg]
  (let [k (str (gensym "crash"))
        dismiss (fn [state]
                  (swap! sess update :overlays
                         (fn [os] (vec (remove #(= k (:key %)) os))))
                  (schedule-render! sess)
                  state)]
    (w/normalize
     [:dialog {:key k :title "Internal error" :card-width 540 :card-height 190
               :on-dismiss dismiss}
      [:label {:text "a server-side handler threw; the app may be in an inconsistent state"
               :size 13 :color "$inkDim" :anchors {:left 20 :top 14 :right 20}}]
      [:label {:text msg :size 12 :mono true :color "#ff6b6b" :selectable true
               :anchors {:left 20 :top 44 :right 20}}]
      [:button {:label "Dismiss" :on-click dismiss
                :anchors {:right 20 :bottom 16}}]])))

;; -- event dispatch --------------------------------------------------------------

(def ^:private echo-props
  "Events whose new value the client already shows: sync our copy of the tree
  quietly so the next diff doesn't patch it back."
  {"toggle"       (fn [v] {"checked" v})
   "input"        (fn [v] {"value" v})
   "commit"       (fn [v] {"value" v})
   "submit"       (fn [v] {"value" v})
   "select"       (fn [v] {"selected" v})
   ;; Keyed tables send {"row" i "key" k}, and selection identity is the key.
   "row-select"   (fn [v] (if (map? v)
                            {"selectedKey" (get v "key")}
                            {"selected" v}))
   "split-resize" (fn [v] {"pos" v})
   "sort"         (fn [v] {"sortKey" (get v "key")
                           "sortDir" (if (get v "asc") "asc" "desc")})})

(defn- echo-into-tree
  "Apply the local echo to our mounted copy of the node."
  [tree id props]
  (if (= (:id tree) id)
    (update tree :props merge props)
    (update tree :kids (fn [ks] (mapv #(echo-into-tree % id props) ks)))))

(defn- dispatch! [sess {:strs [id ev value]}]
  (let [id (long (or id 0))]
    (cond
      ;; Session-level events carry node id 0, because no widget owns them.
      (zero? id)
      (case ev
        "resize" (let [w (get value "w") h (get value "h")]
                   (when (and w h (pos? w))
                     (swap! sess assoc :vw w :vh h)
                     (schedule-render! sess)))
        "menu"   (when-let [h (and (number? value)
                                   (get (:menu-handlers @sess) (long value)))]
                   (swap! (:!state @sess) (fn [s] (or (h s) s))))
        "key"    (when-let [h (and (number? value)
                                   (get (:key-handlers @sess) (long value)))]
                   (swap! (:!state @sess) (fn [s] (or (h s) s))))
        "aux"    (when-let [h (and (number? value) (:aux-handler @sess))]
                   (swap! (:!state @sess) (fn [s] (or (h s (long value)) s))))
        "fullscreen" (do (swap! sess assoc :fullscreen (boolean value))
                          (when-let [h (:fullscreen-handler @sess)]
                            (swap! (:!state @sess)
                                   (fn [s] (or (h s (boolean value)) s)))))
        "focus"  (do (swap! sess assoc :unfocused (not value))
                     (when-let [h (:focus-handler @sess)]
                       (swap! (:!state @sess)
                              (fn [s] (or (h s (boolean value)) s)))))
        nil)

      :else
      (when-let [node (get (:index @sess) id)]
        (when-let [echo (echo-props ev)]
          (let [props (echo value)]
            (swap! sess (fn [s] (-> s
                                    (update :tree echo-into-tree id props)
                                    (update-in [:index id :props] merge props))))))
        (if (= ev "visible-range")
          (let [start (long (get value "start" 0))
                end   (long (get value "end" 0))
                w     (table-rows node (state sess) start end)]
            (swap! sess assoc-in [:ranges id] [start end])
            (when w
              (swap! sess assoc-in [:rows-cache id] w)
              (send-ops! sess [(rows-op id start false w)])))
          (when-let [h (get (:handlers node) ev)]
            (swap! (:!state @sess)
                   (fn [s] (or (invoke-handler h s value) s)))))))))

;; -- lifecycle ---------------------------------------------------------------------

(defn open!
  "Bind a fresh or resumed session to a channel and (re)mount from the current
  tree. client-seq is the last seq the resuming client applied (nil for fresh
  connections): when it still matches ours, nothing changed while the client
  was away"
  [sess ch client-seq]
  (swap! sess assoc :channel ch :closed false)
  (post! sess "mount"
         (fn []
           (cond
             (and client-seq (:tree @sess) (= client-seq (:seq @sess)))
             (do (send-json! sess {"t" "resume" "seq" (:seq @sess)})
                 (flush-cookies! sess)) ; set while the client was away

             (:tree @sess)
             (do (send-mount! sess)                ; resume with changes: replay
                 (swap! sess assoc :rows-cache {})
                 (refresh-rows! sess))

             :else
             (do (render! sess)                    ; first mount: build it
                 (swap! sess assoc :rows-cache {})
                 (refresh-rows! sess))))))

(defn- expire! [sess]
  (when (and (:closed @sess) (nil? (:channel @sess)))
    (log (format "session %d closed" (:sid @sess)))
    (swap! sess assoc :expired true)
    (swap! registry dissoc (:token @sess))
    (remove-watch (:!state @sess) ::render)
    (when (var? (:view @sess))
      (remove-watch (:view @sess)
                    (keyword "caution.session" (str "reload-" (:sid @sess)))))
    (.shutdown ^ExecutorService (:executor @sess))
    (when-let [on-close (:on-close @sess)]
      (try (on-close sess) (catch Throwable t (log "on-close threw:" (.getMessage t)))))))

(defn close! [sess ch]
  (when (= ch (:channel @sess))
    (swap! sess assoc :channel nil :closed true)
    (log (format "session %d disconnected - resumable for %dms"
                 (:sid @sess) (:grace-ms @sess)))
    (.schedule timer ^Runnable (fn [] (expire! sess))
               (long (:grace-ms @sess)) TimeUnit/MILLISECONDS)))

(defn close-session!
  "End the session now. The client reconnects into a fresh session and a
  fresh mount"
  [sess]
  (let [ch (:channel @sess)]
    (swap! sess assoc :channel nil :closed true)
    (expire! sess)
    ;; after expire! drops it from the registry, so the client's reconnect
    ;; finds nothing to resume
    (some-> ch hk/close)
    nil))

(defn new-session
  [{:keys [view init identity request vw vh on-mount on-close resume-grace-ms]}]
  (let [!state (atom (if (fn? init) (init) (or init {})))
        sess (atom {:sid      (swap! sid-counter inc)
                    :token    (str (java.util.UUID/randomUUID))
                    :view     view
                    :!state   !state
                    :identity identity
                    :request  request
                    :vw       (or vw 0)
                    :vh       (or vh 0)
                    :on-close on-close
                    :grace-ms (or resume-grace-ms 60000)
                    :tree     nil
                    :index    {}
                    :overlays []
                    :ranges   {}
                    :rows-cache {}
                    :next-id  1
                    :seq      0
                    :dirty    (atom false)
                    :executor (Executors/newSingleThreadExecutor)})]
    (add-watch !state ::render (fn [_ _ old new]
                                 (when (not= old new) (schedule-render! sess))))
    ;; Hot reload: when the view is a var (serve {:view #'view ...}), watch it -
    ;; re-defing the view at the REPL re-renders this session immediately.
    (when (var? view)
      (add-watch view (keyword "caution.session" (str "reload-" (:sid @sess)))
                 (fn [_ _ old new]
                   (when-not (identical? old new) (schedule-render! sess)))))
    (swap! registry assoc (:token @sess) sess)
    (when on-mount
      (try (on-mount sess)
           (catch Throwable t (log "on-mount threw:" (.getMessage t)))))
    sess))

(defn lookup [token] (get @registry token))

(defn- allow-event?
  "Token bucket: burst 240, refill 120/s"
  [sess]
  (let [now (System/nanoTime)
        {:keys [ev-tokens ev-last]} @sess
        tokens (if ev-last
                 (min 240.0 (+ ev-tokens (* (/ (- now ev-last) 1e9) 120.0)))
                 240.0)
        ok? (>= tokens 1.0)]
    (swap! sess assoc :ev-tokens (if ok? (dec tokens) tokens) :ev-last now)
    ok?))

(defn receive! [sess raw]
  (let [msg (json/read-str raw)]
    (when (= "ev" (get msg "t"))
      (if (allow-event? sess)
        (post! sess (str "event " (get msg "ev"))
               (fn [] (dispatch! sess msg)))
        (log (format "session %d event flood - dropping %s"
                     (:sid @sess) (get msg "ev")))))))

(defn resumable?
  "A resume token only re-adopts a session minted for the same identity."
  [sess identity]
  (and sess (:closed @sess) (= (:identity @sess) identity)))
