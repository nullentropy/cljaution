(ns caution.core
  (:require [caution.server :as server]
            [caution.session :as session]
            [caution.wire :as wire]
            [clojure.string :as str]))

(def serve
  "Run a caution app. Returns a zero-arg fn that stops the server.

  Options:
    :port    listen port (default 8787)
    :init    initial per-session state - a value or a zero-arg fn
    :view    (fn [state session] hiccup) - required
    :app-js  path to the prebuilt terminal bundle
             (default \"../caution/dist/app.js\", a sibling caution
              checkout's `go run ./cmd/bundle` output; falls back to the
              caution/app.js on the classpath)
    :authorize (fn [req] identity) - runs on every connection including
             resumes; throw to reject. A refused client shows \"access
             denied\" and stops reconnecting. A resume only re-adopts a
             session whose identity matches.
    :resume-grace-ms how long a disconnected session waits for its client
             (default 60000)
    :allowed-origins  vector of allowed Origins (default: same-origin only,
             \"*\" for dev)
    :on-mount (fn [session]) - after a session is created (start tickers here)
    :on-close (fn [session]) - after it expires for good
    :routes  {\"/path\" ring-handler}. A \"/\" route replaces the host
             page, to gate the page load; hand off to host-page
    :static  {\"/prefix/\" \"directory\"}"
  server/serve)

(def state        "Current app state." session/state)
(def swap-state!  "Apply f to session state; re-renders and patches the client."
  session/swap-state!)
(def set-theme!   "Push design tokens: a map of token name -> color string."
  session/set-theme!)
(def set-metrics! "Push metric tokens: a map of token name -> logical-px number
  (\"radius.control\", \"border.width\", ...) - the geometry half of theming."
  session/set-metrics!)

(def compact-metrics
  "The dense look"
  {"radius.control" 4 "radius.popover" 6 "radius.panel" 8
   "control.height" 26 "control.padX" 10 "row.height" 22
   "table.headerHeight" 26 "table.cellPadX" 8 "checkbox.size" 15
   "slider.thumb" 7 "slider.track" 3 "dialog.titleHeight" 38
   "space.pad" 7 "space.gap" 7})

(def comfortable-metrics
  "The airy look"
  {"radius.control" 8 "radius.popover" 10 "radius.panel" 14
   "control.height" 38 "control.padX" 20 "row.height" 34
   "table.headerHeight" 38 "table.cellPadX" 16 "checkbox.size" 20
   "slider.thumb" 10 "slider.track" 5 "dialog.titleHeight" 56
   "space.pad" 13 "space.gap" 13})
(def set-title!   "Name the window (browser tab title / native titlebar)."
  session/set-title!)
(def set-motion!  "Decorative motion: :system (the OS preference, default),
  :full or :reduced. A :no-fade prop drops the insert fade for a subtree."
  session/set-motion!)
(def set-cookie!  "Store a cookie on the client, sent on every later connection:
  {:name \"login\" :value token :max-age 2592000}. A negative :max-age deletes.
  Read it back with (cookie (request s) \"login\")."
  session/set-cookie!)
(def close-session! "End the session now. The client reconnects into a fresh
  session and mount." session/close-session!)
(def host-page    "Ring handler for the terminal's host page, for a custom \"/\" route."
  server/host-page-handler)
(def set-keys!    "Register session-wide shortcuts: [{:key \"cmd+j\" :handler f} ...]."
  session/set-keys!)
(def set-aux!     "Handle mouse buttons 4 (back) and 5 (forward): (fn [state button])."
  session/set-aux!)
(def set-fullscreen! "Handle entering/leaving system fullscreen: (fn [state on?])."
  session/set-fullscreen!)
(def set-focus!   "Handle the window gaining/losing focus: (fn [state focused?])."
  session/set-focus!)
(def focused?     "Last known window focus, true until the client says otherwise."
  session/focused?)
(def preload!     "Warm client resource caches (images) ahead of first use."
  session/preload!)
(def preload-sounds! "Decode WAV sources on the client ahead of their first play!."
  session/preload-sounds!)
(def set-sounds!  "Set the gesture sound table: {\"press\" \"/click.wav\" ...}."
  session/set-sounds!)
(def play!        "Play a WAV once on the client (URL or data: URI)."
  session/play!)
(def loop!        "Play a WAV on repeat until stop!." session/loop!)
(def stop!        "Stop every playing instance of a source." session/stop!)
(def stop-all!    "Stop every sound." session/stop-all!)
(def set-menu!    "Replace the menu bar: a vector of {:title _ :items [...]},
  where an item is {:title _ :key \"n\" :on-pick (fn [state])}, {:sep true},
  or a submenu {:title _ :items [...]}. Realized natively (NSMenu) by the
  native terminal; picks run like any other handler. Rides mounts - set it
  in :on-mount and it arrives with the first paint."
  session/set-menu!)
(defn cookie
  "The value of cookie name on a ring request, or nil"
  [req name]
  (some (fn [pair]
          (let [[k v] (str/split (str/trim pair) #"=" 2)]
            (when (= k name) v)))
        (some-> (get-in req [:headers "cookie"]) (str/split #";"))))

(def identity-of  "Whatever :authorize returned for this session." session/identity-of)
(def request      "The HTTP request that opened the session." session/request)
(def viewport     "Client window size as [w h] logical px." session/viewport)
(def alive?       "False once the session has expired - stop background loops."
  session/alive?)
(def closed?      "True while no client is attached." session/closed?)
(def rerender-all!
  "Hot reload: re-render every live session in place (state survives). Views
  passed as vars (#'view) re-render on re-def automatically; call this after
  redefining helpers."
  session/rerender-all!)

(defn- queue-cmd [state k cmd]
  (update state ::cmds (fnil conj []) {:k k :cmd cmd}))

(defn focus
  "Ask the client to focus (and reveal) the node keyed k: its :key, or its
  \"name\" in a ui document. Compose into any handler:
      (fn [s] (-> s (assoc :panel :compose) (focus :composer)))
  The commands queue in state and go out with the next render."
  [state k]
  (queue-cmd state k "focus"))

(defn reveal
  "Scroll the node keyed k into view without taking focus."
  [state k]
  (queue-cmd state k "reveal"))

(defn clear-value
  "Force-clear a text field even while the user is editing it.
      (-> state (assoc :note \"\") (clear-value :composer))"
  [state k]
  (queue-cmd state k "clear"))

(defn override-value
  "Make the client take the value this render gives the node keyed k, even
  while the user is editing
      (-> state (assoc :prefix v') (override-value :prefix))"
  [state k]
  (queue-cmd state k "value"))

;; -- trees -----------------------------------------------------------------------
;;
;; A :tree node is the table in tree mode: same columns, same virtualized
;; rows, plus per-row hierarchy meta. The app owns the items and the expanded
;; set and these pure helpers flatten the visible slice into the shape the :rows fn must return.
;;
;;   [:tree {:columns [{:key "n" :title "Name" :weight 1}]
;;           :row-count (c/tree-row-count items (:open s))
;;           :rows (fn [s start end] (c/tree-rows items (:open s) start end))
;;           :on-toggle (fn [s v] (update s :open c/toggle-member (get v "key")))
;;           :on-row-select (fn [s v] (assoc s :picked (get v "key")))}]

(defn- tree-flatten [items expanded depth]
  (into []
        (mapcat (fn [{:keys [key cells kids]}]
                  (let [open? (and (contains? expanded key) (seq kids))]
                    (cons {:cells cells
                           :meta (cond-> {:key key :d depth}
                                   (seq kids) (assoc :k true)
                                   open? (assoc :x true))}
                          (when open? (tree-flatten kids expanded (inc depth)))))))
        items))

(defn tree-rows
  "The [start, end] window of a tree's visible rows, for a :tree node's
  :rows fn. items are {:key _ :cells [..] :kids [..]}; expanded is a set of
  open keys."
  [items expanded start end]
  (let [flat (tree-flatten items expanded 0)]
    (subvec flat (min start (count flat)) (min (inc end) (count flat)))))

(defn tree-row-count
  "How many rows the tree shows with this expanded set - the :row-count prop."
  [items expanded]
  (count (tree-flatten items expanded 0)))

(defn toggle-member
  "Set helper for :on-toggle handlers: flip k's membership."
  [s k]
  (let [s (or s #{})]
    (if (contains? s k) (disj s k) (conj s k))))

;; -- for tests and tooling --------------------------------------------------------

(def normalize "Hiccup -> internal node (types, props, handlers, kids)." wire/normalize)
(def ->wire    "Internal node -> the JSON shape the client inflates." wire/->wire)
