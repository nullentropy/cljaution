(ns caution.demo
  (:require [caution.core :as c]
            [clojure.string :as str])
  (:import [java.time LocalTime]
           [java.time.format DateTimeFormatter])
  (:gen-class))

;; -- data -------------------------------------------------------------------------

(def ^:private firsts
  ["Ada" "Grace" "Alan" "Edsger" "Barbara" "Donald" "Ken" "Dennis" "Bjarne"
   "Guido" "Linus" "Margaret"])
(def ^:private lasts
  ["Lovelace" "Hopper" "Turing" "Dijkstra" "Liskov" "Knuth" "Thompson"
   "Ritchie" "Stroustrup" "Rossum" "Torvalds" "Hamilton"])
(def ^:private roles ["Engineer" "Designer" "Manager" "Analyst" "Support" "Ops"])

(def employees
  (vec (for [i (range 10000)
             :let [f (nth firsts (mod i (count firsts)))
                   l (nth lasts (mod (quot i (count firsts)) (count lasts)))]]
         {:id (format "E%05d" (inc i))
          :name (str f " " l)
          :email (str (.toLowerCase f) "." (.toLowerCase l) i "@example.com")
          :role (nth roles (mod (* i 7) (count roles)))})))

(def ^:private sorted-employees
  (memoize
   (fn [k asc]
     (let [kw (keyword (or k "id"))
           v  (vec (sort-by kw employees))]
       (if asc v (vec (rseq v)))))))

(def themes
  {"Midnight" {"bg" "#16181d" "ink" "#e8eaf0" "inkDim" "#9aa3b2" "inkFaint" "#6b7280"
               "panel" "#262b33" "panelAlt" "#20252c" "panelInset" "#1b1f26"
               "edge" "#3a4150" "edgeSoft" "#333a46" "titlebar" "#2e343e"
               "control" "#2e343e" "controlEdge" "#4a5262" "accent" "#4f8cff"}
   "Violet"   {"bg" "#151221" "ink" "#eae6f5" "inkDim" "#a49bc0" "inkFaint" "#746b91"
               "panel" "#251f38" "panelAlt" "#1f1a30" "panelInset" "#191428"
               "edge" "#3d3459" "edgeSoft" "#352d4e" "titlebar" "#2d2545"
               "control" "#2d2545" "controlEdge" "#4d4273" "accent" "#a78bfa"}
   "Light"    {"bg" "#eef0f4" "ink" "#1b1e24" "inkDim" "#5b6472" "inkFaint" "#8a93a2"
               "panel" "#ffffff" "panelAlt" "#f7f8fa" "panelInset" "#eceef2"
               "edge" "#d4d9e0" "edgeSoft" "#e2e6ec" "titlebar" "#e8ebef"
               "control" "#f1f2f5" "controlEdge" "#c6cdd7" "accent" "#2563eb"}})

(def theme-names ["Midnight" "Violet" "Light"])

(def crt-frag "
vec4 effect(vec2 uv) {
  vec2 c = uv * 2.0 - 1.0;
  c *= 1.0 + 0.06 * dot(c, c);
  vec2 suv = (c + 1.0) * 0.5;
  vec4 col = src(suv);
  float scan = 0.86 + 0.14 * sin(suv.y * u_res.y * 3.14159);
  col.rgb = mix(col.rgb, col.rgb * vec3(0.5, 1.1, 0.55), 0.6) * scan;
  col.rgb *= 1.0 - 0.3 * dot(c, c);
  return col;
}")

(def torus-frag "
float sdTorus(vec3 p, vec2 t) {
  vec2 q = vec2(length(p.xz) - t.x, p.y);
  return length(q) - t.y;
}
vec3 rot(vec3 p) {
  float ca = cos(u_time * 0.6), sa = sin(u_time * 0.6);
  float cb = cos(u_time * 0.37), sb = sin(u_time * 0.37);
  p.xz = mat2(ca, -sa, sa, ca) * p.xz;
  p.xy = mat2(cb, -sb, sb, cb) * p.xy;
  return p;
}
float map(vec3 p) { return sdTorus(rot(p), vec2(0.82, 0.34)); }
vec4 effect(vec2 uv) {
  vec2 sc = uv * 2.0 - 1.0;
  sc.x *= u_res.x / u_res.y;
  sc.y = -sc.y;
  vec3 ro = vec3(0.0, 0.0, -2.7);
  vec3 rd = normalize(vec3(sc, 1.7));
  float t = 0.0; float d = 1e9;
  for (int i = 0; i < 72; i++) {
    d = map(ro + rd * t);
    if (d < 0.001 || t > 7.0) break;
    t += d;
  }
  vec3 col = vec3(0.05, 0.055, 0.07);
  if (d < 0.01) {
    vec3 p = ro + rd * t;
    vec2 e = vec2(0.0025, 0.0);
    vec3 n = normalize(vec3(map(p + e.xyy) - map(p - e.xyy),
                            map(p + e.yxy) - map(p - e.yxy),
                            map(p + e.yyx) - map(p - e.yyx)));
    vec3 l = normalize(vec3(0.5, 0.8, -0.6));
    float dif = clamp(dot(n, l), 0.0, 1.0);
    float spe = pow(clamp(dot(reflect(-l, n), -rd), 0.0, 1.0), 24.0);
    col = vec3(0.31, 0.55, 1.0) * (0.12 + 0.88 * dif) + spe * 0.55;
  }
  return vec4(col, 1.0);
}")

(defn bump [n] (fn [state] (-> state (update :count + n) (update :events inc))))

(defn add-row [state]
  (let [n (inc (:row-n state))]
    (-> state
        (update :events inc)
        (assoc :row-n n)
        (update :rows conj {:n n
                            :label (format "%s %02d - inserted by the Clojure server at %s"
                                           (:prefix state) n (.format (LocalTime/now)
                                                                      (DateTimeFormatter/ofPattern "HH:mm:ss")))}))))

(defn clear-rows [state]
  (-> state (update :events inc) (assoc :rows [] :row-n 0 :confirm? false)))

(defn open-confirm
  "Raise the clear-rows confirmation"
  [state]
  (-> state (update :events inc) (assoc :confirm? true)))

(defn apply-theme
  "A state transition that switches to theme i"
  [session i]
  (fn [state]
    (c/set-theme! session (themes (nth theme-names i)))
    (-> state (assoc :theme-idx i) (update :events inc))))

(defn commit-prefix [state v]
  (when (= v "boom")
    (throw (ex-info "intentional demo failure - dismiss and keep going" {})))
  (let [v' (str/trim v)]
    (-> state (update :events inc)
        (assoc :prefix v' :mirror (format "server sees: %s (committed)" (pr-str v')))
        (cond-> (not= v' v) (c/override-value :prefix)))))

;; -- view: state -> hiccup ------------------------------------------------------------

(defn- dot [x color]
  [:panel {:frame [x 14 12 12] :bg color :radius 6}])

(defn- dim [text]
  [:label {:text text :size 13 :color "$inkDim"}])

(defn- window
  [{:keys [count auto prefix mirror] :as state} session]
  [:panel {:bg "$panel" :radius 12 :border-color "$edge" :border-width 1
           :shadow {:blur 32 :dx 0 :dy 14 :color "#00000080"}}
   [:dock {:anchors {:left 0 :right 0 :top 0 :bottom 0}}
    [:panel {:dock "top" :height 40 :bg "$titlebar" :radius {:tl 12 :tr 12 :br 0 :bl 0}}
     (dot 16 "#ff5f57") (dot 36 "#febc2e") (dot 56 "#28c840")
     [:label {:text "Server Counter" :size 13 :weight 600 :color "$inkDim"
              :anchors {:center-x 0 :center-y 0}}]]
    [:panel {:dock "top" :height 1 :bg "$edge"}]
    [:panel {:dock "bottom" :height 64}
     [:hstack {:spacing 8 :anchors {:left 20 :bottom 20}}
      [:button {:label "−" :on-click (bump -1)}]
      [:button {:label "+" :primary true :on-click (bump 1)}]
      [:button {:label "Add row" :on-click add-row}]
      [:button {:label "Clear rows…" :on-click open-confirm}]]]
    [:panel {:dock "fill" :clips true}
     [:label {:text (str count) :size 28 :weight 600 :anchors {:left 24 :top 20}}]
     [:label {:text "this number lives in Clojure: the view is a pure function of state"
              :size 13 :color "$inkDim" :selectable true
              :anchors {:left 24 :top 66 :right 24}}]
     [:checkbox {:label "Auto-increment every second (server push, no clicks)"
                 :checked auto
                 :on-toggle (fn [s v] (-> s (assoc :auto v) (update :events inc)))
                 :anchors {:left 24 :top 106}}]
     [:grid {:columns [{:kind "content" :align "end"}
                       {:kind "fill" :weight 1 :align "stretch"}]
             :col-gap 12 :row-gap 10
             :anchors {:left 24 :top 144 :right 24}}
      (dim "Row prefix")
      [:textfield {:value prefix :sensitive true
                   :placeholder "row prefix: Enter commits (try \"boom\")"
                   :on-input (fn [s v] (assoc s :mirror (format "server sees: %s (input)" (pr-str v))))
                   :on-commit commit-prefix}]
      (dim "Theme")
      [:select {:options ["Midnight" "Violet" "Light"]
                :selected (:theme-idx state)
                :on-select (fn [s i] ((apply-theme session i) s))}]
      [:label {:text mirror :size 13 :color "$inkDim" :selectable true :span 2}]]]]])

(defn- rows-panel [{:keys [rows]}]
  [:panel {:bg "$panelAlt" :radius 12 :border-color "$edgeSoft" :border-width 1
           :effect {:frag crt-frag}}
   [:scroll {:inset-bottom 16
             :anchors {:left 1 :top 1 :right 1 :bottom 1}}
    (into [:vstack {:spacing 8 :anchors {:left 20 :top 16 :right 20}}
           [:label {:text "rows inserted by the server appear here"
                    :size 13 :color "$inkDim"}]]
          (for [{:keys [n label]} rows]
            [:label {:key n :text label :size 13 :selectable true}]))]])

(defn- table-panel [{:keys [sort-key sort-asc selected-row]}]
  [:panel {:bg "$panelAlt" :radius 12 :border-color "$edgeSoft" :border-width 1}
   [:label {:text (if selected-row
                    (str "Employees - selected " selected-row)
                    "Employees - 10,000 rows, virtualized; click headers to sort server-side")
            :size 13 :color "$inkDim"
            :anchors {:left 16 :top 12 :right 16}}]
   [:table {:columns [{:key "id" :title "ID" :width 90}
                      {:key "name" :title "Name" :weight 2}
                      {:key "email" :title "Email" :weight 3}
                      {:key "role" :title "Role" :weight 1}]
            :row-count (count employees)
            :sort-key sort-key
            :sort-dir (if sort-asc "asc" "desc")
            :rows (fn [state start end]
                    (let [rows (sorted-employees (:sort-key state) (:sort-asc state))]
                      (for [i (range (max 0 start) (min (count rows) (inc end)))
                            :let [e (nth rows i)]]
                        [(:id e) (:name e) (:email e) (:role e)])))
            :on-sort (fn [s v]
                       (-> s
                           (assoc :sort-key (get v "key") :sort-asc (get v "asc")
                                  :selected-row nil)
                           (update :events inc)))
            :on-row-select (fn [s i]
                             (let [rows (sorted-employees (:sort-key s) (:sort-asc s))
                                   e (nth rows i nil)]
                               (-> s
                                   (assoc :selected-row (when e (str (:id e) " · " (:name e))))
                                   (update :events inc))))
            :anchors {:left 16 :top 40 :right 16 :bottom 16}}]])

(defn header-title
  []
  "caution :: Clojure SDK")

(defn view [{:keys [confirm? now events who] :as state} session]
  [:panel {}
   [:dock {:anchors {:left 0 :right 0 :top 0 :bottom 0}}
    [:panel {:dock "top" :height 84}
     [:label {:text (header-title) :size 22 :weight 600
              :anchors {:left 32 :top 22}}]
     [:label {:text "the view is a pure function of state; the diff becomes protocol patches"
              :size 13 :color "$inkDim" :selectable true
              :anchors {:left 32 :top 56 :right 120}}]
     [:image {:src "/assets/avatar.png" :fit "cover" :radius 22 :alt (str "avatar for " who)
              :width 44 :height 44
              :anchors {:right 32 :center-y 0}}]]
    [:panel {:dock "bottom" :height 34}
     [:label {:text (format "%s · %d events handled · signed in as %s · one Clojure thread per session"
                            now events who)
              :size 12 :color "$inkFaint"
              :anchors {:left 32 :center-y 0 :right 16}}]]
    [:panel {:dock "fill"}
     [:vsplit {:pos 364 :min-a 280 :min-b 160
               :anchors {:left 32 :top 12 :right 32 :bottom 12}}
      [:hsplit {:pos 552 :min-a 520 :min-b 200}
       (window state session)
       [:vsplit {:pos 210 :min-a 120 :min-b 120}
        (rows-panel state)
        [:panel {:bg "$panelAlt" :radius 12 :border-color "$edgeSoft" :border-width 1}
         [:label {:text "raymarched in GLSL - u_time from the client clock"
                  :size 12 :color "$inkFaint"
                  :anchors {:left 14 :top 10 :right 14}}]
         [:shader {:frag torus-frag :animate true
                   :anchors {:left 12 :top 32 :right 12 :bottom 12}}]]]]
      (table-panel state)]]]
   (when confirm?
     [:dialog {:title "Clear all rows?" :card-width 440 :card-height 168
               :on-dismiss (fn [s] (assoc s :confirm? false))}
      [:label {:text "This removes every row the server inserted. There is no undo."
               :size 13 :color "$inkDim" :anchors {:left 20 :top 16}}]
      [:hstack {:spacing 10 :anchors {:right 20 :bottom 20}}
       [:button {:label "Cancel" :on-click (fn [s] (assoc s :confirm? false))}]
       [:button {:label "Clear rows" :primary true :on-click clear-rows}]]])])

;; -- wiring -----------------------------------------------------------------------------

(defn- now-str []
  (.format (LocalTime/now) (DateTimeFormatter/ofPattern "HH:mm:ss")))

(defn- on-mount
  "Record who connected, install the menu bar, then start the clock. Server
  push: a background thread nudges state once a second; the view re-renders
  and only the changed labels are patched."
  [session]
  (c/swap-state! session assoc :who (str (c/identity-of session)))
  (c/set-menu! session
               [{:title "Rows"
                 :items [{:title "Add Row" :key "n" :on-pick add-row}
                         {:sep true}
                         {:title "Clear Rows…" :key "shift+cmd+k" :on-pick open-confirm}]}
                {:title "Theme"
                 :items (vec (map-indexed (fn [i title]
                                            {:title title :on-pick (apply-theme session i)})
                                          theme-names))}])
  (future
    (loop []
      (Thread/sleep 1000)
      (when (c/alive? session)
        (c/swap-state! session
                       (fn [s] (cond-> (assoc s :now (str "server time " (now-str)))
                                 (:auto s) (update :count inc))))
        (recur)))))

(defn- avatar-png
  "A generated PNG so the demo has an image without shipping binary assets."
  []
  (let [size 128
        center (/ size 2.0)
        rings [[0x4f 0x8c 0xff] [0xa7 0x8b 0xfa] [0x22 0xc5 0x5e] [0x16 0x18 0x1d]]
        img (java.awt.image.BufferedImage. size size java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (doseq [y (range size) x (range size)
            :let [d (Math/hypot (- (+ x 0.5) center) (- (+ y 0.5) center))]
            :when (<= d center)]
      (let [[r g b] (nth rings (mod (int (/ d (/ center 4))) (clojure.core/count rings)))]
        (.setRGB img x y (unchecked-int (bit-or 0xff000000 (bit-shift-left r 16)
                                                (bit-shift-left g 8) b)))))
    (let [out (java.io.ByteArrayOutputStream.)]
      (javax.imageio.ImageIO/write img "png" out)
      (.toByteArray out))))

(defn -main [& _]
  (clojure.core.server/start-server
   {:name "caution-repl" :port 5877 :accept 'clojure.core.server/repl})
  (println "caution: socket REPL on :5877 - redefine views live")
  (let [png (avatar-png)]
    (c/serve
     {:port 8788
      :app-js "../caution/dist/app.js"
      :init (fn []
              {:count 0 :auto false :events 0 :rows [] :row-n 0
               :prefix "row" :mirror "server sees: \"row\"" :theme-idx 0
               :sort-key "id" :sort-asc true :selected-row nil :confirm? false
               :now (str "server time " (now-str)) :who "clojure"})
      ;; The var, not the value: redefining `view` hot-reloads live sessions
      :view #'view
      :on-mount on-mount
      :authorize (fn [req]
                   (or (some->> (get-in req [:headers "cookie"])
                                (re-find #"user=([^;]+)")
                                second)
                       "anonymous"))
      :routes {"/assets/avatar.png"
               (fn [_] {:status 200
                        :headers {"Content-Type" "image/png"
                                  "Cache-Control" "public, max-age=3600"}
                        :body (java.io.ByteArrayInputStream. png)})}}))
  @(promise))
