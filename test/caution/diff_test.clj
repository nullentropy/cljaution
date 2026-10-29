(ns caution.diff-test
  (:require [caution.diff :as d]
            [caution.session :as session]
            [caution.wire :as w]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]))

(defn- mount [hiccup]
  (:tree (d/reconcile nil (w/normalize hiccup) 1)))

(defn- patch
  [old-hiccup new-hiccup]
  (:ops (d/reconcile (mount old-hiccup) (w/normalize new-hiccup) 1000)))

(defn- ops-of [ops] (frequencies (map #(get % "op") ops)))

(defn- kid-texts
  [old-hiccup new-hiccup]
  (let [old (mount old-hiccup)
        {:keys [tree]} (d/reconcile old (w/normalize new-hiccup) 1000)]
    (mapv #(get-in % [:props "text"]) (:kids tree))))

;; -- props -------------------------------------------------------------------------

(deftest identical-trees-produce-no-ops
  (testing "an unchanged view costs nothing on the wire"
    (is (= [] (patch [:panel {:bg "$panel"} [:label {:text "x"}]]
                     [:panel {:bg "$panel"} [:label {:text "x"}]])))))

(deftest only-changed-props-are-sent
  (let [ops (patch [:label {:text "a" :size 13 :color "$ink"}]
                   [:label {:text "b" :size 13 :color "$ink"}])]
    (is (= 1 (clojure.core/count ops)))
    (is (= {"text" "b"} (get (first ops) "p"))
        "size and color are unchanged, so they stay off the wire")))

(deftest removed-props-are-nulled
  (is (= {"color" nil}
         (get (first (patch [:label {:text "x" :color "$ink"}] [:label {:text "x"}])) "p"))
      "the client's prop parsers coerce null back to the default"))

;; -- children ----------------------------------------------------------------------

(deftest append-emits-one-insert
  (is (= {"insert" 1}
         (ops-of (patch [:vstack {} [:label {:key 1 :text "one"}]]
                        [:vstack {} [:label {:key 1 :text "one"}]
                                    [:label {:key 2 :text "two"}]])))))

(deftest remove-from-middle-emits-one-remove
  (is (= {"remove" 1}
         (ops-of (patch [:vstack {} [:label {:key 1 :text "a"}]
                                    [:label {:key 2 :text "b"}]
                                    [:label {:key 3 :text "c"}]]
                        [:vstack {} [:label {:key 1 :text "a"}]
                                    [:label {:key 3 :text "c"}]])))))

(deftest keyed-reorder-moves-instead-of-rebuilding
  (let [old [:vstack {} [:label {:key 1 :text "a"}]
                        [:label {:key 2 :text "b"}]
                        [:label {:key 3 :text "c"}]]
        new [:vstack {} [:label {:key 3 :text "c"}]
                        [:label {:key 1 :text "a"}]
                        [:label {:key 2 :text "b"}]]]
    (is (= {"move" 1} (ops-of (patch old new)))
        "rotating three keyed children needs exactly one move")
    (is (= ["c" "a" "b"] (kid-texts old new)))))

(deftest keyed-children-keep-their-ids-across-reorder
  (let [old (mount [:vstack {} [:label {:key :a :text "a"}] [:label {:key :b :text "b"}]])
        new (:tree (d/reconcile old (w/normalize [:vstack {} [:label {:key :b :text "b"}]
                                                             [:label {:key :a :text "a"}]])
                                1000))
        id-of (fn [tree k] (->> (:kids tree) (filter #(= k (:key %))) first :id))]
    (is (= (id-of old :a) (id-of new :a)))
    (is (= (id-of old :b) (id-of new :b)))))

(deftest type-change-replaces-the-node
  (is (= {"remove" 1 "insert" 1}
         (ops-of (patch [:vstack {} [:label {:text "x"}]]
                        [:vstack {} [:button {:label "x"}]])))))

(deftest conditional-child-appears-and-disappears
  (testing "a (when ...) child is an insert one render and a remove the next"
    (is (= {"insert" 1} (ops-of (patch [:panel {} nil]
                                       [:panel {} [:label {:key :d :text "dialog"}]]))))
    (is (= {"remove" 1} (ops-of (patch [:panel {} [:label {:key :d :text "dialog"}]]
                                       [:panel {} nil]))))))

(deftest growing-a-list-touches-only-the-new-row
  (let [rows (fn [n] (into [:vstack {}]
                           (for [i (range n)] [:label {:key i :text (str "row " i)}])))]
    (is (= {"insert" 1} (ops-of (patch (rows 20) (rows 21))))
        "appending row 21 must not repaint the other twenty")))

;; -- hiccup normalization -------------------------------------------------------------

(deftest handlers-become-subscriptions
  (let [n (w/normalize [:button {:label "Save" :on-click (fn [s] s)}])]
    (is (= ["click"] (get-in n [:props "on"])))
    (is (fn? (get-in n [:handlers "click"])))
    (is (nil? (get-in n [:props "on-click"])) "handlers never go on the wire")))

(deftest clojure-names-become-wire-names
  (let [n (w/normalize [:panel {:border-color "$edge" :anchors {:left 10 :center-x 0}}])]
    (is (= {"borderColor" "$edge" "anchors" {"left" 10 "centerX" 0}} (:props n)))))

(deftest keyword-values-become-strings
  (is (= "cover" (get-in (w/normalize [:image {:src "/a.png" :fit :cover}]) [:props "fit"]))))

(deftest split-sugar-expands-to-the-protocol-type
  (let [n (w/normalize [:hsplit {:pos 100}])]
    (is (= "split" (:type n)))
    (is (= "h" (get-in n [:props "axis"])))))

(deftest dialogs-fill-their-parent-by-default
  (is (= {"left" 0 "right" 0 "top" 0 "bottom" 0}
         (get-in (w/normalize [:dialog {:title "hi"}]) [:props "anchors"]))
      "a modal scrim covers the viewport, like the Go SDK's Dialog()")
  (is (= {"left" 5} (get-in (w/normalize [:dialog {:title "hi" :anchors {:left 5}}])
                            [:props "anchors"]))
      "unless the author placed it deliberately"))

(deftest strings-are-label-sugar
  (is (= "label" (:type (w/normalize "hello"))))
  (is (= "hello" (get-in (w/normalize "hello") [:props "text"]))))

(deftest tables-subscribe-to-visible-range
  (is (= ["visible-range"]
         (get-in (w/normalize [:table {:row-count 10 :rows (fn [_ _ _] [])}])
                 [:props "on"]))))

;; -- menus -------------------------------------------------------------------------

(deftest menus-take-the-documented-wire-form
  (let [{:keys [menu handlers]}
        (w/menus->wire
         [{:title "Rows"
           :items [{:title "Add Row" :key "n" :on-pick (fn [s] s)}
                   {:sep true}
                   {:title "Clear Rows…" :key "shift+cmd+k" :on-pick (fn [s] s)}]}
          {:title "Theme"
           :items [{:title "Midnight" :on-pick (fn [s] s)}
                   {:title "Violet" :on-pick (fn [s] s)}]}])]
    (is (= [{"title" "Rows"
             "items" [{"id" 1 "title" "Add Row" "key" "n"}
                      {"sep" true}
                      {"id" 2 "title" "Clear Rows…" "key" "shift+cmd+k"}]}
            {"title" "Theme"
             "items" [{"id" 3 "title" "Midnight"}
                      {"id" 4 "title" "Violet"}]}]
           menu)
        "the shape README.md documents and the Go SDK emits: ids number
         pickable items in traversal order, separators consume no id")
    (is (= [1 2 3 4] (sort (keys handlers))))
    (is (= menu (json/read-str (json/write-str menu)))
        "pure wire data - handlers never ride along")))

(deftest submenu-ids-continue-depth-first
  (let [{:keys [menu handlers]}
        (w/menus->wire
         [{:title "File"
           :items [{:title "Export"
                    :items [{:title "PNG" :on-pick (fn [s] (assoc s :fmt "png"))}
                            {:title "SVG"}]}
                   {:title "Close" :key "w" :on-pick (fn [s] s)}]}])]
    (is (= [{"title" "File"
             "items" [{"title" "Export"
                       "items" [{"id" 1 "title" "PNG"}
                                {"id" 2 "title" "SVG"}]}
                      {"id" 3 "title" "Close" "key" "w"}]}]
           menu)
        "a submenu carries no id of its own; numbering runs depth-first through it")
    (is (= {:fmt "png"} ((get handlers 1) {}))
        "the id->handler map is how a pick finds its way back to app code")
    (is (not (contains? handlers 2))
        "an item without :on-pick keeps its id but registers no handler")))

(deftest menu-picks-dispatch-like-any-other-handler
  (let [sess (session/new-session {:view (fn [_ _] [:panel {}]) :init {:picked []}})]
    (session/set-menu! sess [{:title "Rows"
                              :items [{:title "Add" :key "n"
                                       :on-pick (fn [s] (update s :picked conj "add"))}
                                      {:sep true}
                                      {:title "Clear"
                                       :on-pick (fn [s] (update s :picked conj "clear"))}]}])
    (is (= [{"title" "Rows"
             "items" [{"id" 1 "title" "Add" "key" "n"}
                      {"sep" true}
                      {"id" 2 "title" "Clear"}]}]
           (:menu @sess))
        "the wire form lives on the session, so it rides mounts and survives resume")
    ;; A pick is a session-level event - node id 0, like resize. The unknown
    ;; id must be ignored. Events are FIFO on the session thread, so once the
    ;; second one arrives we know the first was a no-op.
    (session/receive! sess (json/write-str {"t" "ev" "id" 0 "ev" "menu" "value" 99 "seq" 1}))
    (session/receive! sess (json/write-str {"t" "ev" "id" 0 "ev" "menu" "value" 2 "seq" 1}))
    (let [deadline (+ (System/currentTimeMillis) 3000)]
      (while (and (empty? (:picked (session/state sess)))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 10)))
    (is (= ["clear"] (:picked (session/state sess)))
        "value 2 is Clear - the separator consumed no id - and pick 99 did nothing")))
