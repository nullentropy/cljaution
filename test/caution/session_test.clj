(ns caution.session-test
  (:require [clojure.test :refer [deftest is testing]]
            [caution.core :as c]
            [caution.diff :as d]
            [caution.server :as server]
            [caution.session]
            [caution.wire :as w]))

(def ^:private dispatch! #'caution.session/dispatch!)

(defn- render-step
  [old-tree next-id hiccup]
  (let [{:keys [tree next-id]} (d/reconcile old-tree (w/normalize hiccup) next-id)]
    {:tree tree :next-id next-id :index (w/index-tree tree)}))

(defn- session-shell
  [{:keys [tree index]} !state]
  (atom {:sid 0 :tree tree :index index :!state !state
         :ranges {} :rows-cache {} :menu-handlers {}}))

(deftest click-racing-a-remove-is-dropped
  (let [!state (atom {:clicks 0})
        v1 [:panel {}
            [:button {:label "doomed" :on-click (fn [s] (update s :clicks inc))}]]
        r1 (render-step nil 1 v1)
        btn-id (:id (first (:kids (:tree r1))))
        sess (session-shell r1 !state)]

    (testing "a click on a live node runs its handler"
      (dispatch! sess {"id" btn-id "ev" "click" "seq" 1})
      (is (= 1 (:clicks @!state))))

    (testing "a click racing the remove of its target dissolves silently"
      (let [r2 (render-step (:tree r1) (:next-id r1) [:panel {}])]
        (is (some #(= "remove" (get % "op"))
                  (:ops (d/reconcile (:tree r1) (w/normalize [:panel {}]) (:next-id r1))))
            "the re-render actually removed the node")
        (swap! sess assoc :tree (:tree r2) :index (:index r2))
        (dispatch! sess {"id" btn-id "ev" "click" "seq" 1})
        (is (= 1 (:clicks @!state)) "the stale click must not fire")))

    (testing "ids are never recycled: a new sibling gets a fresh id"
      (let [r2 (render-step (:tree r1) (:next-id r1) [:panel {}])
            r3 (render-step (:tree r2) (:next-id r2)
                            [:panel {}
                             [:button {:label "new" :on-click (fn [s] (update s :clicks + 100))}]])
            new-id (:id (first (:kids (:tree r3))))]
        (is (not= btn-id new-id))))))

(deftest resume-ack-when-nothing-changed
  (let [sent (atom [])
        base {:sid 0 :tree {:id 1 :type "panel" :props {} :kids []}
              :seq 7 :index {} :ranges {} :rows-cache {} :!state (atom {})}
        attach! #'caution.session/open!]
    (with-redefs [caution.session/send-json! (fn [_ msg] (swap! sent conj msg))
                  caution.session/post! (fn [_ _ f] (f))]
      (testing "matching seq -> ack"
        (attach! (atom base) :ch 7)
        (is (= [{"t" "resume" "seq" 7}] @sent)))
      (testing "stale seq -> remount"
        (reset! sent [])
        (attach! (atom base) :ch 5)
        (is (= "mount" (get (first @sent) "t"))))
      (testing "fresh connection (no seq) with a tree -> remount"
        (reset! sent [])
        (attach! (atom base) :ch nil)
        (is (= "mount" (get (first @sent) "t")))))))

(deftest preload-follows-the-mount-contract
  (let [sent (atom [])
        base {:sid 0 :tree {:id 1 :type "panel" :props {} :kids []}
              :seq 0 :index {} :ranges {} :rows-cache {} :!state (atom {})}
        attach! #'caution.session/open!]
    (with-redefs [caution.session/send-json! (fn [_ msg] (swap! sent conj msg))
                  caution.session/post! (fn [_ _ f] (f))]
      (let [sess (atom base)]
        (caution.session/preload! sess "/a.png" "/b.png")
        (caution.session/preload! sess "/a.png") ; all-duplicate: no op
        (is (= 1 (count @sent)))
        (is (= {"op" "resource" "images" ["/a.png" "/b.png"]}
               (first (get (first @sent) "ops"))))
        (is (= ["/a.png" "/b.png"] (:preloads @sess)))
        (testing "the stored set rides the next mount"
          (reset! sent [])
          (attach! sess :ch nil)
          (is (= {"images" ["/a.png" "/b.png"]}
                 (get (first @sent) "resources"))))))))

(deftest metrics-follow-the-mount-contract
  (let [sent (atom [])
        base {:sid 0 :tree {:id 1 :type "panel" :props {} :kids []}
              :seq 0 :index {} :ranges {} :rows-cache {} :!state (atom {})}
        attach! #'caution.session/open!]
    (with-redefs [caution.session/send-json! (fn [_ msg] (swap! sent conj msg))
                  caution.session/post! (fn [_ _ f] (f))]
      (let [sess (atom base)]
        (caution.session/set-metrics! sess {"radius.control" 0 "border.width" 2})
        (is (= 1 (count @sent)))
        (is (= {"op" "metrics" "metrics" {"radius.control" 0 "border.width" 2}}
               (first (get (first @sent) "ops"))))
        (testing "the stored map rides the next mount"
          (reset! sent [])
          (attach! sess :ch nil)
          (is (= {"radius.control" 0 "border.width" 2}
                 (get (first @sent) "metrics"))))))))

(deftest tree-rows-flatten-and-ship-meta
  (let [items [{:key "a" :cells ["a"]
                :kids [{:key "a1" :cells ["a1"]}
                       {:key "a2" :cells ["a2"] :kids [{:key "a2x" :cells ["a2x"]}]}]}
               {:key "b" :cells ["b"]}]]
    (is (= 2 (c/tree-row-count items #{})))
    (is (= 4 (c/tree-row-count items #{"a"})))
    (is (= [{:cells ["a"] :meta {:key "a" :d 0 :k true :x true}}
            {:cells ["a1"] :meta {:key "a1" :d 1}}
            {:cells ["a2"] :meta {:key "a2" :d 1 :k true}}
            {:cells ["b"] :meta {:key "b" :d 0}}]
           (c/tree-rows items #{"a"} 0 9)))
    ;; the session splits {:cells :meta} rows into the wire's rows + meta
    (let [table-rows #'caution.session/table-rows
          node {:rows (fn [_ start end] (c/tree-rows items #{"a"} start end))}
          w (table-rows node {} 0 9)]
      (is (= [["a"] ["a1"] ["a2"] ["b"]] (:rows w)))
      (is (= {"key" "a" "d" 0 "k" true "x" true} (first (:meta w))))
      (is (= {"key" "a1" "d" 1} (second (:meta w)))))
    (is (= #{"a"} (c/toggle-member #{} "a")))
    (is (= #{} (c/toggle-member #{"a"} "a")))))

(deftest command-helpers-queue-in-state
  (let [s (-> {} (c/focus :email) (c/clear-value :note) (c/focus :email))]
    (is (= [{:k :email :cmd "focus"} {:k :note :cmd "clear"} {:k :email :cmd "focus"}]
           (:caution.core/cmds s)))))

(deftest commands-resolve-keys-and-names-to-cmd-ops
  (let [cmd-ops #'caution.session/cmd-ops
        view (fn [s] [:panel {}
                      [:textfield {:key :f :value (:v s)}]
                      [:textfield {:name "note" :value "n"}]])
        r1 (d/reconcile nil (w/normalize (view {:v "a"})) 1)
        s2 (-> {:v "b"} (c/override-value :f) (c/clear-value "note") (c/focus :missing))
        r2 (d/reconcile (:tree r1) (w/normalize (view s2)) (:next-id r1))
        ops (vec (cmd-ops (:tree r2) (w/index-tree (:tree r2)) (:caution.core/cmds s2)))]
    (is (= [{"op" "cmd" "id" 2 "cmd" "value" "value" "b"}
            {"op" "cmd" "id" 3 "cmd" "clear"}]
           ops)
        "the value command carries the rendered value; unknown keys drop")))

(deftest keyed-row-select-echoes-the-key
  (let [!state (atom {:picked nil})
        v [:panel {}
           [:table {:columns [{:key "id" :title "ID"}] :row-count 10 :row-key 0
                    :on-row-select (fn [s v] (assoc s :picked v))}]]
        r (render-step nil 1 v)
        tbl-id (:id (first (:kids (:tree r))))
        sess (session-shell r !state)]
    (dispatch! sess {"id" tbl-id "ev" "row-select" "seq" 1
                     "value" {"row" 3 "key" "E00004"}})
    (is (= {"row" 3 "key" "E00004"} (:picked @!state)))
    (is (= "E00004" (get-in @sess [:index tbl-id :props "selectedKey"])))))

(deftest wasm-negotiates-content-encoding
  (let [magic (fn [resp n]
                (let [b (byte-array n)]
                  (.read ^java.io.InputStream (:body resp) b)
                  (mapv #(bit-and % 0xff) b)))
        gz (server/wasm-handler {:headers {"accept-encoding" "gzip, br"}})
        plain (server/wasm-handler {:headers {}})]
    (is (= 200 (:status gz)))
    (is (= "gzip" (get-in gz [:headers "Content-Encoding"])))
    (is (= "application/wasm" (get-in gz [:headers "Content-Type"])))
    (is (= [0x1f 0x8b] (magic gz 2)) "gzip magic bytes")
    (is (= 200 (:status plain)))
    (is (nil? (get-in plain [:headers "Content-Encoding"])))
    (is (= [0x00 0x61 0x73 0x6d] (magic plain 4)) "wasm magic bytes")))

(deftest cookies-ride-the-mount-and-wait-out-a-disconnect
  (let [sent (atom [])
        base {:sid 0 :tree {:id 1 :type "panel" :props {} :kids []}
              :seq 3 :index {} :ranges {} :rows-cache {} :!state (atom {})}
        attach! #'caution.session/open!]
    (with-redefs [caution.session/send-json! (fn [s msg]
                                               (when (:channel @s)
                                                 (swap! sent conj msg)
                                                 true))
                  caution.session/post! (fn [_ _ f] (f))]
      (testing "live: one cookie op per cookie"
        (let [sess (atom (assoc base :channel :ch))]
          (c/set-cookie! sess {:name "login" :value "abc" :max-age 60 :same-site :lax})
          (is (= [{"t" "patch" "seq" 4
                   "ops" [{"op" "cookie"
                           "cookie" "login=abc; Path=/; Max-Age=60; SameSite=Lax"}]}]
                 @sent))
          (is (empty? (:cookies @sess)))))

      (testing "disconnected: held, seq untouched, sent after the resume ack"
        (reset! sent [])
        (let [sess (atom base)]
          (c/set-cookie! sess {:name "login" :value "" :max-age -1})
          (is (= 3 (:seq @sess)) "a resume ack must still match")
          (attach! sess :ch 3)
          (is (= [{"t" "resume" "seq" 3}
                  {"t" "patch" "seq" 4
                   "ops" [{"op" "cookie" "cookie" "login=; Path=/; Max-Age=0"}]}]
                 @sent))))

      (testing "a remount carries pending cookies"
        (reset! sent [])
        (let [sess (atom (assoc base :cookies ["a=1; Path=/"]))]
          (attach! sess :ch 1)
          (is (= ["a=1; Path=/"] (get (first @sent) "cookies")))
          (is (empty? (:cookies @sess)))))

      (testing "invalid names and values are dropped"
        (let [sess (atom base)]
          (c/set-cookie! sess {:name "bad name" :value "x"})
          (c/set-cookie! sess {:name "ok" :value "has;semicolon"})
          (is (empty? (:cookies @sess))))))))

(deftest motion-rides-the-mount
  (let [sent (atom [])]
    (with-redefs [caution.session/send-json! (fn [_ msg] (swap! sent conj msg) true)
                  caution.session/post! (fn [_ _ f] (f))]
      (let [sess (atom {:sid 0 :tree {:id 1 :type "panel" :props {} :kids []}
                        :seq 0 :index {} :ranges {} :rows-cache {} :!state (atom {})})]
        (c/set-motion! sess :reduced)
        (is (= {"op" "motion" "motion" "reduced"} (first (get (first @sent) "ops"))))
        (#'caution.session/open! sess :ch nil)
        (is (= "reduced" (get (last @sent) "motion")))))
    (is (true? (get-in (c/normalize [:scroll {:no-fade true}]) [:props "noFade"])))))

(deftest close-session-ends-it-for-good
  (let [closed (atom nil)
        sess (caution.session/new-session {:view (fn [_ _] [:panel {}])})]
    (swap! sess assoc :channel :ch)
    (with-redefs [org.httpkit.server/close (fn [ch] (reset! closed ch))]
      (c/close-session! sess))
    (is (= :ch @closed) "the socket is hung up")
    (is (not (c/alive? sess)))
    (is (nil? (caution.session/lookup (:token @sess))) "a reconnect can't resume it")))

(deftest cookie-reads-the-request-header
  (let [req {:headers {"cookie" "a=1; login=abc=def; b=2"}}]
    (is (= "abc=def" (c/cookie req "login")))
    (is (nil? (c/cookie req "missing")))
    (is (nil? (c/cookie {:headers {}} "login")))))

(deftest submit-repeats-and-focus-reaches-the-handler
  (let [!state (atom {:sent [] :focus []})
        r (render-step nil 1 [:panel {}
                              [:textfield {:on-submit (fn [s v] (update s :sent conj v))}]])
        id (:id (first (:kids (:tree r))))
        sess (session-shell r !state)]
    (swap! sess assoc :focus-handler (fn [s f] (update s :focus conj f)))
    (is (= ["submit"] (get-in (:index r) [id :props "on"])))
    (dispatch! sess {"id" id "ev" "submit" "value" "hi"})
    (dispatch! sess {"id" id "ev" "submit" "value" "hi"})
    (dispatch! sess {"id" 0 "ev" "focus" "value" false})
    (is (= ["hi" "hi"] (:sent @!state)) "every enter sends, changed or not")
    (is (= [false] (:focus @!state)))
    (is (not (c/focused? sess)))))
