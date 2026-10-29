(ns caution.wire
  (:require [clojure.string :as str]))

(def ^:private event-props
  {:on-click        "click"
   :on-toggle       "toggle"
   :on-input        "input"
   :on-commit       "commit"
   :on-select       "select"
   :on-dismiss      "dismiss"
   :on-sort         "sort"
   :on-row-select   "row-select"
   :on-row-activate "row-activate"
   :on-cell-activate "cell-activate"
   :on-split-resize "split-resize"
   :on-context      "context"})

(defn kebab->camel
  [s]
  (let [[head & tail] (str/split s #"-")]
    (apply str head (map str/capitalize tail))))

(defn- prop-name [k]
  (kebab->camel (name k)))

(defn- prop-value
  [v]
  (cond
    (keyword? v) (name v)
    (map? v) (reduce-kv (fn [m k x] (assoc m (prop-name k) (prop-value x))) {} v)
    (sequential? v) (mapv prop-value v)
    :else v))

(defn- split-props
  [attrs]
  (reduce-kv
   (fn [[props handlers rows k] pk pv]
     (cond
       (= :key pk)   [props handlers rows pv]
       (= :rows pk)  [props handlers pv k]
       (event-props pk) [props (assoc handlers (event-props pk) pv) rows k]
       (nil? pv)     [props handlers rows k]
       :else         [(assoc props (prop-name pk) (prop-value pv)) handlers rows k]))
   [{} {} nil nil]
   attrs))

(def ^:private tag-aliases
  "Sugar for widget types that the protocol distinguishes by a prop."
  {:hsplit {:type "split" :props {"axis" "h"}}
   :vsplit {:type "split" :props {"axis" "v"}}})

(def ^:private fill-anchors
  {"left" 0 "right" 0 "top" 0 "bottom" 0})

(defn- type-defaults
  [type props]
  (if (and (= type "dialog")
           (not (contains? props "anchors"))
           (not (contains? props "frame")))
    (assoc props "anchors" fill-anchors)
    props))

(declare normalize)

(defn- normalize-kids [kids]
  (into []
        (comp (mapcat #(if (and (sequential? %) (not (vector? %))) % [%]))
              (remove nil?)
              (map normalize))
        kids))

(defn normalize
  "Hiccup vector -> internal node. Strings are sugar for a label's text."
  [form]
  (cond
    (string? form)
    (normalize [:label {:text form}])

    (and (vector? form) (keyword? (first form)))
    (let [[tag & more] form
          attrs (if (map? (first more)) (first more) {})
          kids  (if (map? (first more)) (rest more) more)
          [props handlers rows k] (split-props attrs)
          alias (tag-aliases tag)
          type  (or (:type alias) (name tag))
          props (type-defaults type (merge (:props alias) props))
          kids  (normalize-kids kids)
          subs  (vec (sort (keys handlers)))
          subs  (if (and rows (not (some #{"visible-range"} subs)))
                  (conj subs "visible-range")
                  subs)]
      (cond-> {:type type
               :props props
               :handlers handlers
               :kids kids}
        k    (assoc :key k)
        rows (assoc :rows rows)
        (seq subs) (assoc-in [:props "on"] subs)))

    :else
    (throw (ex-info "caution: not a widget form" {:form form}))))

(defn ->wire
  "Internal node (with :id) -> the JSON shape the client inflates"
  [node]
  (cond-> {"id" (:id node) "type" (:type node)}
    (seq (:props node)) (assoc "p" (:props node))
    (seq (:kids node))  (assoc "kids" (mapv ->wire (:kids node)))))

(defn index-tree
  "Flat id -> node map, for event dispatch and rows lookups"
  [node]
  (into {(:id node) node} (map index-tree) (:kids node)))

;; -- menus ---------------------------------------------------------------------

(defn- menu-items->wire
  "Menu items -> [wire-items next-id handlers], numbering pickable items
  depth-first at their position"
  [items next-id handlers]
  (reduce
   (fn [[out next-id handlers] {:keys [title key sep items on-pick]}]
     (cond
       sep
       [(conj out {"sep" true}) next-id handlers]

       (seq items)
       (let [[sub next-id handlers] (menu-items->wire items next-id handlers)]
         [(conj out {"title" title "items" sub}) next-id handlers])

       :else
       (let [id (inc next-id)]
         [(conj out (cond-> {"id" id "title" title}
                      key (assoc "key" key)))
          id
          (cond-> handlers on-pick (assoc id on-pick))])))
   [[] next-id handlers]
   items))

(defn menus->wire
  "Menu-bar data -> {:menu wire :handlers {id (fn [state])}}.

  A menu is {:title _ :items [...]}; an item is a pickable
  {:title _ :key _ :on-pick f}, a separator {:sep true}, or a submenu
  {:title _ :items [...]}. Pickable items are numbered 1.. in traversal
  order. That id is what the client sends back as a session-level menu
  event and :on-pick handlers are lifted out of the wire form into the
  id-keyed map, like :on-* props are lifted out of hiccup."
  [menus]
  (let [[wire _ handlers]
        (reduce (fn [[out next-id handlers] {:keys [title items]}]
                  (let [[sub next-id handlers] (menu-items->wire items next-id handlers)]
                    [(conj out {"title" title "items" sub}) next-id handlers]))
                [[] 0 {}]
                menus)]
    {:menu wire :handlers handlers}))
