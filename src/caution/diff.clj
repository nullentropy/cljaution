(ns caution.diff
  (:require [caution.wire :as w]))

(defn- assign-ids
  [node ctx]
  (let [id  (:next-id ctx)
        ctx (assoc ctx :next-id (inc id))
        [kids ctx] (reduce (fn [[acc c] k]
                             (let [[k' c'] (assign-ids k c)]
                               [(conj acc k') c']))
                           [[] ctx]
                           (:kids node))]
    [(assoc node :id id :kids kids) ctx]))

(defn- diff-props
  [old new]
  (persistent!
   (reduce (fn [m k]
             (if (contains? new k) m (assoc! m k nil)))
           (reduce-kv (fn [m k v] (if (= v (get old k)) m (assoc! m k v)))
                      (transient {})
                      new)
           (keys old))))

(defn- match-key
  [node idx]
  [(:type node) (if-some [k (:key node)] [:key k] [:idx idx])])

(declare reconcile-node)

(defn- claim-old
  [olds news]
  (let [by-key (into {} (map-indexed (fn [i o] [(match-key o i) o])) olds)]
    (first
     (reduce (fn [[claims taken] [i n]]
               (let [o (by-key (match-key n i))]
                 (if (and o (not (taken (:id o))))
                   [(assoc claims i o) (conj taken (:id o))]
                   [claims taken])))
             [{} #{}]
             (map-indexed vector news)))))

(defn- splice-out [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn- splice-in [v i x]
  (let [i (min i (count v))]
    (into (conj (subvec v 0 i) x) (subvec v i))))

(defn- reconcile-kids
  [parent-id olds news ctx]
  (let [claims  (claim-old olds news)
        claimed (into #{} (map :id) (vals claims))
        ctx     (reduce (fn [c o]
                          (update c :ops conj {"op" "remove" "id" (:id o)}))
                        ctx
                        (remove #(claimed (:id %)) olds))]
    (loop [i 0, cur (filterv #(claimed (:id %)) olds), out [], ctx ctx]
      (if (= i (count news))
        [out ctx]
        (let [n (nth news i)]
          (if-let [o (claims i)]
            (let [[merged ctx] (reconcile-node o n ctx)
                  j (->> cur (keep-indexed #(when (= (:id %2) (:id o)) %1)) first)
                  [cur ctx] (if (= j i)
                              [cur ctx]
                              [(splice-in (splice-out cur j) i o)
                               (update ctx :ops conj {"op" "move" "id" (:id o)
                                                      "parent" parent-id "index" i})])]
              (recur (inc i) cur (conj out merged) ctx))
            (let [[created ctx] (assign-ids n ctx)
                  ctx (update ctx :ops conj {"op" "insert" "parent" parent-id
                                             "index" i "node" (w/->wire created)})]
              (recur (inc i) (splice-in cur i created) (conj out created) ctx))))))))

(defn- reconcile-node
  [old new ctx]
  (let [changed (diff-props (:props old) (:props new))
        ctx (cond-> ctx
              (seq changed) (update :ops conj {"op" "set" "id" (:id old) "p" changed}))
        [kids ctx] (reconcile-kids (:id old) (:kids old) (:kids new) ctx)]
    [(assoc new :id (:id old) :kids kids) ctx]))

(defn reconcile
  "old tree (with ids, or nil) + new normalized tree -> {:tree :ops :next-id}.
  A root whose type or key changed can't be patched in place, so the caller
  gets :remount? and sends a fresh mount instead."
  [old new next-id]
  (if (or (nil? old)
          (not= (:type old) (:type new))
          (not= (:key old) (:key new)))
    (let [[tree ctx] (assign-ids new {:next-id next-id :ops []})]
      {:tree tree :ops [] :next-id (:next-id ctx) :remount? true})
    (let [[tree ctx] (reconcile-node old new {:next-id next-id :ops []})]
      {:tree tree :ops (:ops ctx) :next-id (:next-id ctx) :remount? false})))
