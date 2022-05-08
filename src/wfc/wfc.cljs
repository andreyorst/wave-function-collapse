(ns wfc.wfc
  (:require [clojure.set :as set]))

(defn get-superposition [sample]
  (into #{} (flatten sample)))

(defn clamp [lo x hi]
  (cond (< x lo) lo
        (> x hi) hi
        :else x))

(defn get-neighbors [m [x y]]
  (->> {:up [(dec x) y]
        :down [(inc x) y]
        :left [x (dec y)]
        :right [x (inc y)]}
       (into {}
             (keep (fn [[dir pos]]
                     (when-some [cell (get-in m pos)]
                       [dir {:pos pos :cell cell}]))))))

(defn build-recipe [sample]
  (->> sample
       (map-indexed
        (fn [x r]
          (map-indexed
           (fn [y c]
             (let [neigbors (into {}
                                  (map (fn [[dir {:keys [cell]}]] [dir #{cell}]))
                                  (get-neighbors sample [x y]))]
               {c neigbors})) r)))
       flatten
       (apply merge-with #(merge-with set/union %1 %2))))

(defn sample-weights [sample]
  (frequencies (apply concat sample)))

(defn cell-enthropy [cell weights]
  (let [weight-sum (reduce + (map #(get weights %) cell))
        log-weight-sum (reduce + (map #(let [weight (get weights %)]
                                         (* weight (Math/log (double weight)))) cell))]
    (Math/log (- weight-sum (/ log-weight-sum weight-sum)))))

(defn done? [world]
  (not (some set? (flatten world))))

(defn lowest-entropy-cell [world weights]
  (let [enthropies (->> world
                        (map-indexed
                         (fn [x r]
                           (keep-indexed
                            (fn [y cell]
                              (when (set? cell)
                                [(cell-enthropy cell weights) [x y]])) r)))
                        (reduce into (sorted-map)))]
    (if-some [[_ c] (first enthropies)]
      c
      [0 0])))

(defn weighted-random [elements weights]
  (let [variants (reduce (fn [m k] (assoc m k (weights k))) {} elements)
        weights (reductions #(+ % %2) (vals variants))
        rnd (rand-int (last weights))]
    (nth (keys variants) (count (take-while #(<= % rnd) weights)))))

(defn collapse
  [world [x y] val]
  (if (and (coll? val)
           (= 1 (count val)))
    (assoc-in world [x y] (first val))
    (assoc-in world [x y] val)))

(defn collapse-neighbors*
  [world recipe pos]
  (let [cell (get-in world pos)
        neighbors (get-neighbors world pos)
        neighboring-cells (map :pos (vals neighbors))
        world' (reduce
                (fn [world [dir {pos :pos}]]
                  (let [c (get-in world pos)
                        variants (set/intersection
                                  (if (coll? c) (set c) #{c})
                                  (if (set? cell)
                                    (into #{} (mapcat #(get-in recipe [% dir])) cell)
                                    (get-in recipe [cell dir])))]
                    (if (and (set? c) (seq variants))
                      (collapse world pos variants)
                      world)))
                world neighbors)]
    [world'
     (when-not (= world' world) neighboring-cells)]))

(defn collapse-neighbors [world recipe pos]
  (loop [neighbors [pos]
         world world]
    (if (seq neighbors)
      (let [pos (first neighbors)
            [world next] (collapse-neighbors* world recipe pos)]
        (recur (concat (rest neighbors) next)
               world))
      world)))

(defn gen-world [max-x max-y]
  (mapv vec (for [_ (range max-x)]
              (for [_ (range max-y)] nil))))

(defn populate-world [world sample]
  (let [superpos (get-superposition sample)]
    (mapv (fn [r] (mapv (fn [c] (if (nil? c) superpos c)) r)) world)))

(defn wfc [world sample]
  (let [recipe (build-recipe sample)
        weights (sample-weights sample)]
    (loop [world (populate-world world sample)
           pos [(rand-int (count (first world)))
                (rand-int (count world))]]
      (if-not (done? world)
        (let [cell (get-in world pos)
              world (-> world
                        (collapse pos (weighted-random cell weights))
                        (collapse-neighbors recipe pos))]
          (recur world
                 (lowest-entropy-cell world weights)))
        world))))
