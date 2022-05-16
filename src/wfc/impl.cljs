(ns wfc.impl
  "Implementation of the wave function collapse alrorithm."
  (:require
   [clojure.set :as set]
   [wfc.config :as config]
   [wfc.canvas-utils :as cu]))

(defn- get-superposition [sample]
  (into #{} (flatten sample)))

(defn- get-neighbors [m [x y]]
  (->> {:up [(dec x) y]
        :down [(inc x) y]
        :left [x (dec y)]
        :right [x (inc y)]}
       (into {}
             (keep (fn [[dir pos]]
                     (when-some [cell (get-in m pos)]
                       [dir {:pos pos :cell cell}]))))))

(defn- build-recipe [sample]
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

(defn- sample-weights [sample]
  (frequencies (apply concat sample)))

(defn- cell-entropy
  "Shannon entropy."
  [cell weights]
  (loop [variants cell
         weight-sum 0
         log-weight-sum 0]
    (if-let [[variant & variants] variants]
      (let [weight (get weights variant)]
        (recur variants
               (+ weight-sum weight)
               (+ log-weight-sum (* weight (Math/log (double weight))))))
      (Math/log (- weight-sum (/ log-weight-sum weight-sum))))))

(defn- done? [world]
  (not (some set? (flatten world))))

(defn- lowest-entropy-cell
  "Search for the cell with the lowest entropy.
  Each row is processed and cells are put into the sorted map, sorting
  by the entropy. Then the first value from each row is taken and
  sorted by the entropy again. The lowest entropy cell is then
  selected."
  [world weights]
  (loop [x 0
         [row & rows] world
         entropies (sorted-map)]
    (if (seq row)
      (let [row-entropy
            (loop [y 0
                   [column & columns] row
                   entropies (sorted-map)]
              (if column
                (recur (inc y)
                       columns
                       (if (set? column)
                         (assoc entropies (cell-entropy column weights) [x y])
                         entropies))
                (first entropies)))]
        (recur (inc x)
               rows
               (if row-entropy
                 (conj entropies row-entropy)
                 entropies)))
      (if-let [[_ c] (first entropies)]
        c
        [0 0]))))

(defn- weighted-random [elements weights]
  (let [variants (reduce (fn [m k] (assoc m k (weights k))) {} elements)
        weights (reductions #(+ % %2) (vals variants))
        rnd (rand-int (last weights))]
    (nth (keys variants) (count (take-while #(<= % rnd) weights)))))

(defn- collapse
  [world [x y] val]
  (if (and (coll? val)
           (= 1 (count val)))
    (assoc-in world [x y] (first val))
    (assoc-in world [x y] val)))

(defn- collapse-neighbors*
  "Part of the recursive collapsing algorithm.
  Collapses four nearest neighbors and returns the world state
  alongside with these neighbors, so that the caller could process
  these points with this function in a trampoline-like way."
  [world recipe pos]
  (let [cell (get-in world pos)
        neighbors (get-neighbors world pos)
        neighboring-cells (map :pos (vals neighbors))]
    (loop [neighbors neighbors
           world world]
      (if-let [[neighbor & neighbors] neighbors]
        (let [[dir {pos :pos}] neighbor
              c (get-in world pos)]
          (if (set? c)
            (let [variants (set/intersection
                            (if (coll? c) (set c) #{c})
                            (if (set? cell)
                              (into #{} (mapcat #(get-in recipe [% dir])) cell)
                              (get-in recipe [cell dir])))]
              (if (seq variants)
                (recur neighbors (collapse world pos variants))
                (recur neighbors world)))
            (recur neighbors world)))
        [world neighboring-cells]))))

(defn- collapse-neighbors
  "Recursive collapsing neighbors using trampoline-like technique.
  Instead of doing work recursively calls `collapse-neighbors*` and
  collects a stack of values to be processed next, after the called
  function finishes for current position."
  [world recipe pos]
  (loop [neighbors [pos]
         visited #{}
         world world]
    (if (seq neighbors)
      (let [[pos & neighbors] neighbors
            [world' next] (if-not (visited pos)
                            (collapse-neighbors* world recipe pos)
                            [world []])]
        (recur (if (= world world')
                 neighbors
                 (concat neighbors next))
               (conj visited pos)
               world'))
      world)))

(defn- populate-world [world sample]
  (let [superpos (get-superposition sample)]
    (mapv (fn [r] (mapv (fn [c] (if (nil? c) superpos c)) r)) world)))

(defn- pre-filled? [world]
  (some some? (flatten world)))

(defn- init-world
  "Initializes the world with a given sample, and afterwards collapses
  cells if world was pre-filled with information."
  [world sample recipe]
  (let [filled? (pre-filled? world)
        width (count (first world))
        height (count world)
        world (populate-world world sample)]
    (if filled?
      (reduce (fn [world pos]
                (collapse-neighbors world recipe pos))
              world
              (for [x (range width)
                    y (range height)]
                [x y]))
      world)))

(defn gen-world
  "Creates a new empty world of given size."
  [max-x max-y]
  (mapv vec (for [_ (range max-x)]
              (for [_ (range max-y)] nil))))

(defn wfc
  "Main algorithm loop.
  The loop itself is done in a callback style by calling
  `.requestAnimationFrame` method of a `window`.  The world state is
  captured in a mutable closure."
  [world sample callback animate?]
  (let [recipe (build-recipe sample)
        weights (sample-weights sample)
        *world (volatile! (init-world world sample recipe))
        *pos (volatile! (if (pre-filled? world)
                          (lowest-entropy-cell @*world weights)
                          [(rand-int (count (first world)))
                           (rand-int (count world))]))
        compute
        (fn compute []
          (let [world @*world
                pos @*pos]
            (if-not (done? world)
              (let [cell (get-in world pos)
                    world (-> world
                              (collapse pos (weighted-random cell weights))
                              (collapse-neighbors recipe pos))]
                (when animate?
                  (callback world))
                (vreset! *world world)
                (vreset! *pos (lowest-entropy-cell world weights))
                (.requestAnimationFrame js/window compute))
              (do (callback world)
                  (swap! config/*state assoc
                         :world-state world
                         :rendered-image (cu/get-image (.getContext (:render-view @config/*dom-elements) "2d"))
                         :move? true)))))]
    (compute)))
