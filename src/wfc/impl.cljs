(ns wfc.impl
  "Implementation of the wave function collapse algorithm."
  (:require
   [clojure.set :as set]
   [wfc.config :as config]
   [wfc.canvas-utils :as cu]))

(defn- get-superposition [sample]
  (into #{} (flatten sample)))

(defn- get-cell [world [x y]]
  (nth (nth world y) x))

(defn- get-neighbors [m [x y]]
  (->> {:up [(dec x) y]
        :down [(inc x) y]
        :left [x (dec y)]
        :right [x (inc y)]}
       (into {}
             (keep (fn [[dir [x y :as pos]]]
                     (when-some [cell (get-in m [y x])]
                       [dir {:pos pos :cell cell}]))))))

(defn- build-recipe [sample]
  (->> sample
       (map-indexed
        (fn [y r]
          (map-indexed
           (fn [x c]
             (let [neigbors (into {}
                                  (map (fn [[dir {:keys [cell]}]] [dir #{cell}]))
                                  (get-neighbors sample [x y]))]
               {c neigbors})) r)))
       flatten
       (apply merge-with #(merge-with set/union %1 %2))))

(defn sample-weights [sample]
  (frequencies (apply concat sample)))

(defn cell-entropy
  "Shannon entropy."
  [cell weights]
  (loop [variants (if (coll? cell) cell [cell])
         weight-sum 0
         log-weight-sum 0]
    (if-let [[variant & variants] variants]
      (let [weight (get weights variant)]
        (recur variants
               (+ weight-sum weight)
               (+ log-weight-sum (* weight (Math/log (double weight))))))
      (- (Math/log weight-sum) (/ log-weight-sum weight-sum)))))

(defn- done? [world]
  (not (some set? (flatten world))))

(defn- lowest-entropy-cell
  "Search for the cell with the lowest entropy.
  Each row is processed and cells are put into the sorted map, sorting
  by the entropy. Then the first value from each row is taken and
  sorted by the entropy again. The lowest entropy cell is then
  selected."
  ([world weights]
   (let [width (count (first world))
         height (count world)]
     (loop [x 0
            y 0
            pos [x y]
            lowest js/Number.MAX_SAFE_INTEGER]
       (if (< y height)
         (if (< x width)
           (let [pos' [x y]
                 cell (get-cell world pos')
                 ent (cell-entropy cell weights)]
             (if (and (< ent lowest) (set? cell))
               (recur (inc x) y pos' ent)
               (recur (inc x) y pos lowest)))
           (recur 0 (inc y) pos lowest))
         pos))))
  ([cells world weights]
   (loop [pos (first cells)
          cells (next cells)
          lowest (cell-entropy (get-cell world pos) weights)]
     (if cells
       (let [pos' (first cells)
             ent (cell-entropy (get-cell world pos') weights)
             cells  (next cells)]
         (if (< ent lowest)
           (recur pos' cells ent)
           (recur pos cells lowest)))
       pos))))

(defn- weighted-random [elements weights]
  (let [variants (reduce (fn [m k] (assoc m k (weights k))) {} elements)
        weights (reductions #(+ % %2) (vals variants))
        rnd (rand-int (last weights))]
    (nth (keys variants) (count (take-while #(<= % rnd) weights)))))

(defn- collapse
  [world [x y] val]
  (if (and (coll? val)
           (= 1 (count val)))
    (assoc-in world [y x] (first val))
    (assoc-in world [y x] val)))

(defn- collapse-neighbors*
  "Part of the recursive collapsing algorithm.
  Collapses four nearest neighbors and returns the world state
  alongside with these neighbors, so that the caller could process
  these points with this function in a trampoline-like way."
  [world recipe pos]
  (let [cell (get-cell world pos)
        neighbors (get-neighbors world pos)
        neighboring-cells (map :pos (vals neighbors))]
    (loop [neighbors neighbors
           world world]
      (if-let [[neighbor & neighbors] neighbors]
        (let [[dir {pos :pos}] neighbor
              c (get-cell world pos)]
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
  [world recipe pos weights]
  (loop [neighbors #{pos}
         world world]
    (if (seq neighbors)
      (let [pos (lowest-entropy-cell neighbors world weights)
            [world' next] (collapse-neighbors* world recipe pos)]
        (recur (if (= world world')
                 (disj neighbors pos)
                 (disj (into neighbors next) pos))
               world'))
      world)))

(defn- populate-world [world sample]
  (let [superpos (get-superposition sample)]
    (mapv (fn [r] (mapv (fn [c] (if (nil? c) superpos c)) r)) world)))

(defn- pre-filled? [world]
  (some some? (flatten world)))

(defn- init-world-and-get-starting-pos
  "Returns super-position populated world and the least random starting
  point, if the world was pre-filled before. Otherwise returns a
  random point."
  [world recipe sample weights]
  (let [width (count (first world))
        height (count world)
        world' (populate-world world sample)
        positions (->> (for [x (range width) y (range height)] [x y])
                       (filter #(and (not (set? (get-cell world' %)))
                                     (some set? (map :cell (vals (get-neighbors world' %)))))))
        positions (sort (fn [a b]
                          (< (cell-entropy (apply set/union (vals (get recipe (get-cell world' a)))) weights)
                             (cell-entropy (apply set/union (vals (get recipe (get-cell world' b)))) weights)))
                        positions)]
    (if (pre-filled? world)
      [world' (or (first positions) [0 0])]
      [world' [(rand-int width) (rand-int height)]])))

(defn gen-world
  "Creates a new empty world of given size."
  [max-x max-y]
  (mapv vec (for [_ (range max-y)]
              (for [_ (range max-x)] nil))))

(defn wfc
  "Main algorithm loop.
  The loop itself is done in a callback style by calling
  `.requestAnimationFrame` method of a `window`."
  [world sample draw-callback]
  (let [recipe (build-recipe sample)
        renderer (:render-view @config/*dom-elements)
        ctx (.getContext renderer "2d")
        weights (sample-weights sample)
        [world pos] (init-world-and-get-starting-pos world recipe sample weights)]
    ((fn compute [world pos]
       (if-not (done? world)
         (let [cell (get-cell world pos)
               world (cond-> world
                       (set? cell) (collapse pos (weighted-random cell weights))
                       true (collapse-neighbors recipe pos weights))
               pos (lowest-entropy-cell world weights)]
           (when (:animate? @config/*state)
             (draw-callback world))
           (.requestAnimationFrame js/window #(compute world pos)))
         (do (draw-callback world)
             (swap! config/*state assoc
                    :world-state world
                    :rendered-image (cu/get-image ctx)
                    :move? true))))
     world pos)))
