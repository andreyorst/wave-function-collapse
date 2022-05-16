(ns wfc.render
  (:require
   [wfc.impl :as impl]
   [wfc.config :as config]
   [wfc.canvas-utils :as cu]))

(defn draw-world [ctx size tiles fallback world]
  (loop [world world
         x 0]
    (when-let [[row & rows] world]
      (loop [row row
             y 0]
        (when-let [[cell & cells] row]
          (cu/draw ctx (get tiles cell fallback) x y size)
          (recur cells (+ y size))))
      (recur rows (+ x size)))))

(defn- solve [world ctx size animate?]
  (let [{:keys [tiles sample tile-size]} @config/*state
        fallback-ctx (.getContext (cu/create-canvas tile-size tile-size) "2d")
        _ (cu/draw-checker-board fallback-ctx tile-size tile-size 16)
        fallback (cu/get-image fallback-ctx)]
    (impl/wfc world sample (partial draw-world ctx size tiles fallback) animate?)))

(defn render [_]
  (let [{:keys [tile-size world-state]} @config/*state]
    (if (number? tile-size)
      (when-let [renderer (get @config/*dom-elements :render-view)]
        (config/clear-error :render-error)
        (let [ctx (.getContext renderer "2d")
              width renderer.width
              height renderer.height]
          (cu/draw-checker-board ctx width height 16)
          (solve (or world-state
                     (impl/gen-world (/ width tile-size) (/ height tile-size)))
                 ctx tile-size
                 (:animate? @config/*state))))
      (config/display-error :render-error "Please set tile size"))))

(defn- shift [world dir]
  (case dir
    :up (mapv #(into [] (cons nil (butlast %))) world)
    :down (mapv #(conj (into [] (drop 1) %) nil) world)
    :right (conj (into [] (drop 1) world)
                 (into [] (repeat (count (first world)) nil)))
    :left (into [] (cons (into [] (repeat (count (first world)) nil))
                         (into [] (butlast world))))))

(defn move [dir]
  (if-let [{:keys [world-state tile-size]} @config/*state]
    (when-let [renderer (get @config/*dom-elements :render-view)]
      (let [world (shift world-state dir)
            ctx (.getContext renderer "2d")]
        (swap! config/*state assoc :move? false)
        (solve world ctx tile-size false)))
    (config/display-error :render-error "Please generate an image")))

(defn clear-render-view []
  (when-let [renderer (get @config/*dom-elements :render-view)]
    (if-let [tile-size (:tile-size @config/*state)]
      (do (config/clear-error :render-error)
          (cu/init-canvas renderer)
          (swap! config/*state assoc :world-state
                 (impl/gen-world (/ renderer.width tile-size)
                                 (/ renderer.height tile-size)))
          (swap! config/*state dissoc :rendered-image)
          (swap! config/*state assoc :move? false))
      (config/display-error :render-error "Please set tile size"))))

(defn set-world-size [_]
  (when-let [renderer (get @config/*dom-elements :render-view)]
    (config/clear-error :render-error)
    (let [width (config/get-text-input-value :world-width-input)
          height (config/get-text-input-value :world-height-input)
          {:keys [tile-size]} @config/*state]
      (if (and width height)
        (if tile-size
          (let [width (impl/clamp (* tile-size 4) (* width tile-size) config/max-world-pixel-width)
                height (impl/clamp (* tile-size 4) (* height tile-size) config/max-world-pixel-height)]
            (set! renderer.width width)
            (set! renderer.height height)
            (set! (.-value (get @config/*dom-elements :world-width-input))
                  (Math/floor (/ width tile-size)))
            (set! (.-value (get @config/*dom-elements :world-height-input))
                  (Math/floor (/ height tile-size)))
            (clear-render-view))
          (config/display-error :render-error "Please set tile size"))
        (cond (not width)
              (config/display-error :render-error "Wrong world width")
              (not height)
              (config/display-error :render-error "Wrong world height"))))))

(defn toggle-animate [event]
  (swap! config/*state assoc :animate? event.target.checked))
