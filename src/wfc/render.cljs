(ns wfc.render
  (:require
   [wfc.impl :as impl]
   [wfc.config :as config]
   [wfc.canvas-utils :as cu]))

(defn- solve [world ctx size]
  (let [{:keys [tiles sample]} @config/*state
        world (impl/wfc world sample)
        fallback (js/Image.)]
    (swap! config/*state assoc :world-state world)
    (loop [world world
           x 0]
      (when-let [[row & rows] world]
        (loop [row row
               y 0]
          (when-let [[cell & cells] row]
            (cu/draw ctx (get tiles cell fallback) x y size)
            (recur cells (+ y size))))
        (recur rows (+ x size))))
    (swap! config/*state assoc :rendered-image (cu/get-image ctx))))

(defn render [_]
  (let [{:keys [tile-size world-state]} @config/*state]
    (if (number? tile-size)
      (when-let [renderer (get @config/*dom-elements :render-view)]
        (config/clear-error :render-error)
        (let [ctx (.getContext renderer "2d")
              width renderer.width
              height renderer.height]
          (.clearRect ctx 0 0 width height)
          (solve (or world-state
                     (impl/gen-world (/ width tile-size) (/ height tile-size)))
                 ctx tile-size))
        (swap! config/*state assoc :move? true))
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
        (solve world ctx tile-size)))
    (config/display-error :render-error "Please generate an image")))

(defn clear-render-view []
  (when-let [renderer (get @config/*dom-elements :render-view)]
    (let [{:keys [tile-size]} @config/*state]
      (cu/init-canvas renderer)
      (swap! config/*state assoc :world-state
              (impl/gen-world (/ renderer.width tile-size)
                              (/ renderer.height tile-size)))
      (swap! config/*state dissoc :rendered-image)
      (swap! config/*state assoc :move? false))))

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
