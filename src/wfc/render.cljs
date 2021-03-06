(ns wfc.render
  "Handles rendering, setting world size, and animation."
  (:require
   [wfc.impl :as impl]
   [wfc.config :as config]
   [wfc.canvas-utils :as cu]))

(defn- create-fallback-tile [tile-size]
  (let [canvas (cu/create-canvas tile-size tile-size)
        ctx (.getContext canvas "2d")]
    (cu/draw-checkerboard ctx tile-size tile-size 16)
    (cu/get-image ctx)))

(defn- create-entropy-tile [size cell weights]
  (let [canvas (cu/create-canvas size size)
        ctx (.getContext canvas "2d")]
    (cu/init-canvas canvas (str (/ (Math/floor (* (impl/cell-entropy cell weights) 10)) 10)))
    (cu/get-image ctx)))

(defn- draw-world [ctx size tiles sample world]
  (let [fallback (and (not (:show-entropy? @config/*state))
                      (create-fallback-tile size))
        weigths (impl/sample-weights sample)]
    (loop [world world
           y 0]
      (when-let [[row & rows] world]
        (loop [row row
               x 0]
          (when-let [[cell & cells] row]
            (let [fallback (or fallback (create-entropy-tile size cell weigths))]
              (cu/draw ctx (get tiles cell fallback) x y size)
              (recur cells (+ x size)))))
        (recur rows (+ y size))))))

(defn- solve [world ctx size]
  (let [{:keys [tiles sample]} @config/*state]
    (impl/wfc world sample (partial draw-world ctx size tiles sample))))

(defn toggle-animate
  "Reads the value of the `Animate` check box."
  [event]
  (let [val event.target.checked]
    (swap! config/*state assoc :animate? val)
    (set! (.-disabled (:entropy-button @config/*dom-elements)) (not val))))

(defn toggle-entropy
  "Reads the value of the `Show entropy` check box."
  [event]
  (swap! config/*state assoc :show-entropy? event.target.checked))

(defn render
  "Render an image based on current tile set and rules.
  Requires `tile-size` to be set.
  The main work is done in `solve`."
  [_]
  (let [{:keys [tile-size world-state]} @config/*state]
    (if (number? tile-size)
      (when-let [renderer (get @config/*dom-elements :render-view)]
        (config/clear-error :render-error)
        (let [ctx (.getContext renderer "2d")
              width renderer.width
              height renderer.height
              animate? (:animate? @config/*state)]
          (when animate?
            (cu/draw-checkerboard ctx width height 16))
          (solve (or world-state
                     (impl/gen-world (/ width tile-size) (/ height tile-size)))
                 ctx tile-size)))
      (config/display-error :render-error "Please set tile size"))))

(defn clear-render-view
  "Clears the output image, and resets the world state, so that the
  Generate button would create a new one."
  []
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

(defn set-world-size
  "Changes the size of the canvas and recreates a new empty world state
  for a given width and height, obtained from the respecting inputs."
  [_]
  (when-let [renderer (get @config/*dom-elements :render-view)]
    (config/clear-error :render-error)
    (let [width (config/get-text-input-value :world-width-input)
          height (config/get-text-input-value :world-height-input)
          {:keys [tile-size]} @config/*state]
      (if (and width height)
        (if tile-size
          (let [width (cu/clamp (* tile-size 4) (* width tile-size) config/max-world-pixel-width)
                height (cu/clamp (* tile-size 4) (* height tile-size) config/max-world-pixel-height)]
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
