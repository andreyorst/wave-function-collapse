(ns wfc.render
  (:require
   [wfc.impl :as impl]
   [wfc.config :as config]
   [wfc.canvas-utils :as cu]))

(defn- solve [world canvas size]
  (let [tiles @config/*tiles
        world (impl/wfc world @config/*sample)
        fallback (js/Image.)]
    (reset! config/*world-state world)
    (loop [world world
           x 0]
      (when-let [[row & rows] world]
        (loop [row row
               y 0]
          (when-let [[cell & cells] row]
            (cu/draw canvas (get tiles cell fallback) x y size)
            (recur cells (+ y size))))
        (recur rows (+ x size))))
    (reset! config/*rendered-image (cu/get-image canvas))))

(defn render [_]
  (if (number? @config/*tile-size)
    (when-let [renderer (.getElementById js/document "render_view")]
      (config/clear-error "render_error")
      (let [ctx (.getContext renderer "2d")
            width renderer.width
            height renderer.height
            size @config/*tile-size]
        (.clearRect ctx 0 0 width height)
        (solve (or @config/*world-state
                   (impl/gen-world (/ width size) (/ height size)))
               renderer size))
      (reset! config/*move? true))
     (config/display-error "render_error" "Please set tile size")))

(defn- shift [world dir]
  (case dir
    :up (mapv #(into [] (cons nil (butlast %))) world)
    :down (mapv #(conj (into [] (drop 1) %) nil) world)
    :right (conj (into [] (drop 1) world)
                 (into [] (repeat (count (first world)) nil)))
    :left (into [] (cons (into [] (repeat (count (first world)) nil))
                         (into [] (butlast world))))))

(defn move [dir]
  (if-let [world @config/*world-state]
    (when-let [renderer (.getElementById js/document "render_view")]
      (let [world (shift world dir)]
        (solve world renderer @config/*tile-size)))
    (config/display-error "render_error" "Please generate an image")))

(defn clear-render-view []
  (when-let [renderer (.getElementById js/document "render_view")]
    (let [tile-size @config/*tile-size]
      (cu/init-canvas renderer tile-size)
      (reset! config/*world-state
              (impl/gen-world (/ renderer.width tile-size)
                              (/ renderer.height tile-size)))
      (reset! config/*rendered-image nil)
      (reset! config/*move? false))))

(defn set-world-size [_]
  (when-let [renderer (.getElementById js/document "render_view")]
    (config/clear-error "render_error")
    (let [width (config/get-text-input-value "world_width")
          height (config/get-text-input-value "world_height")
          tile-size @config/*tile-size]
      (if (and width height)
        (if tile-size
          (let [width (impl/clamp 0 (* width tile-size) config/max-world-pixel-width)
                height (impl/clamp 0 (* height tile-size) config/max-world-pixel-height)]
            (set! renderer.width width)
            (set! renderer.height height)
            (set! (.-value (.getElementById js/document "world_width"))
                  (Math/floor (/ width tile-size)))
            (set! (.-value (.getElementById js/document "world_height"))
                  (Math/floor (/ height tile-size)))
            (clear-render-view))
          (config/display-error "render_error" "Please set tile size"))
        (cond (not width)
              (config/display-error "render_error" "Wrong world width")
              (not height)
              (config/display-error "render_error" "Wrong world height"))))))
