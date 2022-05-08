(ns wfc.render
  (:require
   [wfc.wfc :refer [wfc gen-world clamp]]
   [wfc.sample :refer [*tiles* *sample*]]
   [wfc.config :as config]))

(defn draw [ctx image x y size]
  (let [arr (js/Uint8ClampedArray. image.data)
        place (.getImageData ctx x y size size)]
    (.set (.-data place) arr)
    (.putImageData ctx place x y)))

(defn solve [ctx width height]
  (let [tiles *tiles*
        size config/*tile-size*]
    (loop [world (wfc (gen-world (/ width size) (/ height size)) *sample*)
           x 0]
      (when-let [[row & rows] world]
        (loop [row row
               y 0]
          (when-let [[cell & cells] row]
            (draw ctx (get tiles cell) x y size)
            (recur cells (+ y size))))
        (recur rows (+ x size))))))

(defn render [_]
  (if (number? config/*tile-size*)
    (when-let [renderer (.getElementById js/document "render_view")]
      (config/clear-error "render_error")
      (let [ctx (.getContext renderer "2d")
            width (.-width renderer)
            height (.-height renderer)]
        (.clearRect ctx 0 0 width height)
        (solve ctx width height)))
    (config/display-error "render_error" "Please set tile size")))


(defn set-world-size [_]
  (when-let [renderer (.getElementById js/document "render_view")]
    (config/clear-error "render_error")
    (let [width (config/get-text-input-value "world_width")
          height (config/get-text-input-value "world_height")
          tile-size config/*tile-size*]
      (if (and width height)
        (if tile-size
          (let [width (clamp 0 (* width config/*tile-size*) config/max-world-pixel-width)
                height (clamp 0 (* height config/*tile-size*) config/max-world-pixel-height)]
            (set! (.-width renderer) width)
            (set! (.-height renderer) height)
            (set! (.-value (.getElementById js/document "world_width")) (Math/floor (/ width tile-size)))
            (set! (.-value (.getElementById js/document "world_height")) (Math/floor (/ height tile-size)))
            (config/init-canvas renderer tile-size))
          (config/display-error "render_error" "Please set tile size"))
        (cond (not width)
              (config/display-error "render_error" "Wrong world width")
              (not height)
              (config/display-error "render_error" "Wrong world height"))))))
