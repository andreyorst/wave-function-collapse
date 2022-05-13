(ns wfc.render
  (:require
   [wfc.wfc :refer [wfc gen-world clamp]]
   [wfc.sample :as sample :refer [tiles sample]]
   [wfc.config :as config]))

(defn draw [ctx image x y size]
  (let [arr (js/Uint8ClampedArray. image.data)
        place (.getImageData ctx x y size size)]
    (.set (.-data place) arr)
    (.putImageData ctx place x y)))

(defonce ^:dynamic *world* nil)

(defn solve [world ctx size]
  (let [tiles tiles
        world (wfc world sample)]
    (set! *world* world)
    (loop [world world
           x 0]
      (when-let [[row & rows] world]
        (loop [row row
               y 0]
          (when-let [[cell & cells] row]
            (draw ctx (get tiles cell cell) x y size)
            (recur cells (+ y size))))
        (recur rows (+ x size))))))

(defn render [_]
  (if (number? config/tile-size)
    (when-let [renderer (.getElementById js/document "render_view")]
      (config/clear-error "render_error")
      (let [ctx (.getContext renderer "2d")
            width (.-width renderer)
            height (.-height renderer)
            size config/tile-size
            world (gen-world (/ width size) (/ height size))]
        (.clearRect ctx 0 0 width height)
        (solve world ctx size)))
     (config/display-error "render_error" "Please set tile size")))

(defn shift [world dir]
  (case dir
    "up" (mapv #(into [] (cons nil (butlast %))) world)
    "down" (mapv #(conj (into [] (drop 1) %) nil) world)
    "right" (conj (into [] (drop 1) world) (into [] (repeat (count (first world)) nil)))
    "left" (into [] (cons (into [] (repeat (count (first world)) nil)) (into [] (butlast world))))))

(defn move [dir]
  (if-let [world *world*]
    (when-let [renderer (.getElementById js/document "render_view")]
      (let [ctx (.getContext renderer "2d")
            world (shift world dir)]
        (solve world ctx config/tile-size)))
    (config/display-error "render_error" "Please generate an image")))

(defn set-world-size [_]
  (when-let [renderer (.getElementById js/document "render_view")]
    (config/clear-error "render_error")
    (let [width (config/get-text-input-value "world_width")
          height (config/get-text-input-value "world_height")
          tile-size config/tile-size]
      (if (and width height)
        (if tile-size
          (let [width (clamp 0 (* width tile-size) config/max-world-pixel-width)
                height (clamp 0 (* height tile-size) config/max-world-pixel-height)]
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
