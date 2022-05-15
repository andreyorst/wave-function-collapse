(ns wfc.canvas-utils
  (:require
   [wfc.config :as config]
   [wfc.impl :refer [clamp]]))

(defonce canvas-base "#dcdcdc")
(defonce canvas-check "#eee")

(defn create-canvas [width height]
  (let [canvas (.createElement js/document "canvas")]
    (set! canvas.width width)
    (set! canvas.height height)
    canvas))

(defn get-image [canvas]
  (let [ctx (.getContext canvas "2d")]
    (.getImageData ctx 0 0 canvas.width canvas.height)))

(defn draw
  ([canvas image x y size]
   (draw canvas image x y size size))
  ([canvas image x y width height]
   (let [ctx (.getContext canvas "2d")
         arr (js/Uint8ClampedArray. image.data)
         place (.getImageData ctx x y width height)]
     (.set place.data arr)
     (.putImageData ctx place x y))))

(defn draw-grid
  ([canvas tile-size]
   (draw-grid canvas tile-size :dash "#000" true))
  ([canvas tile-size style redraw?]
   (draw-grid canvas tile-size style "#000" redraw?))
  ([canvas tile-size style color redraw?]
   (let [ctx (.getContext canvas "2d")
         width canvas.width
         height canvas.height]
     (when redraw?
       (.clearRect ctx 0 0 width height)
       (.drawImage ctx (:image @config/image) 0 0))
     (set! ctx.lineWidth 2)
     (set! ctx.strokeStyle color)
     (.beginPath ctx)
     (when (= style :dash)
       (.setLineDash ctx [4 4]))
     (doseq [x (range tile-size width tile-size)]
       (.moveTo ctx x 0)
       (.lineTo ctx x height))
     (doseq [y (range tile-size height tile-size)]
       (.moveTo ctx 0 y)
       (.lineTo ctx width y))
     (.stroke ctx)
     (.closePath ctx))))

(defn draw-checker-board [canvas width height grid-size]
  (let [ctx (.getContext canvas "2d")]
    (set! ctx.fillStyle canvas-base)
    (.fillRect ctx 0 0 width height)
    (set! ctx.fillStyle "#eee")
    (doseq [x (range 0 width (* grid-size 2))
            y (range 0 height (* grid-size 2))]
      (.fillRect ctx x y grid-size grid-size))
    (doseq [x (range grid-size width (* grid-size 2))
            y (range grid-size height (* grid-size 2))]
      (.fillRect ctx x y grid-size grid-size))))

(defn init-canvas
  ([canvas] (init-canvas canvas 32 ""))
  ([canvas grid-size] (init-canvas canvas grid-size ""))
  ([canvas grid-size text]
   (let [ctx (.getContext canvas "2d")
         width (clamp 0 canvas.width config/max-world-pixel-width)
         height (clamp 0 canvas.height config/max-world-pixel-height)]
     (set! canvas.width width)
     (set! canvas.height height)
     (draw-checker-board canvas width height grid-size)
     (set! ctx.textBaseline "middle")
     (set! ctx.textAlign "center")
     (set! ctx.font "1.4em Arial")
     (set! ctx.fillStyle "black")
     (.fillText ctx text (/ width 2) (/ height 2)))))
