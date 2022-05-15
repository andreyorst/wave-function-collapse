(ns wfc.canvas-utils
  (:require
   [wfc.config :as config]
   [wfc.impl :refer [clamp]]))

(defn create-canvas [width height]
  (let [canvas (.createElement js/document "canvas")]
    (set! canvas.width width)
    (set! canvas.height height)
    canvas))

(defn get-image [ctx]
  (.getImageData ctx 0 0 ctx.canvas.width ctx.canvas.height))

(defn draw
  ([ctx image x y size]
   (draw ctx image x y size size))
  ([ctx image x y width height]
   (let [arr (js/Uint8ClampedArray. image.data)
         place (.getImageData ctx x y width height)]
     (.set place.data arr)
     (.putImageData ctx place x y))))

(defn draw-grid
  ([ctx tile-size]
   (draw-grid ctx tile-size :dash "#000" true))
  ([ctx tile-size style redraw?]
   (draw-grid ctx tile-size style "#000" redraw?))
  ([ctx tile-size style color redraw?]
   (let [width ctx.canvas.width
         height ctx.canvas.height]
     (when redraw?
       (.clearRect ctx 0 0 width height)
       (.drawImage ctx (:image @config/*state) 0 0))
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

(defn draw-checker-board
  ([ctx width height grid-size]
   (draw-checker-board ctx 0 0 width height grid-size))
  ([ctx x y width height grid-size]
   (let [[base check] (if config/dark-mode?
                        ["#2e2e2e" "#333"]
                        ["#dcdcdc"  "#eee"])]
     (set! ctx.fillStyle base)
     (.fillRect ctx x y width height)
     (set! ctx.fillStyle check)
     (doseq [x (range x (+ x width) (* grid-size 2))
             y (range y (+ y height) (* grid-size 2))]
       (.fillRect ctx x y grid-size grid-size))
     (doseq [x (range (+ x grid-size) (+ x width) (* grid-size 2))
             y (range (+ y grid-size) (+ y height) (* grid-size 2))]
       (.fillRect ctx x y grid-size grid-size)))))

(defn init-canvas
  ([canvas] (init-canvas canvas ""))
  ([canvas text]
   (let [ctx (.getContext canvas "2d")
         width (clamp 0 canvas.width config/max-world-pixel-width)
         height (clamp 0 canvas.height config/max-world-pixel-height)]
     (set! canvas.width width)
     (set! canvas.height height)
     (draw-checker-board ctx width height 16)
     (set! ctx.textBaseline "middle")
     (set! ctx.textAlign "center")
     (set! ctx.font "1.4em Arial")
     (set! ctx.fillStyle (if config/dark-mode? "#dedddc" "#555"))
     (.fillText ctx text (/ width 2) (/ height 2)))))
