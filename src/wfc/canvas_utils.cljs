(ns wfc.canvas-utils
  (:require [wfc.config :as config]))

(defn create-canvas [width height]
  (let [canvas (.createElement js/document "canvas")]
    (set! canvas.width width)
    (set! canvas.height height)
    canvas))

(defn draw [ctx image x y size]
  (let [arr (js/Uint8ClampedArray. image.data)
        place (.getImageData ctx x y size size)]
    (.set place.data arr)
    (.putImageData ctx place x y)))

(defn draw-grid [canvas tile-size]
  (let [ctx (.getContext canvas "2d")
        width canvas.width
        height canvas.height]
    (.clearRect ctx 0 0 width height)
    (.drawImage ctx (:image @config/image) 0 0)
    (doseq [x (range tile-size width tile-size)]
      (.beginPath ctx)
      (.setLineDash ctx [4 4])
      (.moveTo ctx x 0)
      (.lineTo ctx x height)
      (.stroke ctx)
      (.closePath ctx))
    (doseq [y (range tile-size height tile-size)]
      (.beginPath ctx)
      (.setLineDash ctx [4 4])
      (.moveTo ctx 0 y)
      (.lineTo ctx width y)
      (.stroke ctx)
      (.closePath ctx))))
