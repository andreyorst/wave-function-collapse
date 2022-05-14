(ns wfc.config
  (:require [wfc.impl :refer [clamp]]))

(defonce tile-size (atom nil))
(defonce sample (atom []))
(defonce tiles (atom {}))
(defonce image
  (atom {:image nil
         :width 0
         :height 0}))

;; Make sure that the output is divisable by 32 to make all samples work on mobile
(defonce max-world-pixel-height
  (if (.-matches (js/window.matchMedia "(max-width: 552px)"))
    (-> js/document.documentElement.clientWidth
        (- 40)                          ; canvas element padding
        (/ 32)
        Math/floor
        (* 32))
    512))

(defonce max-world-pixel-width max-world-pixel-height)

(defn get-text-input-value [id]
  (when-let [tile-size (.getElementById js/document id)]
    (let [value (js/parseInt tile-size.value)]
      (if (js/isNaN value)
        nil
        value))))

(defn init-canvas
  ([canvas] (init-canvas canvas 32 ""))
  ([canvas grid-size] (init-canvas canvas grid-size ""))
  ([canvas grid-size text]
   (let [ctx (.getContext canvas "2d")
         width (clamp 0 canvas.width max-world-pixel-width)
         height (clamp 0 canvas.height max-world-pixel-height)]
     (set! canvas.width width)
     (set! canvas.height height)
     (set! ctx.fillStyle  "#dcdcdc")
     (set! ctx.textBaseline "middle")
     (set! ctx.textAlign "center")
     (set! ctx.font "1.4em Arial")
     (.fillRect ctx 0 0 width height)
     (set! ctx.fillStyle  "#eee")
     (doseq [x (range 0 width (* grid-size 2))
             y (range 0 height (* grid-size 2))]
       (.fillRect ctx x y grid-size grid-size))
     (doseq [x (range grid-size width (* grid-size 2))
             y (range grid-size height (* grid-size 2))]
       (.fillRect ctx x y grid-size grid-size))
     (set! ctx.fillStyle "black")
     (.fillText ctx text (/ width 2) (/ height 2)))))

(defn display-error [error_class msg]
  (when-let [box (.getElementById js/document error_class)]
    (set! box.innerHTML msg)
    (let [style box.style]
      (set! style.opacity 1)
      (set! style.height "42px")
      (set! style.marginBottom "10px"))))

(defn clear-error [error_class]
  (when-let [box (.getElementById js/document error_class)]
    (let [style box.style]
      (set! style.opacity 0)
      (set! style.height 0)
      (set! style.marginBottom "0px"))))
