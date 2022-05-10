(ns wfc.config
  (:require [wfc.wfc :refer [clamp]]))

(defonce ^:dynamic *tile-size* nil)
(defonce mobile? (.-matches (js/window.matchMedia "(max-width: 552px)")))
(defonce client-width js/document.documentElement.clientWidth)
;; Make sure that the output is divisable by 32 to make all samples work on mobile
(defonce max-world-pixel-height (if mobile? (* (Math/floor (/ (- client-width 40) 32)) 32) 512))
(defonce max-world-pixel-width max-world-pixel-height)

(defn get-text-input-value [id]
  (when-let [tile-size (.getElementById js/document id)]
    (let [value (js/parseInt (.-value tile-size))]
      (if (js/isNaN value)
        nil
        value))))

(defn init-canvas
  ([canvas] (init-canvas canvas 32 ""))
  ([canvas grid-size] (init-canvas canvas grid-size ""))
  ([canvas grid-size text]
   (let [ctx (.getContext canvas "2d")
         width (clamp 0 (.-width canvas) max-world-pixel-width)
         height (clamp 0 (.-height canvas) max-world-pixel-height)]
     (set! (.-width canvas) width)
     (set! (.-height canvas) height)
     (set! (.-fillStyle ctx)  "#dcdcdc")
     (set! (.-textBaseline ctx) "middle")
     (set! (.-textAlign ctx) "center")
     (set! (.-font ctx) "1.4em Arial")
     (.fillRect ctx 0 0 width height)
     (set! (.-fillStyle ctx)  "#eee")
     (doseq [x (range 0 width (* grid-size 2))
             y (range 0 height (* grid-size 2))]
       (.fillRect ctx x y grid-size grid-size))
     (doseq [x (range grid-size width (* grid-size 2))
             y (range grid-size height (* grid-size 2))]
       (.fillRect ctx x y grid-size grid-size))
     (set! (.-fillStyle ctx) "black")
     (.fillText ctx text (/ width 2) (/ height 2)))))

(defn display-error [error_class msg]
  (when-let [box (.getElementById js/document error_class)]
    (set! (.-innerHTML box) msg)
    (let [style (.-style box)]
      (set! (.-opacity style) 1)
      (set! (.-height style) "42px")
      (set! (.-marginBottom style) "10px"))))

(defn clear-error [error_class]
  (when-let [box (.getElementById js/document error_class)]
    (let [style (.-style box)]
      (set! (.-opacity style) 0)
      (set! (.-height style) 0)
      (set! (.-marginBottom style) "0px"))))
