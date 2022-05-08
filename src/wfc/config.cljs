(ns wfc.config)

(defonce ^:dynamic *tile-size* nil)
(defonce max-world-pixel-height 512)
(defonce max-world-pixel-width 512)

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
         width (.-width canvas)
         height (.-height canvas)]
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
