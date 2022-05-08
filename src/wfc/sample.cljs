(ns wfc.sample
  (:require [wfc.config :as config]
            [wfc.wfc :refer [clamp]]))

(defonce ^:dynamic *sample* [])
(defonce ^:dynamic *tiles* {})
(defonce ^:dynamic *image* {:image nil
                            :tile-size 0
                            :width 0
                            :height 0})

(defn split-to-tiles [tile-size]
  (let [image *image*
        canvas (.createElement js/document "canvas")
        _ (set! (.-width canvas) (:width image))
        _ (set! (.-height canvas) (:height image))
        ctx (.getContext canvas "2d")
        _ (.drawImage ctx (:image image) 0 0)
        hashes-imgs (for [x (range 0 (:width image) tile-size)]
                      (for [y (range 0 (:height image) tile-size)]
                        (let [img (.getImageData ctx x y tile-size tile-size)]
                          [(hash (vec img.data)) img])))]
    (set! *sample* (mapv #(mapv first %) hashes-imgs))
    (set! *tiles* (into {} (apply concat hashes-imgs)))))

(defn draw-grid [tile-size]
  (when-let [viewer (.getElementById js/document "sample_view")]
    (let [ctx (.getContext viewer "2d")
          width (.-width viewer)
          height (.-height viewer)]
      (.clearRect ctx 0 0 width height)
      (.drawImage ctx (:image *image*) 0 0)
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
        (.closePath ctx)))))

(defn draw [event]
  (when-let [viewer (.getElementById js/document "sample_view")]
    (let [image (js/Image.)
          ctx (.getContext viewer "2d")
          handler (fn []
                    (let [width (clamp 0 (.-width image) config/max-world-pixel-width)
                          height (clamp 0 (.-height image) config/max-world-pixel-height)]
                      (set! *image* {:image image
                                     :width width
                                     :height height})
                      (set! config/*tile-size* nil)
                      (set! (.-width viewer) width)
                      (set! (.-height viewer) height)
                      (.clearRect ctx 0 0 (.-width viewer) (.-height viewer))
                      (.drawImage ctx image 0 0)))]
      (.addEventListener image "load" handler)
      (set! (.-src image) (-> event .-target .-result)))))

(defn upload [event]
  (when-let [files (-> event .-target .-files)]
    (let [reader (js/FileReader.)]
      (.addEventListener reader "load" draw)
      (.readAsDataURL reader (aget files 0)))))

(defn set-tile-size' [value]
  (config/clear-error "sample_error")
  (if (:image *image*)
    (do (set! config/*tile-size* value)
        (set! (.-value (.getElementById js/document "tile_size")) value)
        (config/clear-error "sample_error")
        (draw-grid value)
        (split-to-tiles value))
    (config/display-error "sample_error" "Please upload an image")))

(defn set-tile-size [_]
  (config/clear-error "sample_error")
  (if-let [value (config/get-text-input-value "tile_size")]
    (set-tile-size' value)
    (config/display-error "sample_error" "Wrong tile size")))

(defn load-image [img tile-size]
  (fn [_]
    (when-let [viewer (.getElementById js/document "sample_view")]
      (let [image (js/Image.)
            ctx (.getContext viewer "2d")
            handler (fn []
                      (let [width (clamp 0 (.-width image) config/max-world-pixel-width)
                            height (clamp 0 (.-height image) config/max-world-pixel-height)]
                        (set! *image* {:image image
                                       :width width
                                       :height height})
                        (set! (.-width viewer) width)
                        (set! (.-height viewer) height)
                        (set-tile-size' tile-size)
                        (.clearRect ctx 0 0 (.-width viewer) (.-height viewer))
                        (.drawImage ctx image 0 0)
                        (draw-grid tile-size)
                        (split-to-tiles tile-size)))]
        (.addEventListener image "load" handler)
        (set! (.-src image) img)))))
