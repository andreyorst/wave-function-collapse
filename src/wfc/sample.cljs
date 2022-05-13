(ns wfc.sample
  (:require [wfc.config :as config]
            [wfc.wfc :refer [clamp]]))

(defonce sample [])
(defonce tiles {})
(defonce image {:image nil
                :width 0
                :height 0})

(defn split-to-tiles [image tile-size]
  (let [canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-width canvas) (:width image))
    (set! (.-height canvas) (:height image))
    (.drawImage ctx (:image image) 0 0)
    (mapv
     (fn [x]
       (mapv (fn [y]
               (let [img (.getImageData ctx x y tile-size tile-size)]
                 [(hash (vec img.data)) img]))
             (range 0 (:height image) tile-size)))
     (range 0 (:width image) tile-size))))

(defn draw-grid [tile-size]
  (when-let [viewer (.getElementById js/document "sample_view")]
    (let [ctx (.getContext viewer "2d")
          width (.-width viewer)
          height (.-height viewer)]
      (.clearRect ctx 0 0 width height)
      (.drawImage ctx (:image image) 0 0)
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
    (let [img (js/Image.)
          ctx (.getContext viewer "2d")
          handler (fn []
                    (let [width (clamp 0 (.-width image) config/max-world-pixel-width)
                          height (clamp 0 (.-height image) config/max-world-pixel-height)]
                      (set! image {:image img
                                   :width width
                                   :height height})
                      (set! config/tile-size nil)
                      (set! (.-width viewer) width)
                      (set! (.-height viewer) height)
                      (.clearRect ctx 0 0 (.-width viewer) (.-height viewer))
                      (.drawImage ctx img 0 0)))]
      (.addEventListener img "load" handler)
      (set! (.-src img) (-> event .-target .-result)))))

(defn upload [event]
  (when-let [files (-> event .-target .-files)]
    (let [reader (js/FileReader.)]
      (.addEventListener reader "load" draw)
      (.readAsDataURL reader (aget files 0)))))

(defn set-sample! [hashes-tiles]
  (set! sample (mapv #(mapv first %) hashes-tiles))
  (set! tiles (into {} (apply concat hashes-tiles))))

(defn set-tile-size! [value]
  (config/clear-error "sample_error")
  (if (:image image)
    (do
      (set! config/tile-size value)
      (set! (.-value (.getElementById js/document "tile_size")) value)
      (-> image (split-to-tiles value) (set-sample!)))
    (config/display-error "sample_error" "Please upload an image")))

(defn set-tile-size [_]
  (config/clear-error "sample_error")
  (if-let [value (config/get-text-input-value "tile_size")]
    (do (set-tile-size! value)
        (draw-grid value))
    (config/display-error "sample_error" "Wrong tile size")))

(defn load-image [sample tile-size]
  (fn [_]
    (when-let [viewer (.getElementById js/document "sample_view")]
      (let [img (js/Image.)
            handler (fn []
                      (let [width (clamp 0 (.-width img) config/max-world-pixel-width)
                            height (clamp 0 (.-height img) config/max-world-pixel-height)]
                        (set! image {:image img :width width :height height})
                        (set! (.-width viewer) width)
                        (set! (.-height viewer) height)
                        (set-tile-size! tile-size)
                        (draw-grid tile-size)))]
        (.addEventListener img "load" handler)
        (set! (.-src img) sample)))))
