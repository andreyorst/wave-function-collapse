(ns wfc.sample
  (:require [wfc.config :as config]
            [wfc.impl :refer [clamp]]))

(defonce sample (atom []))
(defonce tiles (atom {}))
(defonce image (atom {:image nil
                      :width 0
                      :height 0}))

(defn create-canvas [width height]
  (let [canvas (.createElement js/document "canvas")]
    (set! canvas.width width)
    (set! canvas.height height)
    canvas))

(defn split-to-tiles [{:keys [width height image]} tile-size]
  (let [canvas (create-canvas width height)
        ctx (.getContext canvas "2d")]
    (.drawImage ctx image 0 0)
    (->> (for [x (range 0 height tile-size)]
           (->> (for [y (range 0 width tile-size)
                      :let [img (.getImageData ctx x y tile-size tile-size)]]
                  [(hash (vec img.data)) img])
                (into [])))
         (into []))))

(defn draw-grid [tile-size]
  (when-let [viewer (.getElementById js/document "sample_view")]
    (let [ctx (.getContext viewer "2d")
          width viewer.width
          height viewer.height]
      (.clearRect ctx 0 0 width height)
      (.drawImage ctx (:image @image) 0 0)
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
    (let [ctx (.getContext viewer "2d")
          img (js/Image.)]
      (->> #(let [width (clamp 0 image.width config/max-world-pixel-width)
                  height (clamp 0 image.height config/max-world-pixel-height)]
              (reset! image {:image img
                             :width width
                             :height height})
              (reset! config/tile-size nil)
              (set! viewer.width width)
              (set! viewer.height height)
              (.clearRect ctx 0 0 viewer.width viewer.height)
              (.drawImage ctx img 0 0))
           (.addEventListener img "load"))
      (set! img.src event.target.result))))

(defn upload [event]
  (when-let [files event.target.files]
    (let [reader (js/FileReader.)]
      (.addEventListener reader "load" draw)
      (.readAsDataURL reader (aget files 0)))))

(defn set-sample! [hashes-tiles]
  (reset! sample (mapv #(mapv first %) hashes-tiles))
  (reset! tiles (into {} (apply concat hashes-tiles))))

(defn set-tile-size! [value]
  (config/clear-error "sample_error")
  (if (:image @image)
    (do
      (reset! config/tile-size value)
      (set! (.-value (.getElementById js/document "tile_size")) value)
      (-> @image (split-to-tiles value) (set-sample!)))
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
                      (let [width (clamp 0 img.width config/max-world-pixel-width)
                            height (clamp 0 img.height config/max-world-pixel-height)]
                        (reset! image {:image img :width width :height height})
                        (set! viewer.width width)
                        (set! viewer.height height)
                        (set-tile-size! tile-size)
                        (draw-grid tile-size)))]
        (.addEventListener img "load" handler)
        (set! img.src sample)))))
