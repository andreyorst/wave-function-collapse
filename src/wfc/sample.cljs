(ns wfc.sample
  (:require [wfc.config :as config]
            [wfc.impl :refer [clamp]]
            [wfc.canvas-utils :as cu]
            [wfc.editor :as editor]))

(defn split-to-tiles [{:keys [width height image]} tile-size]
  (let [canvas (cu/create-canvas width height)
        ctx (.getContext canvas "2d")]
    (.drawImage ctx image 0 0)
    (->> (for [x (range 0 height tile-size)]
           (->> (for [y (range 0 width tile-size)
                      :let [img (.getImageData ctx x y tile-size tile-size)]]
                  [(hash (vec img.data)) img])
                (into [])))
         (into []))))

(defn draw [event]
  (when-let [viewer (.getElementById js/document "sample_view")]
    (let [ctx (.getContext viewer "2d")
          img (js/Image.)]
      (->> #(let [width (clamp 0 img.width config/max-world-pixel-width)
                  height (clamp 0 img.height config/max-world-pixel-height)]
              (reset! config/image {:image img
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
  (reset! config/sample (mapv #(mapv first %) hashes-tiles))
  (reset! config/tiles (into {} (apply concat hashes-tiles))))

(defn set-tile-size! [value]
  (reset! config/tile-size value)
  (set! (.-value (.getElementById js/document "tile_size")) value)
  (-> @config/image (split-to-tiles value) (set-sample!)))

(defn set-tile-size [_]
  (config/clear-error "sample_error")
  (if-let [value (config/get-text-input-value "tile_size")]
    (when-let [sample-viewer (.getElementById js/document "sample_view")]
      (if (:image @config/image)
        (do (set-tile-size! value)
            (cu/draw-grid sample-viewer value)
            (editor/draw-tile-picker))
        (config/display-error "sample_error" "Please upload an image")))
    (config/display-error "sample_error" "Wrong tile size")))

(defn load-image [sample tile-size]
  (fn [_]
    (when-let [viewer (.getElementById js/document "sample_view")]
      (let [img (js/Image.)
            handler (fn []
                      (let [width (clamp 0 img.width config/max-world-pixel-width)
                            height (clamp 0 img.height config/max-world-pixel-height)]
                        (config/clear-error "sample_error")
                        (reset! config/image {:image img :width width :height height})
                        (set! viewer.width width)
                        (set! viewer.height height)
                        (set-tile-size! tile-size)
                        (cu/draw-grid viewer tile-size)
                        (editor/draw-tile-picker)))]
        (.addEventListener img "load" handler)
        (set! img.src sample)))))
