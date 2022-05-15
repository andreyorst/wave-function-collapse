(ns wfc.sample
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.editor :as editor]
   [wfc.impl :refer [clamp]]))

(defn split-to-tiles [image width height tile-size]
  (let [canvas (cu/create-canvas width height)
        ctx (.getContext canvas "2d")]
    (.drawImage ctx image 0 0)
    (->> (for [x (range 0 width tile-size)]
           (->> (for [y (range 0 height tile-size)
                      :let [img (.getImageData ctx x y tile-size tile-size)]]
                  [(hash (vec img.data)) img])
                (into [])))
         (into []))))

(defn draw [event]
  (when-let [viewer (get @config/*dom-elements :sample-view)]
    (let [ctx (.getContext viewer "2d")
          img (js/Image.)]
      (->> #(let [width (clamp 0 img.width config/max-world-pixel-width)
                  height (clamp 0 img.height config/max-world-pixel-height)]
              (swap! config/*state assoc :image img :image-width width :image-height height)
              (swap! config/*state dissoc :tile-size)
              (set! viewer.width width)
              (set! viewer.height height)
              (editor/hide-tile-picker)
              (.clearRect ctx 0 0 viewer.width viewer.height)
              (.drawImage ctx img 0 0)
              (swap! config/*state dissoc :world-state))
           (.addEventListener img "load"))
      (set! img.src event.target.result))))

(defn upload [event]
  (when-let [files event.target.files]
    (let [reader (js/FileReader.)]
      (.addEventListener reader "load" draw)
      (.readAsDataURL reader (aget files 0)))))

(defn set-sample! [hashes-tiles]
  (swap! config/*state assoc :sample (mapv #(mapv first %) hashes-tiles))
  (swap! config/*state assoc :tiles (into {} (apply concat hashes-tiles))))

(defn set-tile-size! [value]
  (let [value (clamp 16 (* value 2) 128)
        {:keys [image image-width image-height]} @config/*state]
    (swap! config/*state assoc :tile-size value)
    (set! (.-value (get @config/*dom-elements :tile-size-input)) (/ value 2))
    (-> image (split-to-tiles image-width image-height value) (set-sample!))))

(defn set-tile-size [_]
  (config/clear-error :sample-error)
  (config/clear-error :render-error)
  (if-let [value (config/get-text-input-value :tile-size-input)]
    (when-let [sample-viewer (get @config/*dom-elements :sample-view)]
      (if (:image @config/*state)
        (do (set-tile-size! value)
            (cu/draw-grid (.getContext sample-viewer "2d") (:tile-size @config/*state))
            (editor/draw-tile-picker))
        (config/display-error :sample-error "Please upload an image")))
    (config/display-error :sample-error "Wrong tile size")))

(defn load-image [sample tile-size]
  (fn [_]
    (when-let [viewer (get @config/*dom-elements :sample-view)]
      (let [img (js/Image.)
            handler (fn []
                      (let [width (clamp 0 img.width config/max-world-pixel-width)
                            height (clamp 0 img.height config/max-world-pixel-height)]
                        (config/clear-error :sample-error)
                        (swap! config/*state assoc :image img :image-width width :image-height height)
                        (set! viewer.width width)
                        (set! viewer.height height)
                        (set-tile-size! tile-size)
                        (cu/draw-grid (.getContext viewer "2d") (* tile-size 2))
                        (editor/draw-tile-picker)
                        (swap! config/*state dissoc :world-state)))]
        (js/window.scrollTo #js{:top 0 :behavior "smooth"})
        (.addEventListener img "load" handler)
        (set! img.src sample)))))
