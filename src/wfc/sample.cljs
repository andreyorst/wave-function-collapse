(ns wfc.sample
  "Everything related to the sample image.
  Handles uploading, setting tile-size, and splitting into tiles."
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.editor :as editor]))

(defn- split-to-tiles [image width height tile-size]
  (let [canvas (cu/create-canvas width height)
        ctx (.getContext canvas "2d")]
    (.drawImage ctx image 0 0)
    (->> (for [y (range 0 height tile-size)]
           (->> (for [x (range 0 width tile-size)
                      :let [img (.getImageData ctx x y tile-size tile-size)]]
                  [(hash (vec img.data)) img])
                (into [])))
         (into []))))

(defn- draw [event]
  (when-let [viewer (get @config/*dom-elements :sample-view)]
    (let [ctx (.getContext viewer "2d")
          img (js/Image.)]
      (->> #(let [width (cu/clamp 0 img.width config/max-world-pixel-width)
                  height (cu/clamp 0 img.height config/max-world-pixel-height)]
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

(defn- set-sample! [hashes-tiles]
  (swap! config/*state assoc
         :sample (mapv #(mapv first %) hashes-tiles)
         :tiles (into {} (apply concat hashes-tiles))))

(defn- set-tile-size! [value]
  (let [value (cu/clamp 16 (* value 2) 128)
        {:keys [image image-width image-height]} @config/*state]
    (swap! config/*state assoc :tile-size value)
    (set! (.-value (get @config/*dom-elements :tile-size-input)) (/ value 2))
    (-> image (split-to-tiles image-width image-height value) (set-sample!))))

(defn load-image
  "Returns an anonymous function that is registered as event listener
  for loading example images."
  [sample tile-size]
  (fn [_]
    (when-let [viewer (get @config/*dom-elements :sample-view)]
      (let [img (js/Image.)
            handler (fn []
                      (let [width (cu/clamp 0 img.width config/max-world-pixel-width)
                            height (cu/clamp 0 img.height config/max-world-pixel-height)]
                        (config/clear-error :sample-error)
                        (swap! config/*state assoc :image img :image-width width :image-height height)
                        (set! viewer.width width)
                        (set! viewer.height height)
                        (set-tile-size! tile-size)
                        (cu/draw-grid (.getContext viewer "2d") (* tile-size 2))
                        (editor/draw-tile-picker)
                        (swap! config/*state dissoc :world-state)))
            tile-picker-rect (->> @config/*dom-elements :tile-picker-view .getBoundingClientRect)
            y (+ tile-picker-rect.top js/window.scrollY -10)]
        (js/window.scrollTo #js{:top y :behavior "smooth"})
        (.addEventListener img "load" handler)
        (set! img.src sample)))))

(defn upload
  "Upload handler for loading a new sample from the user."
  [event]
  (when-let [files event.target.files]
    (let [reader (js/FileReader.)]
      (.addEventListener reader "load" draw)
      (.readAsDataURL reader (aget files 0)))))

(defn set-tile-size
  "Setter for the tile size.
  Requires an image to be uploaded.
  Additionally draws a grid with a width of the specified tile size."
  [_]
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
