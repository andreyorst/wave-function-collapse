(ns wfc.editor
  "Tile editor for the renderer."
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.impl :as impl]))

(def ^:const MARGIN 2)                      ; px

(defonce *editor
  (atom {:tile-grid {}
         :current-tile-id nil
         :tile-picker nil}))

(defn- draw-tile-grid
  "Draws the grid of tiles for a tile picker."
  [ctx tile-size tiles]
  (let [grid-color (if config/dark-mode? "#1e1e1e" "#fbfcfd")
        size (+ tile-size MARGIN)
        max-tiles-in-row (Math/floor (/ config/max-world-pixel-width size))
        width (* max-tiles-in-row size)
        max-rows (Math/ceil
                  (/ (inc (count tiles)) ; 1 extra tile for empty brush
                     max-tiles-in-row))
        height (* size max-rows)
        max-width (* max-tiles-in-row size)]
    (set! ctx.canvas.height height)
    (set! ctx.canvas.width width)
    (cu/draw-checkerboard ctx width height 8)
    (set! ctx.fillStyle grid-color)
    (loop [tiles (->> tiles (sort-by first))
           x 0 y 0
           tile-grid {}]
      (if (seq tiles)
        (if (< x max-width)
          (let [[tile & tiles] tiles]
            (.fillRect ctx x y size size)
            (cu/draw ctx (second tile)
                     (+ x (/ MARGIN 2)) (+ y (/ MARGIN 2))
                     (- size MARGIN))
            (recur tiles
                   (+ x size) y
                   (assoc tile-grid [x y] (first tile))))
          (recur tiles 0 (+ y size) tile-grid))
        (swap! *editor assoc :tile-grid tile-grid)))
    (cu/draw-grid ctx size :solid grid-color false)
    (set! ctx.lineWidth 2)
    (set! ctx.strokeStyle grid-color)
    (doto ctx
      .beginPath
      (.rect 0 0 width height)
      .stroke
      .closePath)))

(defn- snap-to-grid [x y size]
  (let [x (* (Math/floor (/ x size)) size)
        y (* (Math/floor (/ y size)) size)]
    [x y]))

(defn- overlay-tile
  "Overlays currently selected tile on the renderer, and keeps track of
  not overriding the image."
  [event]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (when-some [tile-id (:current-tile-id @*editor)]
      (let [ctx (.getContext render-view "2d")
            rect (.getBoundingClientRect render-view)
            {:keys [tile-size rendered-image tiles]} @config/*state
            [x y] (snap-to-grid (- event.clientX rect.left)
                                (- event.clientY rect.top)
                                tile-size)]
        (when (and (<= 0 x render-view.width)
                   (<= 0 y render-view.height))
          (when tile-id
            (when-let [tile (get tiles tile-id)]
              (let [image (or rendered-image
                              (->> (cu/get-image ctx)
                                   (swap! config/*state assoc :rendered-image)
                                   :rendered-image))]
                (cu/draw ctx image
                         0 0 render-view.width render-view.height)
                (cu/draw ctx tile x y tile-size)
                (set! ctx.fillStyle "#6c8cd655")
                (set! ctx.strokeStyle "#6c8cd6")
                (set! ctx.lineWidth 2)
                (doto ctx
                  .beginPath
                  (.fillRect x y tile-size tile-size)
                  (.rect x y tile-size tile-size)
                  .stroke)))))))))

(defn- remove-overlay
  "Does the opposite of `overlay-tile`."
  []
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (let [ctx (.getContext render-view "2d")
          image (or (:rendered-image @config/*state)
                    (->> (cu/get-image ctx)
                         (swap! config/*state assoc :rendered-image)
                         :rendered-image))]
      (cu/draw ctx image 0 0 render-view.width render-view.height))))

(defn- mark-tile
  "Marks the tile on the tile-picker as selected, and sets the tile id
  in editor's state."
  [[x y]]
  (when-let [tile-picker (get @config/*dom-elements :tile-picker-view)]
    (let [ctx (.getContext tile-picker "2d")
          width tile-picker.width
          height tile-picker.height
          size (+ (:tile-size @config/*state) MARGIN)]
      (.clearRect ctx 0 0 width height)
      (cu/draw ctx (:tile-picker @*editor) 0 0 width height)
      (set! ctx.fillStyle "#6c8cd655")
      (set! ctx.strokeStyle "#6c8cd6")
      (set! ctx.lineWidth 2)
      (doto ctx
        .beginPath
        (.rect x y size size)
        (.fillRect (dec x) (dec y) (inc size) (inc size))
        .stroke
        .closePath)
      (when-let [render-view (get @config/*dom-elements :render-view)]
        (swap! config/*state assoc :rendered-image (-> render-view (.getContext "2d") cu/get-image))
        (.addEventListener render-view "mousemove" overlay-tile)
        (.addEventListener render-view "mouseleave" remove-overlay)))))

(defn- set-tile
  "Draws the tile on the renderer and adds the tile to the world state."
  [event]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (when-some [tile-id (:current-tile-id @*editor)]
      (let [ctx (.getContext render-view "2d")
            rect (.getBoundingClientRect render-view)
            {:keys [tile-size tiles world-state rendered-image]} @config/*state
            [x y] (case event.type
                    ("mousedown" "mousemove") [event.clientX event.clientY]
                    "touchstart" [(.-clientX (aget event.touches 0))
                                  (.-clientY (aget event.touches 0))]
                    "touchmove" [(.-clientX (aget event.changedTouches 0))
                                 (.-clientY (aget event.changedTouches 0))])
            [x y] (snap-to-grid (- x rect.left) (- y rect.top) tile-size)]
        (when (and (<= 0 x render-view.width)
                   (<= 0 y render-view.height))
          (when-not world-state
            (swap! config/*state assoc :world-state
                   (impl/gen-world (/ render-view.width tile-size) (/ render-view.height tile-size))))
          (swap! config/*state assoc-in [:world-state (/ y tile-size) (/ x tile-size)] (or tile-id nil))
          (if tile-id
            (when-let [tile (get tiles tile-id)]
              (cu/draw ctx rendered-image 0 0 render-view.width render-view.height)
              (cu/draw ctx tile x y tile-size))
            (let [ctx (.getContext render-view "2d")]
              (cu/draw-checkerboard ctx x y tile-size tile-size 16)))
          (swap! config/*state assoc :rendered-image (cu/get-image ctx)))))))

(defn get-tile
  "Extracts the coordinates from the tile picker and finds the tile id
  in editor's state."
  [event]
  (when-let [tile-picker (get @config/*dom-elements :tile-picker-view)]
    (let [rect (.getBoundingClientRect tile-picker)
          x (- event.clientX rect.left)
          y (- event.clientY rect.top)
          pos (snap-to-grid x y (+ (:tile-size @config/*state) MARGIN))
          tile-id (get (:tile-grid @*editor) pos false)]
      (mark-tile pos)
      (swap! *editor assoc :current-tile-id tile-id))))

(defn draw-tile-picker []
  (when-let [tile-picker (get @config/*dom-elements :tile-picker-view)]
    (let [{:keys [tile-size tiles]} @config/*state
          ctx (.getContext tile-picker "2d")]
      (swap! *editor assoc
             :tile-grid {}
             :current-tile-id nil)
      (draw-tile-grid ctx tile-size tiles)
      (swap! *editor assoc :tile-picker (cu/get-image ctx)))))

(defn hide-tile-picker []
  (when-let [tile-picker (get @config/*dom-elements :tile-picker-view)]
    (set! tile-picker.height "0px")))

(defn on-release
  "Handles mouse and touch release events, overlaying a tile from the
  tile picker, and updating current state of the editor."
  [event]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (doseq [type ["mousemove" "touchmove"]]
      (.removeEventListener render-view type set-tile))
    (.removeEventListener render-view "mouseleave" on-release)
    (swap! config/*state assoc :rendered-image (cu/get-image (.getContext render-view "2d")))
    (.addEventListener render-view "mousemove" overlay-tile)
    (when-not (= event.type "mouseleave")
      (overlay-tile event))))

(defn on-press
  "Handles initial mouse and touch down events, setting currently
  selected tile, and all required events for further processing of the
  movement."
  [event]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (set-tile event)
    (when (= event.type "touchstart")
      (.preventDefault event))
    (doseq [type ["mousemove" "touchmove"]]
      (.addEventListener render-view type set-tile))
    (.removeEventListener render-view "mousemove" overlay-tile)
    (.addEventListener render-view "mouseleave" on-release)))
