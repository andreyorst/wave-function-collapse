(ns wfc.editor
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.impl :as impl]))

(def ^:const MARGIN 2)                      ; px

(defonce *editor
  (atom {:tile-grid {}
         :current-tile-id nil
         :tile-picker nil}))

(defn- draw-tile-grid [ctx tile-size tiles]
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
    (cu/draw-checker-board ctx width height 8)
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

(defn- overlay-tile [event]
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
                (cu/draw ctx tile x y tile-size)))))))))

(defn- remove-overlay []
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (let [ctx (.getContext render-view "2d")
          image (or (:rendered-image @config/*state)
                    (->> (cu/get-image ctx)
                         (swap! config/*state assoc :rendered-image)
                         :rendered-image))]
      (cu/draw ctx image 0 0 render-view.width render-view.height))))

(defn- mark-tile [[x y]]
  (when-let [tile-picker (get @config/*dom-elements :tile-picker-view)]
    (let [ctx (.getContext tile-picker "2d")
          width tile-picker.width
          height tile-picker.height
          size (+ (:tile-size @config/*state) MARGIN)]
      (.clearRect ctx 0 0 width height)
      (cu/draw ctx (:tile-picker @*editor) 0 0 width height)
      (set! ctx.strokeStyle "#dd7777")
      (doto ctx
        .beginPath
        (.rect x y size size)
        .stroke
        .closePath)
      (set! ctx.fillStyle "#dd777755")
      (.fillRect ctx (dec x) (dec y) (inc size) (inc size))
      (when-let [render-view (get @config/*dom-elements :render-view)]
        (swap! config/*state assoc :rendered-image (-> render-view (.getContext "2d") cu/get-image))
        (.addEventListener render-view "mousemove" overlay-tile)
        (.addEventListener render-view "mouseleave" remove-overlay)))))

(defn- set-tile [event]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (when-some [tile-id (:current-tile-id @*editor)]
      (let [ctx (.getContext render-view "2d")
            rect (.getBoundingClientRect render-view)
            {:keys [tile-size tiles world-state]} @config/*state
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
          (swap! config/*state assoc-in [:world-state (/ x tile-size) (/ y tile-size)] (or tile-id nil))
          (if tile-id
            (when-let [tile (get tiles tile-id)]
              (cu/draw ctx tile x y tile-size))
            (let [ctx (.getContext render-view "2d")]
              (cu/draw-checker-board ctx x y tile-size tile-size 16)))
          (swap! config/*state assoc :rendered-image (cu/get-image ctx)))))))

(defn get-tile [event]
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

(defn on-release [_]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (doseq [type ["mousemove" "touchmove"]]
      (.removeEventListener render-view type set-tile))
    (.removeEventListener render-view "mouseleave" on-release)
    (swap! config/*state assoc :rendered-image (cu/get-image (.getContext render-view "2d")))
    (.addEventListener render-view "mousemove" overlay-tile)))

(defn on-press [event]
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (set-tile event)
    (when (= event.type "touchstart")
      (.preventDefault event))
    (doseq [type ["mousemove" "touchmove"]]
      (.addEventListener render-view type set-tile))
    (.removeEventListener render-view "mousemove" overlay-tile)
    (.addEventListener render-view "mouseleave" on-release)))
