(ns wfc.editor
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.impl :as impl]
   [wfc.render :as render]))

(def ^:const MARGIN 2)                      ; px
(defonce *tile-grid (atom {}))
(defonce *current-tile-id (atom nil))
(defonce *tile-picker (atom nil))

(defn draw-tile-picker []
  (when-let [tile-picker (.getElementById js/document "tile_picker")]
    (let [ctx (.getContext tile-picker "2d")
          size (+ @config/*tile-size MARGIN)
          tiles @config/*tiles
          max-tiles-in-row (Math/floor (/ config/max-world-pixel-width size))
          width (* max-tiles-in-row size)
          max-rows (Math/ceil
                    (/ (inc (count tiles)) ; 1 extra tile for empty brush
                       max-tiles-in-row))
          height (* size max-rows)
          max-width (* max-tiles-in-row size)]
      (set! tile-picker.height height)
      (set! tile-picker.width width)
      (reset! *tile-grid {})
      (reset! *current-tile-id nil)
      (cu/draw-checker-board tile-picker width height (/ size 4))
      (loop [tiles (->> tiles (sort-by first))
             x 0 y 0]
        (when (seq tiles)
          (if (< x max-width)
            (let [[tile & tiles] tiles]
              (cu/draw tile-picker (second tile) (+ x (/ MARGIN 2)) (+ y (/ MARGIN 2)) (- size MARGIN))
              (swap! *tile-grid assoc [x y] (first tile))
              (recur tiles (+ x size) y))
            (recur tiles 0 (+ y size)))))
      (cu/draw-grid tile-picker size :solid false)
      (set! ctx.lineWidth 2)
      (doto ctx
        .beginPath
        (.moveTo 1 1)
        (.lineTo 1 (dec height))
        (.lineTo (dec width) (dec height))
        (.lineTo (dec width) 1)
        (.lineTo 1 1)
        .stroke
        .closePath)
      (reset! *tile-picker (.getImageData ctx 0 0 tile-picker.width tile-picker.height)))))

(defn hide-tile-picker []
  (when-let [tile-picker (.getElementById js/document "tile_picker")]
    (set! tile-picker.height "0px")))

(defn snap-to-grid [x y size]
  (let [x (* (Math/floor (/ x size)) size)
        y (* (Math/floor (/ y size)) size)]
    [x y]))

(defn mark-tile [[x y]]
  (when-let [tile-picker (.getElementById js/document "tile_picker")]
    (let [ctx (.getContext tile-picker "2d")
          width tile-picker.width
          height tile-picker.height
          size (+ @config/*tile-size MARGIN)
          arr (js/Uint8ClampedArray. (.-data @*tile-picker))
          place (.getImageData ctx 0 0 width height)]
      (.clearRect ctx 0 0 width height)
      (.set place.data arr)
      (.putImageData ctx place 0 0)
      (set! ctx.strokeStyle "#dd7777")
      (doto ctx
        .beginPath
        (.moveTo x y)
        (.lineTo x (+ y size))
        (.lineTo (+ x size) (+ y size))
        (.lineTo (+ x size) y)
        (.lineTo x y)
        (.stroke)
        .closePath)
      (set! ctx.fillStyle "#dd777755")
      (.fillRect ctx (dec x) (dec y) (inc size) (inc size)))))

(defn get-tile [event]
  (when-let [tile-view (.getElementById js/document "tile_picker")]
    (let [rect (.getBoundingClientRect tile-view)
          x (- event.clientX rect.left)
          y (- event.clientY rect.top)
          pos (snap-to-grid x y (+ @config/*tile-size MARGIN))
          tile-id (get @*tile-grid pos false)]
      (mark-tile pos)
      (reset! *current-tile-id tile-id))))

(defn set-tile [event]
  (when-let [render-view (.getElementById js/document "render_view")]
    (when-some [tile-id @*current-tile-id]
      (let [rect (.getBoundingClientRect render-view)
            size @config/*tile-size
            [x y] (case event.type
                    ("mousedown" "mousemove") [event.clientX event.clientY]
                    "touchstart" [(.-clientX (aget event.touches 0))
                                  (.-clientY (aget event.touches 0))]
                    "touchmove" [(.-clientX (aget event.changedTouches 0))
                                 (.-clientY (aget event.changedTouches 0))])
            [x y] (snap-to-grid (- x rect.left) (- y rect.top) @config/*tile-size)]
        (when (and (<= 0 x render-view.width)
                   (<= 0 y render-view.height))
          (when-not @render/*world-state
            (reset! render/*world-state
                    (impl/gen-world (/ render-view.width size) (/ render-view.height size))))
          (swap! render/*world-state assoc-in [(/ x size) (/ y size)] (or tile-id nil))
          (if tile-id
            (when-let [tile (get @config/*tiles tile-id)]
              (cu/draw render-view tile x y size))
            (let [ctx (.getContext render-view "2d")]
              (set! (. ctx -fillStyle)
                    (if (or (and (odd? (/ x 32)) (even? (/ y 32)))
                            (and (even? (/ x 32)) (odd? (/ y 32)))) cu/canvas-base
                        cu/canvas-check))
              (.fillRect ctx x y size size))))))))

(defn on-release [_]
  (when-let [render-view (.getElementById js/document "render_view")]
    (doseq [type ["mousemove" "touchmove"]]
      (.removeEventListener render-view type set-tile))
    (.removeEventListener render-view "mouseleave" on-release)))

(defn on-press [event]
  (when-let [render-view (.getElementById js/document "render_view")]
    (set-tile event)
    (when (= event.type "touchstart")
      (.preventDefault event))
    (doseq [type ["mousemove" "touchmove"]]
      (.addEventListener render-view type set-tile))
    (.addEventListener render-view "mouseleave" on-release)))
