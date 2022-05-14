(ns wfc.editor
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.impl :as impl]
   [wfc.render :as render]))

(defonce margin 2)                      ; px
(defonce tile-grid (atom {}))
(defonce current-tile-id (atom nil))
(defonce tile-picker (atom nil))

(defn draw-tile-picker []
  (when-let [tile-viewer (.getElementById js/document "tile_view")]
    (let [ctx (.getContext tile-viewer "2d")
          size (+ @config/tile-size margin)
          tiles @config/tiles
          max-tiles-in-row (Math/floor (/ config/max-world-pixel-width size))
          width (* max-tiles-in-row size)
          max-rows (Math/ceil
                    (/ (inc (count tiles)) ; 1 extra tile for empty brush
                       max-tiles-in-row))
          height (* size max-rows)
          max-width (* max-tiles-in-row size)]
      (set! tile-viewer.height height)
      (set! tile-viewer.width width)
      (reset! tile-grid {})
      (reset! current-tile-id nil)
      (cu/draw-checker-board ctx width height (/ size 4))
      (loop [tiles (->> tiles (sort-by first))
             x 0 y 0]
        (when (seq tiles)
          (if (< x max-width)
            (let [[tile & tiles] tiles]
              (cu/draw ctx (second tile) (+ x (/ margin 2)) (+ y (/ margin 2)) (- size margin))
              (swap! tile-grid assoc [x y] (first tile))
              (recur tiles (+ x size) y))
            (recur tiles 0 (+ y size)))))
      (set! ctx.lineWidth 2)
      (cu/draw-grid tile-viewer size :solid false)
      (doto ctx
        .beginPath
        (.moveTo 1 1)
        (.lineTo 1 (dec height))
        (.lineTo (dec width) (dec height))
        (.lineTo (dec width) 1)
        (.lineTo 1 1)
        (.stroke)
        .closePath)
      (reset! tile-picker (.getImageData ctx 0 0 tile-viewer.width tile-viewer.height)))))

(defn hide-tile-picker []
  (when-let [tile-viewer (.getElementById js/document "tile_view")]
    (set! tile-viewer.height "0px")))

(defn snap-to-grid [x y size]
  (let [x (* (Math/floor (/ x size)) size)
        y (* (Math/floor (/ y size)) size)]
    [x y]))

(defn mark-tile [[x y]]
  (when-let [tile-viewer (.getElementById js/document "tile_view")]
    (let [ctx (.getContext tile-viewer "2d")
          width tile-viewer.width
          height tile-viewer.height
          size (+ @config/tile-size margin)
          arr (js/Uint8ClampedArray. (.-data @tile-picker))
          place (.getImageData ctx 0 0 width height)]
      (.clearRect ctx 0 0 width height)
      (.set place.data arr)
      (.putImageData ctx place 0 0)
      (let [x x
            y y
            size size]
        (set! ctx.strokeStyle "#dd7777")
        (doto ctx
          .beginPath
          (.moveTo x y)
          (.lineTo x (+ y size))
          (.lineTo (+ x size) (+ y size))
          (.lineTo (+ x size) y)
          (.lineTo x y)
          (.stroke)
          .closePath))
      (set! ctx.fillStyle "#dd777755")
      (.fillRect ctx (dec x) (dec y) (inc size) (inc size)))))

(defn get-tile [event]
  (when-let [tile-view (.getElementById js/document "tile_view")]
    (let [rect (.getBoundingClientRect tile-view)
          x (- event.clientX rect.left)
          y (- event.clientY rect.top)
          pos (snap-to-grid x y (+ @config/tile-size margin))
          tile-id (get @tile-grid pos false)]
      (mark-tile pos)
      (reset! current-tile-id tile-id))))

(defn set-tile [event]
  (when-let [render-view (.getElementById js/document "render_view")]
    (when-some [tile-id @current-tile-id]
      (let [rect (.getBoundingClientRect render-view)
            size @config/tile-size
            [x y] (case event.type
                    ("mousedown" "mousemove") [event.clientX event.clientY]
                    "touchstart" [(.-clientX (aget event.touches 0))
                                  (.-clientY (aget event.touches 0))]
                    "touchmove" [(.-clientX (aget event.changedTouches 0))
                                 (.-clientY (aget event.changedTouches 0))])
            [x y] (snap-to-grid (- x rect.left) (- y rect.top) @config/tile-size)]
        (when (and (<= 0 x render-view.width)
                   (<= 0 y render-view.height))
          (when-not @render/world-state
            (reset! render/world-state
                    (impl/gen-world (/ render-view.width size) (/ render-view.height size))))
          (swap! render/world-state assoc-in [(/ x size) (/ y size)] (or tile-id nil))
          (if tile-id
            (when-let [tile (get @config/tiles tile-id)]
              (cu/draw (.getContext render-view "2d") tile x y size))
            (let [ctx (.getContext render-view "2d")]
              (set! (. ctx -fillStyle)
                    (if (or (and (odd? (/ x 32)) (even? (/ y 32)))
                            (and (even? (/ x 32)) (odd? (/ y 32)))) cu/canvas-base
                        cu/canvas-check))
              (.fillRect ctx x y size size))))))))

(defn on-press [event]
  (when-let [render-view (.getElementById js/document "render_view")]
    (set-tile event)
    (when (= event.type "touchstart")
      (.preventDefault event))
    (doseq [type ["mousemove" "touchmove"]]
      (.addEventListener render-view type set-tile))))

(defn on-release [_]
  (when-let [render-view (.getElementById js/document "render_view")]
    (doseq [type ["mousemove" "touchmove"]]
      (.removeEventListener render-view type set-tile))))
