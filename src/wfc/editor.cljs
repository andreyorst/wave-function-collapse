(ns wfc.editor
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.config :as config]
   [wfc.impl :as impl]))

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
        (.rect 0 0 width height)
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

(defn overlay-tile [event]
  (when-let [render-view (.getElementById js/document "render_view")]
    (when-some [tile-id @*current-tile-id]
      (let [rect (.getBoundingClientRect render-view)
            size @config/*tile-size
            [x y] (snap-to-grid (- event.clientX rect.left)
                                (- event.clientY rect.top)
                                @config/*tile-size)]
        (when (and (<= 0 x render-view.width)
                   (<= 0 y render-view.height))
          (when tile-id
            (when-let [tile (get @config/*tiles tile-id)]
              (when-not (some? @config/*rendered-image)
                (reset! config/*rendered-image (cu/get-image render-view)))
              (cu/draw render-view
                       @config/*rendered-image
                       0 0 render-view.width render-view.height)
              (cu/draw render-view tile x y size))))))))

(defn remove-overlay []
  (when-let [render-view (.getElementById js/document "render_view")]
    (when-not (some? @config/*rendered-image)
      (reset! config/*rendered-image (cu/get-image render-view)))
    (cu/draw render-view @config/*rendered-image 0 0 render-view.width render-view.height)))

(defn mark-tile [[x y]]
  (when-let [tile-picker (.getElementById js/document "tile_picker")]
    (let [ctx (.getContext tile-picker "2d")
          width tile-picker.width
          height tile-picker.height
          size (+ @config/*tile-size MARGIN)]
      (.clearRect ctx 0 0 width height)
      (cu/draw tile-picker @*tile-picker 0 0 width height)
      (set! ctx.strokeStyle "#dd7777")
      (doto ctx
        .beginPath
        (.rect x y size size)
        .stroke
        .closePath)
      (set! ctx.fillStyle "#dd777755")
      (.fillRect ctx (dec x) (dec y) (inc size) (inc size))
      (when-let [render-view (.getElementById js/document "render_view")]
        (reset! config/*rendered-image (cu/get-image render-view))
        (.addEventListener render-view "mousemove" overlay-tile)
        (.addEventListener render-view "mouseleave" remove-overlay)))))

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
          (when-not @config/*world-state
            (reset! config/*world-state
                    (impl/gen-world (/ render-view.width size) (/ render-view.height size))))
          (swap! config/*world-state assoc-in [(/ x size) (/ y size)] (or tile-id nil))
          (if tile-id
            (when-let [tile (get @config/*tiles tile-id)]
              (cu/draw render-view tile x y size))
            (let [ctx (.getContext render-view "2d")]
              (set! (. ctx -fillStyle)
                    (if (or (and (odd? (/ x size)) (even? (/ y size)))
                            (and (even? (/ x size)) (odd? (/ y size)))) cu/canvas-base
                        cu/canvas-check))
              (.fillRect ctx x y size size)))
          (reset! config/*rendered-image (cu/get-image render-view)))))))

(defn on-release [_]
  (when-let [render-view (.getElementById js/document "render_view")]
    (doseq [type ["mousemove" "touchmove"]]
      (.removeEventListener render-view type set-tile))
    (.removeEventListener render-view "mouseleave" on-release)
    (reset! config/*rendered-image (cu/get-image render-view))
    (.addEventListener render-view "mousemove" overlay-tile)))

(defn on-press [event]
  (when-let [render-view (.getElementById js/document "render_view")]
    (set-tile event)
    (when (= event.type "touchstart")
      (.preventDefault event))
    (doseq [type ["mousemove" "touchmove"]]
      (.addEventListener render-view type set-tile))
    (.removeEventListener render-view "mousemove" overlay-tile)
    (.addEventListener render-view "mouseleave" on-release)))
