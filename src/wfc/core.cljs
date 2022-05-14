(ns wfc.core
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.editor :as editor]
   [wfc.input :as input]
   [wfc.render :as render]
   [wfc.sample :as sample]))

(defonce examples
  {"cave" (sample/load-image "cave.png" 32)
   "sea_town" (sample/load-image "sea_town.png" 32)
   "purple_basement" (sample/load-image "purple_basement.png" 32)
   "desert" (sample/load-image "desert.png" 32)
   "desert_cave" (sample/load-image "desert_cave.png" 32)
   "water_structures" (sample/load-image "water_structures.png" 32)})

(defn setup-handlers []
  (when-let [sample-input (.getElementById js/document "sample_input")]
    (.addEventListener sample-input "change" sample/upload))
  (when-let [render-button (.getElementById js/document "render")]
    (.addEventListener render-button "click" render/render))
  (when-let [tile-size (.getElementById js/document "set_tile_size")]
    (.addEventListener tile-size "click" sample/set-tile-size))
  (when-let [world-size (.getElementById js/document "set_world_size")]
    (.addEventListener world-size "click" render/set-world-size))
  (when-let [tile-picker (.getElementById js/document "tile_picker")]
    (.addEventListener tile-picker "click" editor/get-tile))
  (when-let [render-view (.getElementById js/document "render_view")]
    (doseq [type ["mousedown" "touchstart"]]
      (.addEventListener render-view type editor/on-press))
    (doseq [type ["mouseup" "touchend"]]
      (.addEventListener render-view type editor/on-release)))
  (set! (.-onkeydown js/window) input/on-key-press)
  (doseq [[img loader] examples]
    (when-let [example (.getElementById js/document img)]
      (.addEventListener example "click" loader))))

(defn remove-handlers []
  (when-let [sample-input (.getElementById js/document "sample_input")]
    (.removeEventListener sample-input "change" sample/upload))
  (when-let [render-button (.getElementById js/document "render")]
    (.removeEventListener render-button "click" render/render))
  (when-let [tile-size (.getElementById js/document "set_tile_size")]
    (.removeEventListener tile-size "click" sample/set-tile-size))
  (when-let [world-size (.getElementById js/document "set_world_size")]
    (.removeEventListener world-size "click" render/set-world-size))
  (when-let [tile-picker (.getElementById js/document "tile_picker")]
    (.removeEventListener tile-picker "click" editor/get-tile))
  (when-let [render-view (.getElementById js/document "render_view")]
    (doseq [type ["mousedown" "touchstart"]]
      (.removeEventListener render-view type editor/on-press))
    (doseq [type ["mouseup" "touchend"]]
      (.removeEventListener render-view type editor/on-release)))
  (doseq [[img loader] examples]
    (when-let [example (.getElementById js/document img)]
      (.removeEventListener example "click" loader))))

(defn ^:export init []
  (when-let [sample-view (.getElementById js/document "sample_view")]
    (cu/init-canvas sample-view 32 "click to upload sample"))
  (when-let [render-view (.getElementById js/document "render_view")]
    (cu/init-canvas render-view 32))
  (setup-handlers))
