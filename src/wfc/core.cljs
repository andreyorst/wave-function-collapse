(ns wfc.core
  (:require [wfc.render :as render]
            [wfc.sample :as sample]
            [wfc.config :as config]
            [wfc.input :as input]
            [wfc.editor :as editor]
            [wfc.canvas-utils :as cu]))

(defn ^:export init []
  (when-let [sample-view (.getElementById js/document "sample_view")]
    (cu/init-canvas sample-view 32 "click to upload sample"))
  (when-let [render-view (.getElementById js/document "render_view")]
    (cu/init-canvas render-view 32))
  (when-let [sample-input (.getElementById js/document "sample_input")]
    (.addEventListener sample-input "change" sample/upload))
  (when-let [render-button (.getElementById js/document "render")]
    (.addEventListener render-button "click" render/render))
  (when-let [tile-size (.getElementById js/document "set_tile_size")]
    (.addEventListener tile-size "click" sample/set-tile-size))
  (when-let [tile-size (.getElementById js/document "set_world_size")]
    (.addEventListener tile-size "click" render/set-world-size))
  (when-let [tile-view (.getElementById js/document "tile_view")]
    (.addEventListener tile-view "click" editor/get-tile))
  (when-let [tile-view (.getElementById js/document "render_view")]
    (doseq [type ["mousedown" "touchstart"]]
      (.addEventListener tile-view type editor/on-press))
    (doseq [type ["mouseup" "touchend"]]
      (.addEventListener tile-view type editor/on-release)))
  (set! (. js/window -onkeydown) input/on-key-press)
  (doseq [img ["cave"
               "sea_town"
               "purple_basement"
               "desert"
               "desert_cave"
               "water_structures"]]
    (when-let [example (.getElementById js/document img)]
      (.addEventListener example "click" (sample/load-image (str img ".png") 32)))))
