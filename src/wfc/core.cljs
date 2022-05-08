(ns wfc.core
  (:require [wfc.render :as render]
            [wfc.sample :as sample]
            [wfc.config :as config]))

(defn ^:export init []
  (when-let [sample-view (.getElementById js/document "sample_view")]
    (config/init-canvas sample-view 32 "click to upload sample"))
  (when-let [render-view (.getElementById js/document "render_view")]
    (config/init-canvas render-view 32))
  (when-let [sample-input (.getElementById js/document "sample_input")]
    (.addEventListener sample-input "change" sample/upload))
  (when-let [render-button (.getElementById js/document "render")]
    (.addEventListener render-button "click" render/render))
  (when-let [tile-size (.getElementById js/document "set_tile_size")]
    (.addEventListener tile-size "click" sample/set-tile-size))
  (when-let [tile-size (.getElementById js/document "set_world_size")]
    (.addEventListener tile-size "click" render/set-world-size))
  (doseq [img ["cave"
               "sea_town"
               "purple_basement"
               "desert"
               "desert_cave"
               "water_structures"]]
    (when-let [example (.getElementById js/document img)]
      (.addEventListener example "click" (sample/load-image (str img ".png") 32)))))
