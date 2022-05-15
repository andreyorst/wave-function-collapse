(ns wfc.core
  (:require
   [wfc.canvas-utils :as cu]
   [wfc.editor :as editor]
   [wfc.input :as input]
   [wfc.render :as render]
   [wfc.sample :as sample]
   [wfc.config :as config]))

(defonce examples
  {"cave" (sample/load-image "cave.png" 16)
   "sea_town" (sample/load-image "sea_town.png" 16)
   "purple_basement" (sample/load-image "purple_basement.png" 16)
   "desert" (sample/load-image "desert.png" 16)
   "desert_cave" (sample/load-image "desert_cave.png" 16)
   "water_structures" (sample/load-image "water_structures.png" 16)})

(defonce elements-events
  {:sample-view {}
   :sample-input {"change" sample/upload}
   :render-button {"click" render/render}
   :tile-size-button {"click" sample/set-tile-size}
   :world-size-button {"click" render/set-world-size}
   :clear-button {"click" render/clear-render-view}
   :tile-picker-view {"click" editor/get-tile}
   :render-view {["mousedown" "touchstart"] editor/on-press
                 ["mouseup" "touchend"] editor/on-release}
   :tile-size-input {}
   :world-width-input {}
   :world-height-input {}
   :render-error {}
   :sample-error {}})

(defn init-element [id hooks]
  (when-let [element (.getElementById js/document (name id))]
    (swap! config/*dom-elements assoc id element)
    (doseq [[events callback] hooks]
      (doseq [event (if (coll? events) events [events])]
        (.addEventListener element event callback)))))

(defn deinit-element [id]
  (when-let [element (get @config/*dom-elements id)]
    (swap! config/*dom-elements dissoc element)
    (doseq [[types listener] (get elements-events id)]
      (doseq [type (if (coll? types) types [types])]
        (.removeEventListener element type listener)))))

(defn ^:dev/after-load init-elements []
  (doseq [[id events] elements-events]
    (init-element id events))
  (set! (.-onkeydown js/window) input/on-key-press)
  (doseq [[img loader] examples]
    (when-let [example (.getElementById js/document img)]
      (.addEventListener example "click" loader))))

(defn ^:dev/before-load deinit-elements []
  (doseq [id (keys elements-events)]
    (deinit-element id))
  (doseq [[img loader] examples]
    (when-let [example (.getElementById js/document img)]
      (.removeEventListener example "click" loader))))

(defn ^:export init []
  (init-elements)
  (when-let [sample-view (get @config/*dom-elements :sample-view)]
    (cu/init-canvas sample-view "click to upload sample"))
  (when-let [render-view (get @config/*dom-elements :render-view)]
    (cu/init-canvas render-view)))
