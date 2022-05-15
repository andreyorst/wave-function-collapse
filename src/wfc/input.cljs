(ns wfc.input
  (:require
   [wfc.render :refer [move]]
   [wfc.config :as config]))

(defonce valid-key?
  #{"ArrowLeft" "ArrowUp" "ArrowRight" "ArrowDown"})

(defn on-key-press [event]
  (let [event (or event js/window.event)
        key event.key]
    (when (and (valid-key? key)
               (:move? @config/*state))
      (.preventDefault event)
      (case key
        "ArrowLeft" (move :left)
        "ArrowUp" (move :up)
        "ArrowRight" (move :right)
        "ArrowDown" (move :down)
        nil))))
