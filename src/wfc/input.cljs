(ns wfc.input
  (:require
   [wfc.render :refer [move]]))

(defonce valid-key?
  #{"ArrowLeft" "ArrowUp" "ArrowRight" "ArrowDown"})

(defn on-key-press [event]
  (let [event (or event js/window.event)
        key event.key]
    (when (valid-key? key)
      (.preventDefault event)
      (case key
        "ArrowLeft" (move :left)
        "ArrowUp" (move :up)
        "ArrowRight" (move :right)
        "ArrowDown" (move :down)
        nil))))
