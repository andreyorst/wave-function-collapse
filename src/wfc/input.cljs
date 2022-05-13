(ns wfc.input
  (:require [wfc.render :refer [move]]))

(defonce focus (atom false))

(defn grab-focus! [_]
  (reset! focus true))

(defn release-focus! []
  (reset! focus false))

(defonce valid-key?
  #{"Escape" "ArrowLeft" "ArrowUp" "ArrowRight" "ArrowDown"})

(defn on-key-press [event]
  (when @focus
    (let [event (or event js/window.event)
          key event.key]
      (when (valid-key? key)
        (.preventDefault event)
        (case key
          "Escape" (release-focus!)
          "ArrowLeft" (move :left)
          "ArrowUp" (move :up)
          "ArrowRight" (move :right)
          "ArrowDown" (move :down)
          nil)))))
