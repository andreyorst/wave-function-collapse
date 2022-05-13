(ns wfc.input
  (:require [wfc.render :refer [move]]))

(defonce focus false)

(defn on-click [_event]
  (set! focus true))

(defn release-focus []
  (set! focus false))

(defonce valid-key? #{"Escape" "ArrowLeft" "ArrowUp" "ArrowRight" "ArrowDown"})

(defn on-key-press
  "Updates key state when key is pressed."
  [event]
  (when focus
    (let [event (if event event (. js/window -event))
          key (.-key event)]
      (when (valid-key? key)
        (.preventDefault event)
        (case key
          "Escape" (release-focus)
          "ArrowLeft" (move "left")
          "ArrowUp" (move "up")
          "ArrowRight" (move "right")
          "ArrowDown" (move "down")
          nil)))))
