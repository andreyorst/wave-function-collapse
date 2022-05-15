(ns wfc.config)

;; DOM elements
(defonce *dom-elements
  (atom {}))

;; Internal state
(defonce *state (atom {:sample []
                       :tiles {}
                       :world-state nil
                       :rendered-image nil
                       :move? false
                       :image nil
                       :image-width 0
                       :image-height 0}))

(defonce dark-mode?
  (.-matches (js/window.matchMedia "(prefers-color-scheme: dark)")))

(defonce max-world-pixel-height
  ;; Make sure that the output is divisable by 32 to make all samples work on mobile
  (if (.-matches (js/window.matchMedia "(max-width: 552px)"))
    (-> js/document.documentElement.clientWidth
        (- 40)                          ; canvas element padding
        (/ 32)
        Math/floor
        (* 32))
    512))

(defonce max-world-pixel-width max-world-pixel-height)

(defn get-text-input-value [id]
  (when-let [tile-size (get @*dom-elements id)]
    (let [value (js/parseInt tile-size.value)]
      (if (js/isNaN value)
        nil
        value))))

(defn display-error [error-class msg]
  (when-let [box (get @*dom-elements error-class)]
    (set! box.innerHTML msg)
    (let [style box.style]
      (set! style.opacity 1)
      (set! style.height "42px")
      (set! style.marginBottom "10px"))))

(defn clear-error [error-class]
  (when-let [box (get @*dom-elements error-class)]
    (let [style box.style]
      (set! style.opacity 0)
      (set! style.height 0)
      (set! style.marginBottom "0px"))))
