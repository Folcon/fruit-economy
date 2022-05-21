(ns fruit-economy.ui.controls
  (:require [fruit-economy.state :as state])
  (:import [io.github.humbleui.skija Paint Font Typeface]))


(defn on-key-pressed-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @state/*state
        move (fn [[x1 y1]] (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (let [{:keys [camera cell half-vw half-vh]} state
            [camera-x camera-y] camera
            [screen-x screen-y] ((juxt :x :y) (:hui.event/pos raw-event))
            loc-x (+ (int (- (quot screen-x cell) half-vw)) camera-x)
            loc-y (+ (int (- (quot screen-y cell) half-vh)) camera-y)]
        (swap! state/*state assoc :hovering {:world [loc-x loc-y] :screen [screen-x screen-y]})))

    (when (= event :hui/mouse-button)
      (println :panel raw-event)
      (println (:hovering state)))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :panel key)
      (println (:peep @state/*state))
      (condp contains? key
        #{:d :right}
        (doto state/*state
          (swap! update :peep (move [1 0]))
          (swap! update :camera (move [1 0])))

        #{:a :left}
        (doto state/*state
          (swap! update :peep (move [-1 0]))
          (swap! update :camera (move [-1 0])))

        #{:s :down}
        (doto state/*state
          (swap! update :peep (move [0 1]))
          (swap! update :camera (move [0 1])))

        #{:w :up}
        (doto state/*state
          (swap! update :peep (move [0 -1]))
          (swap! update :camera (move [0 -1])))

        #{:minus}
        (let [{:keys [scale zoom init-cell canvas-width canvas-height]} state
              zoom' (max 0.2 (- zoom 0.2))
              cell' (long (/ (* init-cell zoom') scale))
              viewport-width' (inc (quot canvas-width cell'))
              viewport-height' (inc (quot canvas-height cell'))
              half-vw' (quot viewport-width' 2)
              half-vh' (quot viewport-height' 2)]
          (swap! state/*state assoc :zoom zoom' :cell cell' :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh'))

        #{:equals}
        (let [{:keys [scale zoom init-cell canvas-width canvas-height]} state
              zoom' (min 5 (+ zoom 0.2))
              cell' (long (/ (* init-cell zoom') scale))
              viewport-width' (inc (quot canvas-width cell'))
              viewport-height' (inc (quot canvas-height cell'))
              half-vw' (quot viewport-width' 2)
              half-vh' (quot viewport-height' 2)]
          (swap! state/*state assoc :zoom zoom' :cell cell' :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh'))

        ;; (println :panel key)
        nil))))
