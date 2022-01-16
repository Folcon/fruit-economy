(ns fruit-economy.core
  (:require
   [clojure.string :as str]
   [clojure.stacktrace :as stacktrace]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [nrepl.cmdline :as nrepl])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventKey KeyModifier Window]
   [io.github.humbleui.skija Canvas Color4f FontMgr FontStyle Typeface Font Paint ClipMode]
   [io.github.humbleui.types IPoint Rect]))


;; Should be in io.github.humbleui.ui
(def *broken (atom false))

(defrecord UICanvas [width height on-paint on-event]
  ui/IComponent
  (-layout [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx canvas]
    (when on-paint
      (let [canvas ^Canvas canvas
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (.clipRect canvas rect ClipMode/INTERSECT true)
          (try
            (when-not @*broken
              (on-paint canvas width height))
            (catch Exception e
              (reset! *broken true)
              (stacktrace/print-stack-trace (stacktrace/root-cause e))))
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]
    (when on-event
      (try
        (when-not @*broken
          (on-event event))
        (catch Exception e
          (reset! *broken true)
          (stacktrace/print-stack-trace (stacktrace/root-cause e)))))))

(defn ui-canvas
  "(ui-canvas 400 300 {:on-paint #'on-paint-impl
                       :on-event #'on-event-impl})"
  [width height {:keys [on-paint on-event]}]
  (UICanvas. width height on-paint on-event))
;; END Should be in io.github.humbleui.ui

(set! *warn-on-reflection* true)

(defonce font-mgr (FontMgr/getDefault))

;; GAME STATE
(defn new-state []
  (let []
    {:peep [10 10]}))

(defonce *state (atom (new-state)))
;; END GAME STATE

(defonce *window (atom nil))

(defonce ^Typeface face-default
  (.matchFamiliesStyle ^FontMgr font-mgr (into-array String [".SF NS", "Helvetica Neue", "Arial"]) FontStyle/NORMAL))

(defonce *clicks (atom 0))
#_
(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-default        (Font. face-default (float (* 13 scale)))
          leading             (.getCapHeight (.getMetrics font-default))
          fill-text           (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
          fill-button-normal  (doto (Paint.) (.setColor (unchecked-int 0xFFade8f4)))
          fill-button-hovered (doto (Paint.) (.setColor (unchecked-int 0xFFcaf0f8)))
          fill-button-active  (doto (Paint.) (.setColor (unchecked-int 0xFF48cae4)))]
      (ui/column
        (ui/row
          (ui/label "Top Bar" font-default fill-text))
        (ui/row
          (ui/valign 0.1
            (ui/column
              (ui/label "Left Sidebar" font-default fill-text)))
          (ui/valign 0.5
            (ui/halign 0.4
              (ui/column
                (ui/label "Hello from Humble UI! 👋" font-default fill-text)
                (ui/gap 0 leading)
                (ui/dynamic _ [clicks @*clicks]
                  (ui/label (str "Clicked: " clicks) font-default fill-text))
                (ui/gap 0 leading)
                (ui/clickable
                  #(swap! *clicks inc)
                  (ui/clip-rrect (* scale 4)
                    (ui/dynamic ctx [active?  (:hui/active? ctx)
                                     hovered? (:hui/hovered? ctx)]
                      (let [[label fill] (cond
                                           active?  ["Active"    fill-button-active]
                                           hovered? ["Hovered"   fill-button-hovered]
                                           :else    ["Unpressed" fill-button-normal])]
                        (ui/fill fill
                          (ui/padding (* scale 20) leading
                            (ui/label label font-default fill-text))))))))))
          (ui/valign 0.1
            (ui/halign 1.2
              (ui/column
                (ui/label "Right Sidebar" font-default fill-text))))
          ;; Not currently visible, should work out what the layout system is
          (ui/row
            (ui/label "Bottom Bar" font-default fill-text)))))))

(defn draw-impl [^Canvas canvas window-width window-height]
  (let [{:keys [peep] :as state} @*state]
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF33CC33)))]
      (.drawRect canvas (Rect/makeXYWH (first peep) (second peep) 10 10) fill))))

(defn on-key-pressed-impl [{event-type :hui/event :hui.event.key/keys [key pressed?] :as event}]
  (let [state @*state
        move (fn [[x1 y1]] (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))]
    (when (and (= event-type :hui/key) pressed?)
      (println key)
      (println (:peep @*state))
      (condp = key
        :d ;; right
        (swap! *state update :peep (move [1 0]))

        :a ;; left
        (swap! *state update :peep (move [-1 0]))

        :s ;; bottom
        (swap! *state update :peep (move [0 1]))

        :w ;; up
        (swap! *state update :peep (move [0 -1]))

        :r ;; R
        (reset! *state (new-state))

        ;; (println key)
        nil))))

(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-default        (Font. face-default (float (* 24 scale)))
          fill-text           (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/valign 0.5
        (ui/halign 0.5
          (ui/column
            (ui-canvas 400 300 {:on-paint #'draw-impl
                                :on-event #'on-key-pressed-impl})
            (ui/label "Hello from Humble UI! 👋🌲🌳" font-default fill-text)))))))


(defn random-green []
  (let [r (+ 32  (rand-int 32))
        g (+ 192 (rand-int 32))
        b (+ 32  (rand-int 32))]
    (unchecked-int
      (bit-or
        (unchecked-int 0xFF000000)
        (bit-shift-left r 16)
        (bit-shift-left g 8)
        (bit-shift-left b 0)))))

(def new-year-app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font       (Font. face-default (float (* 13 scale)))
          cap-height (.getCapHeight (.getMetrics font))
          fill-text  (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
          labels     (cycle (map #(ui/label (str %) font fill-text) "HappyNew2022!"))]
      (ui/halign 0.5
        (ui/padding 0 (* 10 scale)
          (ui/dynamic ctx [rows (quot (- (:height (:bounds ctx)) (* 10 scale)) (+ (* 11 scale) cap-height))
                           time (quot (System/currentTimeMillis) 1000)]
            (apply ui/column
              (interpose (ui/gap 0 (* 1 scale))
                (for [y (range rows)]
                  (ui/halign 0.5
                    (apply ui/row
                      (interpose (ui/gap (* 1 scale) 0)
                        (for [x (range (inc y))]
                          (if (= x y 0)
                            (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFCC3333)))
                              (ui/padding (* 5 scale) (* 5 scale)
                                (ui/label "★" font fill-text)))
                            (ui/fill (doto (Paint.) (.setColor (random-green)))
                              (ui/padding (* 5 scale) (* 5 scale)
                                (let [idx (+ x (* y (+ y 1) 1/2) -1)]
                                  (nth labels idx))))))))))))))))))

(comment
  (window/request-frame @*window))

(defn on-paint [window ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF0F0F0))
  (let [bounds (window/content-rect window)
        ctx    {:bounds bounds :scale (window/scale window)}
        app    app]
    (ui/-layout app ctx (IPoint. (:width bounds) (:height bounds)))
    (ui/-draw app ctx canvas)
    (window/request-frame window)))

(defn on-event [window event]
  (let [app      app
        changed? (condp instance? event
                   EventMouseMove
                   (let [pos   (IPoint. (.getX ^EventMouseMove event) (.getY ^EventMouseMove event))
                         event {:hui/event :hui/mouse-move
                                :hui.event/pos pos}]
                     (ui/-event app event))

                   EventMouseButton
                   (let [event {:hui/event :hui/mouse-button
                                :hui.event.mouse-button/pressed? (.isPressed ^EventMouseButton event)}]
                     (ui/-event app event))

                   EventKey
                   (let [raw-key (.getKey ^EventKey event)
                         event {:hui/event :hui/key
                                :hui.event.key/modifiers {:alt (.isModifierDown ^EventKey event KeyModifier/ALT)
                                                          :ctrl (.isModifierDown ^EventKey event KeyModifier/CONTROL)}
                                :hui.event.key/key-raw raw-key
                                :hui.event.key/key (keyword (str/replace (str/lower-case (.getName raw-key)) #" " "-"))
                                :hui.event.key/pressed? (.isPressed ^EventKey event)}]
                     (ui/-event app event))

                   nil)]
    (when changed?
      (window/request-frame window))))

(defn make-window []
  (doto
    (window/make
      {:on-close (fn [window] (window/close window) (reset! *window nil))
       :on-paint #'on-paint
       :on-event #'on-event})
    (window/set-title "Humble UI 👋")
    (window/set-content-size 810 650)
    (window/set-window-position 2994 630)
    (window/set-visible true)
    (window/set-z-order :floating)
    (window/request-frame)))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (hui/init)
  (reset! *window (make-window))
  (hui/start))

(comment
  (do
    (hui/doui (some-> @*window window/close))
    (reset! *window (hui/doui (make-window))))

  (hui/doui (window/set-z-order @*window :normal))
  (hui/doui (window/set-z-order @*window :floating)))

(comment
  (-main)
  (identity @*clicks))
