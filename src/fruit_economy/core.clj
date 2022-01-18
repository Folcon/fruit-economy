(ns fruit-economy.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.stacktrace :as stacktrace]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [nrepl.cmdline :as nrepl]
   [fruit-economy.input :refer [mouse-button->kw key->kw]]
   [fruit-economy.utils :refer [resource-file->byte-array]])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventKey KeyModifier Window]
   [io.github.humbleui.skija Canvas Color4f Data FontMgr FontStyle Typeface Font Paint ClipMode]
   [io.github.humbleui.types IPoint Rect]
   [io.github.humbleui.skija.svg SVGDOM]))


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

(defrecord SVGCanvas [width height svg-path]
  ui/IComponent
  (-layout [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx canvas]
    (when svg-path
      (let [canvas ^Canvas canvas
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (.clipRect canvas rect ClipMode/INTERSECT true)
          (try
            (when-not @*broken
              (let [data (Data/makeFromBytes (resource-file->byte-array svg-path))
                    svg-dom (SVGDOM. data)]
                (.render svg-dom canvas)))
            (catch Exception e
              (reset! *broken true)
              (stacktrace/print-stack-trace (stacktrace/root-cause e))))
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]))

(defn svg-canvas
  "(svg-canvas 400 300 {:svg-path \"<path in resource>\"})"
  [width height {:keys [svg-path]}]
  (SVGCanvas. width height svg-path))
;; END Should be in io.github.humbleui.ui

(set! *warn-on-reflection* true)

(defonce font-mgr (FontMgr/getDefault))

;; GAME STATE
(defn new-state []
  (let [width  120
        height 80]
    {:width width
     :height height
     :camera [0 0]
     :peep [5 5]
     :cell 20}))

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

(def symbol-map
  {:blank "⠀"
   :ocean "🌊"})


(defn draw-impl [^Canvas canvas window-width window-height]
  (let [{:keys [width height camera peep world cell] :as state} @*state
        font-default (Font. face-default (float (* 24 1.0)))
        fill-default (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
        terrain (:terrain world)
        [camera-x camera-y] camera]
    (.clear canvas (unchecked-int 0xFFFFFBBB))

    ;; walk cells eq to window size
    (doseq [x (range (quot window-width cell))
            y (range (quot window-height cell))
            :let [;; offset by camera position
                  tile (get-in terrain [(+ camera-y y) (+ camera-x x)])
                  fill (doto (Paint.) (.setColor (render-tile-colour tile)))]]
      (.drawRect canvas (Rect/makeXYWH (* x cell) (- (* y cell) cell) cell cell) fill)
      (.drawString canvas (render-tile-str tile) (* x cell) (* y cell) font-default fill-default))
    (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF33CC33)))]
      (.drawRect canvas (Rect/makeXYWH (first peep) (second peep) 10 10) fill))

    #_(doseq [x (range 65) y (range 50)
              :let [cell 5
                    grey (int (* 256 (fruit-economy.noise/coord->noise fruit-economy.noise/simplex-noise-fn [x y] {:seed 1 :octaves 2 :lacunarity 1 :persistence 0.5 :scale [0.007 0.007] :normalise? true :low 0 :high 1})))]]
        (with-open [fill (doto (Paint.) (.setColor (colour grey grey grey)))]
          (.drawRect canvas (Rect/makeXYWH (+ (* x cell) 20) (+ (* y cell) 0) cell cell) fill)))))

(defn on-key-pressed-impl [{event-type :hui/event :hui.event.key/keys [key pressed?] :as event}]
  (let [state @*state
        move (fn [[x1 y1]] (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))]

    ;; mouse
    (when (= event-type :hui/mouse-button)
      (println event))

    ;; keyboard
    (when (and (= event-type :hui/key) pressed?)
      (println key)
      (println (:peep @*state))
      (condp = key
        :key/d ;; right
        (doto *state
          (swap! update :peep (move [1 0]))
          (swap! update :camera (move [1 0])))

        :key/a ;; left
        (doto *state
          (swap! update :peep (move [-1 0]))
          (swap! update :camera (move [-1 0])))

        :key/s ;; bottom
        (doto *state
          (swap! update :peep (move [0 1]))
          (swap! update :camera (move [0 1])))

        :key/w ;; up
        (doto *state
          (swap! update :peep (move [0 -1]))
          (swap! update :camera (move [0 -1])))

        :key/r ;; R
        (reset! *state (new-state))

        ;; (println key)
        nil))))

(def app
  (ui/dynamic ctx [scale (:scale ctx)
                   camera (:camera @*state)]
    (let [font-default        (Font. face-default (float (* 24 scale)))
          fill-text           (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/valign 0.5
        (ui/halign 0.5
          (ui/column
            (svg-canvas 200 100 {:svg-path "data.svg"})
            (ui-canvas 1200 800 {:on-paint #'draw-impl
                                 :on-event #'on-key-pressed-impl})
            (ui/label (str "👋🌲🌳 Camera: " (pr-str camera)) font-default fill-text)))))))


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
                                :hui.event.mouse-button/raw-event event
                                :hui.event.mouse-button/modifiers {:key/alt (.isModifierDown ^EventMouseButton event KeyModifier/ALT)
                                                                   :key/control (.isModifierDown ^EventMouseButton event KeyModifier/CONTROL)}
                                :hui.event.mouse-button/pressed? (.isPressed ^EventMouseButton event)
                                :hui.event.mouse-button/button (mouse-button->kw (.getButton ^EventMouseButton event))}]
                     (ui/-event app event))

                   EventKey
                   (let [raw-key (.getKey ^EventKey event)
                         event {:hui/event :hui/key
                                :hui.event.key/raw-event event
                                :hui.event.key/modifiers {:key/alt (.isModifierDown ^EventKey event KeyModifier/ALT)
                                                          :key/control (.isModifierDown ^EventKey event KeyModifier/CONTROL)}
                                :hui.event.key/key-mask (._mask raw-key)
                                :hui.event.key/key (key->kw raw-key)
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
    (window/set-title "Fruit Economy 👋")
    (window/set-content-size 1536 960 #_#_810 650)
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
