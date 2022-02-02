(ns fruit-economy.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [environ.core :refer [env]]
   [clojure.stacktrace :as stacktrace]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [fruit-economy.humble-ui :as custom-ui]
   [fruit-economy.colour :refer [colour]]
   [fruit-economy.input :refer [mouse-button->kw key->kw]]
   [fruit-economy.graph :refer [graph?]]
   [fruit-economy.land :as land]
   [fruit-economy.civ :as civ]
   [fruit-economy.game :as game]
   [fruit-economy.economy :as economy]
   [fruit-economy.civ-actions :as civ-actions])
  (:import
   [io.github.humbleui.jwm EventMouseButton EventMouseMove EventMouseScroll EventKey KeyModifier]
   [io.github.humbleui.skija Canvas Color4f FontMgr FontStyle Typeface Font Paint]
   [io.github.humbleui.types IPoint IRect Rect])
  (:gen-class))


(set! *warn-on-reflection* true)

(defn debug? [] (= (env :debug?) "true"))

(defonce font-mgr (FontMgr/getDefault))

(defonce ^:dynamic *canvas-width* 2400)
(defonce ^:dynamic *canvas-height* 1200)

;; GAME STATE
(defn new-state []
  (let [width  60
        height 40
        init-cell 30]
    {:width width
     :height height
     :camera [(/ width 2) (/ height 2)]
     :peep [5 5]
     :init-cell init-cell
     :cell init-cell

     :zoom 1.
     :svg-xyz [0 0 0.]

     :world (-> (land/make-land "World" width height)
              (land/gen-land)
              (land/populate 50 #_100)
              (land/spawn-units 10)
              (economy/add-resources)
              (civ/try-spawn-new-civs 10))
     :history-index 0
     :civ-index 0

     :economy?  false
     :paused?   false
     :tick      0
     :tick-ms   5000
     :last-tick (System/currentTimeMillis)}))

(defonce *state (atom (new-state)))
;; END GAME STATE

(defonce *window (atom nil))

(defonce ^Typeface face-default
  (.matchFamiliesStyle ^FontMgr font-mgr (into-array String [".SF NS", "Helvetica Neue", "Arial"]) FontStyle/NORMAL))

(defonce game-glyph
  (let [economy-glyph (rand-nth ["💰" "💸" "🤑" "🏦" "💵" "💱" "💴" "💶" "💷"])
        fruit-glyph (rand-nth ["🥭" "🍅" "🍊" "🍉" "🍏" "🍐" "🍌" "🍑" "🍈" "🍋" "🍍" "🍓" "🍎" "🍇" "🥝" "🍒"])]
    (str fruit-glyph economy-glyph)))

(defonce emoji-glyph "🍀")
(defonce ^Typeface emoji-face (.matchFamilyStyleCharacter ^FontMgr font-mgr nil FontStyle/NORMAL nil (.codePointAt ^String emoji-glyph 0)))

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

(defn on-tick [state now]
  (let [{:keys [tick world]} state
        world' (game/on-tick world)
        tick' (inc tick)]
    (assoc state
      :world world'
      :tick tick'
      :last-tick now)))

(comment
  (let [[x y] [21 47]
        loc [x y]
        path [y x]]
   (->
     (get-in @*state [:world ::land/civ-name->civ #_:terrain "Civ A+0"])
     ;(get-in @*state [:world ::land/terrain y x])
     #_keys))
  ,)

(defn draw-impl [^Canvas canvas window-width window-height]
  (let [{:keys [camera peep world zoom cell hovering tick paused? tick-ms last-tick] :as state} @*state

        font-default (Font. face-default (float (* 24 zoom)))
        fill-default (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))

        ;; Rendering text
        font-bounds (.measureText font-default "X")
        font-offset-x (-> (- (.getLeft font-bounds))
                        (- (/ (- (.getWidth font-bounds) cell) 2)))
        font-offset-y (-> (- (.getTop font-bounds))
                        (- (/ (- (.getHeight font-bounds) cell) 2)))


        ;; Rendering emoji
        emoji-font (Font. emoji-face (float (* 20 zoom)))
        emoji-bounds (.measureText emoji-font emoji-glyph)
        emoji-offset-x (-> (- (.getLeft emoji-bounds))
                         (- (/ (- (.getWidth emoji-bounds) cell) 2)))
        emoji-offset-y (-> (- (.getTop emoji-bounds))
                         (- (/ (- (.getHeight emoji-bounds) cell) 2)))

        {::land/keys [terrain area->civ-name civ-name->civ area->resources area->units]} world
        territory (into #{} (comp (map (fn [[_k {::civ/keys [territory]}]] territory)) cat) civ-name->civ)
        [camera-x camera-y] camera]
    (.clear canvas (unchecked-int 0xFFFFFBBB))

    #_(println :panel tick)

    ;; walk cells eq to window size
    (doseq [x (range (quot window-width cell))
            y (range (quot window-height cell))
            :let [;; offset by camera position
                  loc [(+ camera-x x) (+ camera-y y)]
                  path [(+ camera-y y) (+ camera-x x)]
                  tile (get-in terrain path)
                  resource (get-in area->resources [loc :glyph])
                  unit (get-in area->units [loc :glyph])
                  territory? (contains? territory loc)
                  {::civ/keys [symbol tint] :as civ} (get civ-name->civ (get area->civ-name loc))
                  [glyph tile-colour font dx dy] (cond
                                                   unit [unit (if territory? tint (land/render-tile-colour tile)) emoji-font emoji-offset-x emoji-offset-y]
                                                   resource [resource (if territory? tint (land/render-tile-colour tile)) emoji-font emoji-offset-x emoji-offset-y]
                                                   civ [symbol tint font-default font-offset-x font-offset-y]
                                                   territory? ["" #_(land/render-tile-str tile) tint font-default font-offset-x font-offset-y]
                                                   :else ["" #_(land/render-tile-str tile) (land/render-tile-colour tile) font-default font-offset-x font-offset-y])
                  tile-colour (if (= (:world hovering) loc) (colour 255 255 255) tile-colour)
                  fill (doto (Paint.) (.setColor tile-colour))]]
      (.drawRect canvas (Rect/makeXYWH (* x cell) (* y cell) cell cell) fill)
      (.drawString canvas glyph (+ dx (* x cell)) (+ dy (* y cell)) font fill-default))
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
    (when (and (= event-type :hui/mouse-move) (:hui.event/pos event))
      (let [{:keys [camera cell]} state
            [camera-x camera-y] camera
            [x y] ((juxt :x :y) (:hui.event/pos event))]
        (swap! *state assoc :hovering {:world [(+ (quot x cell) camera-x) (+ (quot y cell) camera-y)] :screen [x y]})))

    (when (= event-type :hui/mouse-button)
      (println :panel event)
      (println (:hovering state)))

    ;; keyboard
    (when (and (= event-type :hui/key) pressed?)
      (println :panel key)
      (println (:peep @*state))
      (condp contains? key
        #{:key/d :key/right}
        (doto *state
          (swap! update :peep (move [1 0]))
          (swap! update :camera (move [1 0])))

        #{:key/a :key/left}
        (doto *state
          (swap! update :peep (move [-1 0]))
          (swap! update :camera (move [-1 0])))

        #{:key/s :key/down}
        (doto *state
          (swap! update :peep (move [0 1]))
          (swap! update :camera (move [0 1])))

        #{:key/w :key/up}
        (doto *state
          (swap! update :peep (move [0 -1]))
          (swap! update :camera (move [0 -1])))

        ;; (println :panel key)
        nil))))

(defn draw-mini-panel-impl
  "Render for small panel, this will be a sort of global render
  context to ensure stuff like ticks still keep happening"
  [^Canvas canvas window-width window-height]
  (let [{:keys [tick tick-ms last-tick paused?] :as _state} @*state
        now (System/currentTimeMillis)
        fill-text           (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))

        cell-x (/ window-width 2)
        cell-y (/ window-height 2)

        ;; Rendering emoji
        emoji-font (Font. emoji-face (float 72))
        emoji-bounds (.measureText emoji-font emoji-glyph)
        emoji-offset-x (-> (- (.getLeft emoji-bounds))
                         (- (/ (- (.getWidth emoji-bounds) cell-x) 2)))
        emoji-offset-y (-> (- (.getTop emoji-bounds))
                         (- (/ (- (.getHeight emoji-bounds) cell-y) 2)))]
    ;; tick handling
    (when (and
            (not paused?)
            (> (- now last-tick) tick-ms))
      (swap! *state on-tick now))

    #_(println :mini-panel tick)

    ;; Put the clear here to show where the mini panel is,
    ;;   will probably use it in some other way later
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (.drawString canvas game-glyph emoji-offset-x (+ emoji-offset-y (/ cell-y 2)) emoji-font fill-text)))

(defn on-key-pressed-mini-panel-impl [{event-type :hui/event :hui.event.key/keys [key pressed?] :as event}]
  (let [state @*state]

    ;; mouse
    (when (and (= event-type :hui/mouse-move) (:hui.event/pos event))
      (println :mini-panel event))

    (when (= event-type :hui/mouse-button)
      (println :mini-panel event))

    ;; keyboard
    (when (and (= event-type :hui/key) pressed?)
      (println :mini-panel key)
      (println (:peep @*state))
      (condp contains? key
        #{:key/q}
        (let [history-index (get state :history-index)
              history (get-in state [:world ::land/history])
              history-size (count history)]
          (swap! *state assoc :history-index (min (dec history-size) (inc history-index))))

        #{:key/e}
        (let [history-index (get state :history-index)]
          (swap! *state assoc :history-index (max 0 (dec history-index))))

        #{:key/t}
        (swap! *state update :economy? not)

        #{:key/y}
        (swap! *state update :world economy/step-economy)

        #{:key/p}
        (swap! *state update :paused? not)

        #{:key/close-bracket}
        (let [civ-index (get state :civ-index)
              civ-name->civ (get-in state [:world ::land/civ-name->civ])
              size (count civ-name->civ)]
          (swap! *state assoc :civ-index (rem (inc civ-index) size)))

        #{:key/open-bracket}
        (let [civ-index (get state :civ-index)
              civ-name->civ (get-in state [:world ::land/civ-name->civ])
              size (count civ-name->civ)]
          (swap! *state assoc :civ-index (rem (+ (dec civ-index) size) size)))

        #{:key/digit5}
        (let [civ-index (get state :civ-index)
              civ-name->civ (get-in state [:world ::land/civ-name->civ])
              controlling-civ (nth (vals civ-name->civ) civ-index)]
          (when controlling-civ
            (swap! *state update :world civ-actions/grow-pop controlling-civ)))

        #{:key/digit6}
        (let [civ-index (get state :civ-index)
              civ-name->civ (get-in state [:world ::land/civ-name->civ])
              controlling-civ (nth (vals civ-name->civ) civ-index)]
          (when controlling-civ
            (swap! *state update :world civ-actions/expand-territory controlling-civ)))

        #{:key/digit7}
        (let [civ-index (get state :civ-index)
              civ-name->civ (get-in state [:world ::land/civ-name->civ])
              controlling-civ (nth (vals civ-name->civ) civ-index)]
          (when controlling-civ
            (swap! *state update :world civ-actions/improve-tech-level controlling-civ)))

        #{:key/r}
        (reset! *state (new-state))

        ;; (println :mini-panel key)
        nil))))

(defn on-key-pressed-svg-impl [{event-type :hui/event :hui.event.key/keys [key pressed?] :as event}]
  (let [state @*state]

    ;; mouse
    (when (and (= event-type :hui/mouse-move) (:hui.event/pos event))
      (println :tech-ui-panel event))

    (when (= event-type :hui/mouse-button)
      (println :tech-ui-panel event))

    ;; keyboard
    (when (and (= event-type :hui/key) pressed?)
      (println :tech-ui-panel key)
      (condp contains? key
        #{:key/d :key/right}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 0] + 20))

        #{:key/a :key/left}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 0] - 20))

        #{:key/s :key/down}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 1] + 20))

        #{:key/w :key/up}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 1] - 20))

        #{:key/minus}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 2] - 0.1))

        #{:key/equals}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 2] + 0.1))

        ;; (println :tech-ui-panel key)
        nil))))

(def app
  (ui/dynamic ctx [{:keys [scale bounds x-scale y-scale xy-scale]} ctx
                   {:keys [camera tick history-index civ-index economy? svg-xyz]} @*state
                   history (get-in @*state [:world ::land/history])]
    (let [font-default        (Font. face-default (float (* 18 scale)))
          font-small          (Font. face-default (float (* 12 scale)))
          fill-text           (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
          history-size (count history)
          {::land/keys [civ-name->civ economy]} (get @*state :world)
          controlling (nth (keys civ-name->civ) civ-index)
          [svg-x svg-y svg-z] svg-xyz]
      (ui/row
        (ui/column
          (custom-ui/ui-canvas 150 150 {:on-paint #'draw-mini-panel-impl
                                        :on-event #'on-key-pressed-mini-panel-impl}))
        (ui/valign 0.5
          (ui/halign 0.5
            (ui/column
              (if (zero? history-size)
                (ui/gap 0 0)
                (ui/padding 10
                  (ui/label (str (inc history-index) " of " history-size ": " (nth history (- (dec history-size) history-index))) font-default fill-text)))
              (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
                (ui/padding 3
                  (if (and (graph? economy) economy?)
                    (ui/valign 0.5
                      (ui/halign 0.5
                        (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                          (custom-ui/svg-canvas (* x-scale *canvas-width*) (* y-scale *canvas-height*)
                            {:svg-str (economy/->svg economy)
                             :on-event #'on-key-pressed-svg-impl}))))
                    (custom-ui/ui-canvas (* x-scale *canvas-width*) (* y-scale *canvas-height*)
                      {:on-paint #'draw-impl
                       :on-event #'on-key-pressed-impl}))))
              (ui/padding 10
                (ui/label (str "👋🌲🌳Camera: " (pr-str camera) " Year: " tick (when controlling (str " controlling " controlling))) font-default fill-text))
              (ui/padding 10
                (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") font-small fill-text)))))))))



(comment
  (window/request-frame @*window))

(defn on-paint [window ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF0F0F0))
  (let [bounds (window/content-rect window)
        {screen :work-area} (hui/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))
        xy-scale (max x-scale y-scale)
        ctx    {:bounds bounds :scale (window/scale window) :x-scale x-scale :y-scale y-scale :xy-scale xy-scale}
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

                   EventMouseScroll
                   (ui/-event app
                     {:hui/event :hui/mouse-scroll
                      :hui.event.mouse-scroll/dx (.getDeltaX ^EventMouseScroll event)
                      :hui.event.mouse-scroll/dy (.getDeltaY ^EventMouseScroll event)})

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

(defn on-resize [window]
  (let [[min-width min-height] [600 400]
        [window-width window-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) (window/window-rect window))
        {:keys [init-cell]} @*state
        scale (max (float (/ *canvas-width* window-width)) (float (/ *canvas-height* window-height)))]
    (swap! *state assoc :cell (int (/ init-cell scale)))
    (window/set-window-size window (max window-width min-width) (max window-height min-height))))

(defn screen-sized-window [window {:keys [width height right y] :as _work-area}]
  (let [window-width  width
        window-height height
        window-left   (- right window-width)
        window-top    (-> y
                        (+ height)
                        (- (/ window-height 2)))]
    (doto window
      (window/set-window-size window-width window-height)
      (window/set-window-position window-left window-top))))

(defn small-window [window {:keys [width height right y] :as _work-area}]
  (let [window-width  (/ width 2)
        window-height (/ height 2)
        window-left   (- right window-width)
        window-top    (-> y
                        (+ (/ height 2))
                        (- (/ window-height 2)))]
    (doto window
      (window/set-window-size window-width window-height)
      (window/set-window-position window-left window-top))))

(defn make-window []
  (let [{:keys [work-area]} (hui/primary-screen)
        window-size-fn (if (debug?) small-window screen-sized-window)]
    (doto
      (window/make
        {:on-close (if (debug?) #(reset! *window nil) #(System/exit 0))
         :on-paint #'on-paint
         :on-event #'on-event
         :on-resize #'on-resize})
      (window/set-title "Fruit Economy 👋")
      (window-size-fn work-area)
      (window/set-visible true)
      (window/set-z-order :floating))))

(defn -main [& args]
  ;; TODO: Display somewhere in the UI
  (println (str "VERSION: " (env :game-version) (when (debug?) "\nDEBUG BUILD")))
  (when (debug?)
    ;; Swap to require and resolve in one step!
    (future (apply (requiring-resolve 'nrepl.cmdline/-main) args)))
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
  (identity @*clicks)
  (identity @*window)
  (get-in @*state [:world ::land/civ-name->civ])

  (let [civs (get-in @*state [:world #_::land/civ-name->civ ::land/area->manor])]
    #_(into #{} (comp (map (fn [[_k {::civ/keys [territory]}]] territory)) cat) civs))

  (identity @fruit-economy.core/*clicks)

  (identity fruit-economy.core/app)

  (use 'clojure.reflect 'clojure.pprint)
  (clojure.pprint/pprint (clojure.reflect/reflect fruit-economy.core/app))

  (swap! *clicks inc)

  (require '[clj-async-profiler.core :as prof])
  (prof/profile-for 60)
  (prof/serve-files 8080)

  (let [interesting-flamegraph "/tmp/clj-async-profiler/results/01-cpu-flamegraph-2022-01-19-16-09-18.html"
        profiling-path (str/replace interesting-flamegraph "/tmp/" "resources/profiling/")]
    (io/make-parents profiling-path)
    (io/copy (io/file interesting-flamegraph) (io/file profiling-path)))

  ,)
