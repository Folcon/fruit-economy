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
   [fruit-economy.db.core :as db]
   [fruit-economy.data.core :as data]
   [fruit-economy.land :as land]
   [fruit-economy.unit :as unit]
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


(defn init-world [world-name width height]
  (-> (land/make-land world-name width height)
    (land/gen-land)
    (land/populate 50 #_100)
    (unit/spawn-units 10)
    (economy/add-resources)
    (civ/try-spawn-new-civs 10)))

;; GAME STATE
(defn new-state []
  (let [width  60
        height 40
        init-cell 30
        world (init-world "World" width height)]
    {:width width
     :height height
     :camera [(/ width 2) (/ height 2)]
     :peep [5 5]
     :init-cell init-cell
     :cell init-cell

     :zoom 1.
     :svg-xyz [0 0 0.]

     :world world
     :world-db (db/db-bulk-insert (db/init-db) [world])
     :history-index 0
     :civ-index 0

     :economy? false
     :info? false
     :paused? false
     :tick 0
     :tick-ms 5000
     :last-tick (System/currentTimeMillis)
     :render-ms 200
     :last-render (System/currentTimeMillis)}))

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
  (let [{:keys [tick world-db]} state
        world-db' (game/on-tick world-db)
        tick' (inc tick)]
    (assoc state
      :world-db world-db'
      :tick tick'
      :last-tick now)))

(defn on-render [state now]
  (assoc state :last-render now))

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
  (let [{:keys [camera peep world world-db zoom cell hovering viewport-width viewport-height half-vw half-vh tick paused? tick-ms last-tick render-ms last-render] :as state} @*state

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

        {::land/keys [terrain area->civ-name civ-name->civ area->resources area->units]} (data/land-data world-db)
        territory (into #{} (map :area) (data/land-claims world-db))
        [camera-x camera-y] camera
        now (System/currentTimeMillis)
        render? (> (- now last-render) render-ms)]
    ;; slowing render down
    (when render?
      (swap! *state on-render now))

    (.clear canvas (unchecked-int 0xFFFFFBBB))

    #_(println :panel tick)

    ;; walk cells eq to window size
    ;; draw land on buffer if even and then render buffer
    (when render?
      (doseq [;; TODO: Figure out why this viewport-width fixed the tile rendering bug,
              ;;   it's almost certainly a scaling value thing.
              x (range (long (* viewport-width 1.5)))
              y (range viewport-height)
              :let [;; so we need to first have the center of the area we're scaling be [0 0],
                    ;;   so we subtract each side by half it's length,
                    ;;   then we scale it by its zoom,r
                    ;;   remove any remainder by calling int
                    ;;   and then finally we offset by camera position
                    loc-x (+ (int (- x half-vw)) camera-x)
                    loc-y (+ (int (- y half-vh)) camera-y)
                    loc [loc-x loc-y]
                    path [loc-y loc-x]
                    tile (get-in terrain path)
                    territory? (contains? territory loc)
                    {::civ/keys [symbol tint] :as civ} (data/land-area->civ world-db loc)
                    [glyph tile-colour font dx dy] (cond
                                                     civ [symbol tint font-default font-offset-x font-offset-y]
                                                     territory? ["" #_(land/render-tile-str tile) tint font-default font-offset-x font-offset-y]
                                                     :else ["" #_(land/render-tile-str tile) (land/render-tile-colour tile) font-default font-offset-x font-offset-y])
                    tile-colour (if (= (:world hovering) loc) (colour 255 255 255) tile-colour)
                    fill (doto (Paint.) (.setColor tile-colour))
                    ;; TODO: Temp fix for needing to overdraw tiles to stop the gap.
                    padded-cell (+ 0.5 cell)]]
        ;; To draw, we just take the current x or y we're on and simply multiply it by the cell size.
        (.drawRect canvas (Rect/makeXYWH (* x cell) (* y cell) padded-cell padded-cell) fill)
        (.drawString canvas glyph (+ dx (* x cell)) (+ dy (* y cell)) font fill-default)))

    ;; draw things
    ;; walk cells eq to window size
    (when render?
      (doseq [;; TODO: Figure out why this viewport-width fixed the tile rendering bug,
              ;;   it's almost certainly a scaling value thing.
              x (range (long (* viewport-width 1.5)))
              y (range viewport-height)
              :let [loc-x (+ (int (- x half-vw)) camera-x)
                    loc-y (+ (int (- y half-vh)) camera-y)
                    loc [loc-x loc-y]
                    path [loc-y loc-x]
                    things (data/land-area world-db loc)
                    size (count things)
                    thing (when-not (zero? size) (:glyph (nth things (rem tick size))))]
              :when thing
              :let [tile (get-in terrain path)
                    territory? (contains? territory loc)
                    {::civ/keys [symbol tint] :as civ} (data/land-area->civ world-db loc)
                    [glyph tile-colour font dx dy] [thing (if territory? tint (land/render-tile-colour tile)) emoji-font emoji-offset-x emoji-offset-y]
                    tile-colour (if (= (:world hovering) loc) (colour 255 255 255) tile-colour)
                    fill (doto (Paint.) (.setColor tile-colour))
                    ;; TODO: Temp fix for needing to overdraw tiles to stop the gap.
                    padded-cell (+ 0.5 cell)]]
        ;; To draw, we just take the current x or y we're on and simply multiply it by the cell size.
        (.drawRect canvas (Rect/makeXYWH (* x cell) (* y cell) padded-cell padded-cell) fill)
        (.drawString canvas glyph (+ dx (* x cell)) (+ dy (* y cell)) font fill-default)))
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
      (let [{:keys [camera cell half-vw half-vh]} state
            [camera-x camera-y] camera
            [screen-x screen-y] ((juxt :x :y) (:hui.event/pos event))
            loc-x (+ (int (- (quot screen-x cell) half-vw)) camera-x)
            loc-y (+ (int (- (quot screen-y cell) half-vh)) camera-y)]
        (swap! *state assoc :hovering {:world [loc-x loc-y] :screen [screen-x screen-y]})))

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

        #{:key/minus}
        (let [{:keys [scale zoom init-cell canvas-width canvas-height]} state
              zoom' (max 0.2 (- zoom 0.2))
              cell' (long (/ (* init-cell zoom') scale))
              viewport-width' (inc (quot canvas-width cell'))
              viewport-height' (inc (quot canvas-height cell'))
              half-vw' (quot viewport-width' 2)
              half-vh' (quot viewport-height' 2)]
          (swap! *state assoc :zoom zoom' :cell cell' :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh'))

        #{:key/equals}
        (let [{:keys [scale zoom init-cell canvas-width canvas-height]} state
              zoom' (min 5 (+ zoom 0.2))
              cell' (long (/ (* init-cell zoom') scale))
              viewport-width' (inc (quot canvas-width cell'))
              viewport-height' (inc (quot canvas-height cell'))
              half-vw' (quot viewport-width' 2)
              half-vh' (quot viewport-height' 2)]
          (swap! *state assoc :zoom zoom' :cell cell' :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh'))

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

(declare on-resize)

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
              history (data/history-log-entries (:world-db state))
              history-size (count history)]
          (swap! *state assoc :history-index (min (dec history-size) (inc history-index))))

        #{:key/e}
        (let [history-index (get state :history-index)]
          (swap! *state assoc :history-index (max 0 (dec history-index))))

        #{:key/t}
        (swap! *state update :economy? not)

        #{:key/y}
        (swap! *state update :world-db data/step-economy)

        #{:key/p}
        (swap! *state update :paused? not)

        #{:key/close-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! *state assoc :civ-index (rem (inc civ-index) size)))

        #{:key/open-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! *state assoc :civ-index (rem (+ (dec civ-index) size) size)))

        ;#{:key/digit5}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)
        ;
        ;      ordered-civs (data/ordered-civs (:world-db state))
        ;      controlling-civ-id (nth ordered-civs civ-index)
        ;      controlling-civ' (data/entity (:world-db state) controlling-civ-id)]
        ;  (when controlling-civ
        ;    (-> *state
        ;      (swap! update :world civ-actions/grow-pop controlling-civ)
        ;      (swap! update :world-db civ-actions/grow-pop' controlling-civ'))))
        ;
        ;#{:key/digit6}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)
        ;
        ;      ordered-civs (data/ordered-civs (:world-db state))
        ;      _ (println :ordered-civs ordered-civs)
        ;      controlling-civ-id (nth ordered-civs civ-index)
        ;      _ (println :controlling-civ-id controlling-civ-id)
        ;      controlling-civ' (data/entity (:world-db state) controlling-civ-id)
        ;      _ (println :controlling-civ controlling-civ)]
        ;  (when controlling-civ
        ;    (swap! *state update :world civ-actions/expand-territory controlling-civ)))
        ;
        ;#{:key/digit7}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)]
        ;  (when controlling-civ
        ;    (swap! *state update :world civ-actions/improve-tech-level controlling-civ)))

        #{:key/r}
        (do
          (reset! *state (new-state))
          (on-resize @*window))

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
                   history (data/history-log-entries (get @*state :world-db))]
    (let [font-default        (Font. face-default (float (* 18 scale)))
          font-small          (Font. face-default (float (* 12 scale)))
          fill-text           (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
          history-size (count history)
          {::land/keys [civ-name->civ economy]} (data/land-data (get @*state :world-db))
          controlling (nth (keys civ-name->civ) civ-index)
          [svg-x svg-y svg-z] svg-xyz
          canvas-width (* x-scale *canvas-width*)
          canvas-height (* y-scale *canvas-height*)]
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
                          (custom-ui/svg-canvas canvas-width canvas-height
                            {:svg-str (economy/->svg economy)
                             :on-event #'on-key-pressed-svg-impl}))))
                    (custom-ui/ui-canvas canvas-width canvas-height
                      {:on-paint #'draw-impl
                       :on-event #'on-key-pressed-impl}))))
              (ui/padding 10
                (ui/label (str "👋🌲🌳Camera: " (pr-str camera) " Year: " tick (when controlling (str " controlling " controlling))) font-default fill-text))
              (ui/padding 10
                (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") font-small fill-text))
              (ui/padding 10
                (ui/label (str "[WASD] or arrow keys: Pan the camera, [-]: Zoom Out, [+]: Zoom In") font-small fill-text)))))))))


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
    (ui/draw app ctx bounds canvas)
    (window/request-frame window)))

(defn on-event [window event]
  (let [app      app
        changed? (condp instance? event
                   EventMouseMove
                   (let [pos   (IPoint. (.getX ^EventMouseMove event) (.getY ^EventMouseMove event))
                         event {:hui/event :hui/mouse-move
                                :hui.event/pos pos}]
                     (ui/event app event))

                   EventMouseButton
                   (let [pressed? (.isPressed ^EventMouseButton event)
                         event {:hui/event :hui/mouse-button
                                :hui.event.mouse-button/raw-event event
                                :hui.event.mouse-button/modifiers {:key/alt (.isModifierDown ^EventMouseButton event KeyModifier/ALT)
                                                                   :key/control (.isModifierDown ^EventMouseButton event KeyModifier/CONTROL)}
                                :hui.event.mouse-button/pressed? pressed?
                                :hui.event.mouse-button/is-pressed pressed?
                                :hui.event.mouse-button/button (mouse-button->kw (.getButton ^EventMouseButton event))}]
                     (ui/event app event))

                   EventMouseScroll
                   (ui/event app
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
                     (ui/event app event))

                   nil)]
    (when changed?
      (window/request-frame window))))

(defn on-resize [window]
  (let [[min-width min-height] [600 400]
        [window-width window-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) (window/window-rect window))
        bounds (window/content-rect window)
        [width height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) bounds)
        {:keys [init-cell zoom]} @*state
        scale (max (float (/ *canvas-width* width)) (float (/ *canvas-height* height)))
        {screen :work-area} (hui/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))

        ;; we're calling long because drawRect appears to draw at whole numbers so if we want no gaps,
        ;;   we should pass it whole numbers
        ;; TODO: need better names here, we want to be able to disambiguate the size of the tile, whether it's scaled or not and what those things depend on.
        cell' (long (/ (* init-cell zoom) scale))
        canvas-width' (long (* x-scale *canvas-width*))
        canvas-height' (long (* y-scale *canvas-height*))
        viewport-width' (inc (quot canvas-width' cell'))
        viewport-height' (inc (quot canvas-height' cell'))
        half-vw' (quot viewport-width' 2)
        half-vh' (quot viewport-height' 2)]
    (swap! *state assoc
      :scale scale :cell cell' :canvas-width canvas-width' :canvas-height canvas-height'
      :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh')
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
