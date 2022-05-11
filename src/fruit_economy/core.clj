(ns fruit-economy.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [environ.core :refer [env]]
   [clojure.stacktrace :as stacktrace]
   [io.github.humbleui.app :as app]
   [io.github.humbleui.canvas :as canvas]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [fruit-economy.humble-ui :as custom-ui]
   [fruit-economy.components :as cui]
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
   [fruit-economy.sim.basic :as basic])
  (:import
   [io.github.humbleui.jwm EventMouseButton EventMouseMove EventMouseScroll EventKey KeyModifier]
   [io.github.humbleui.skija Surface Canvas Color4f FontMgr FontStyle Typeface Font Paint PaintMode]
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
     :render-ms 400
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
          (ui/label "Top Bar" {:font font-default :paint fill-text}))
        (ui/row
          (ui/valign 0.1
            (ui/column
              (ui/label "Left Sidebar" {:font font-default :paint fill-text})))
          (ui/valign 0.5
            (ui/halign 0.4
              (ui/column
                (ui/label "Hello from Humble UI! 👋" {:font font-default :paint fill-text})
                (ui/gap 0 leading)
                (ui/dynamic _ [clicks @*clicks]
                  (ui/label (str "Clicked: " clicks) {:font font-default :paint fill-text}))
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
                            (ui/label label {:font font-default :paint fill-text}))))))))))
          (ui/valign 0.1
            (ui/halign 1.2
              (ui/column
                (ui/label "Right Sidebar" {:font font-default :paint fill-text}))))
          ;; Not currently visible, should work out what the layout system is
          (ui/row
            (ui/label "Bottom Bar" {:font font-default :paint fill-text})))))))

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

(def *render-cache (atom {}))

(defn make-rendered [cell]
  (let [buffer (Surface/makeRasterN32Premul cell cell #_#_(* cell 10) (* cell 10))]
    {:buffer buffer
     :canvas (.getCanvas buffer)
     :image (.makeImageSnapshot buffer)}))

(defn draw-impl [^Canvas canvas viewport-width viewport-height]
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
    ;; TODO: This approach is workable, but not quite good enough, I need to be able to cache at multiple levels.
    ;;   So for example if there's no civ or thing changes, don't re-render.
    (doseq [x (range viewport-width)
            y (range viewport-height)]
      (let [;; args are the args captured in our memoize-last
            args [world-db camera x y]
            render-fn (fn [world-db camera x y]
                        (let [rendered (get-in @*render-cache [args :rendered] (make-rendered cell))
                              ;; pixel-x and pixel-y
                              px-x (* x cell) px-y (* y cell)

                              loc-x (+ (int (- x half-vw)) camera-x)
                              loc-y (+ (int (- y half-vh)) camera-y)
                              loc [loc-x loc-y]
                              path [loc-y loc-x]
                              biome (get-in terrain path)

                              territory? (contains? territory loc)
                              {::civ/keys [symbol tint] :as civ} (when territory? (data/land-area->civ world-db loc))

                              things (data/land-area world-db loc)
                              size (count things)
                              thing (when-not (zero? size) (:glyph (nth things (rem tick size))))

                              [glyph tile-colour font dx dy] (cond
                                                               thing [thing (if territory? tint (land/render-tile-colour biome)) emoji-font emoji-offset-x emoji-offset-y]
                                                               civ [symbol tint font-default font-offset-x font-offset-y]
                                                               territory? ["" tint font-default font-offset-x font-offset-y]
                                                               :else ["" (land/render-tile-colour biome) font-default font-offset-x font-offset-y])]
                          (with-open [fill (doto (Paint.) (.setColor tile-colour))]
                            (.drawRect ^Canvas (:canvas rendered) (Rect/makeXYWH 0 0 cell cell) fill)
                            (.drawString ^Canvas (:canvas rendered) glyph dx dy font fill-default))
                          (assoc rendered :px-x px-x :px-y px-y :image (.makeImageSnapshot ^Surface (:buffer rendered)))))
            inputs-fn (get-in @*render-cache [args :fn] (hui/memoize-last render-fn))
            rendered' (apply inputs-fn args)
            rendered (get-in @*render-cache [args :rendered])]
        (when-not (identical? rendered rendered')
          (swap! *render-cache assoc args {:fn inputs-fn
                                           :rendered rendered'}))
        (.drawImage canvas (:image rendered') (:px-x rendered') (:px-y rendered'))))

    (when hovering
      (with-open [fill (doto (Paint.) (.setColor (colour 0x66FFD700)))]
        (.drawRect canvas (Rect/makeXYWH ((comp #(* (quot % cell) cell) first :screen) hovering) ((comp #(* (quot % cell) cell) second :screen) hovering) cell cell) fill)))

    (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF33CC33)))]
      (.drawRect canvas (Rect/makeXYWH (first peep) (second peep) 10 10) fill))))

(defn on-key-pressed-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @*state
        move (fn [[x1 y1]] (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (let [{:keys [camera cell half-vw half-vh]} state
            [camera-x camera-y] camera
            [screen-x screen-y] ((juxt :x :y) (:hui.event/pos raw-event))
            loc-x (+ (int (- (quot screen-x cell) half-vw)) camera-x)
            loc-y (+ (int (- (quot screen-y cell) half-vh)) camera-y)]
        (swap! *state assoc :hovering {:world [loc-x loc-y] :screen [screen-x screen-y]})))

    (when (= event :hui/mouse-button)
      (println :panel raw-event)
      (println (:hovering state)))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :panel key)
      (println (:peep @*state))
      (condp contains? key
        #{:d :right}
        (doto *state
          (swap! update :peep (move [1 0]))
          (swap! update :camera (move [1 0])))

        #{:a :left}
        (doto *state
          (swap! update :peep (move [-1 0]))
          (swap! update :camera (move [-1 0])))

        #{:s :down}
        (doto *state
          (swap! update :peep (move [0 1]))
          (swap! update :camera (move [0 1])))

        #{:w :up}
        (doto *state
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
          (swap! *state assoc :zoom zoom' :cell cell' :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh'))

        #{:equals}
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
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))

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

(defn on-key-pressed-mini-panel-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @*state]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (println :mini-panel raw-event))

    (when (= event :hui/mouse-button)
      (println :mini-panel raw-event))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :mini-panel key)
      (println (:peep @*state))
      (condp contains? key
        #{:q}
        (let [history-index (get state :history-index)
              history (data/history-log-entries (:world-db state))
              history-size (count history)]
          (swap! *state assoc :history-index (min (dec history-size) (inc history-index))))

        #{:e}
        (let [history-index (get state :history-index)]
          (swap! *state assoc :history-index (max 0 (dec history-index))))

        #{:t}
        (swap! *state update :economy? not)

        #{:y}
        (swap! *state update :world-db data/step-economy)

        #{:p}
        (swap! *state update :paused? not)

        #{:close-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! *state assoc :civ-index (rem (inc civ-index) size)))

        #{:open-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! *state assoc :civ-index (rem (+ (dec civ-index) size) size)))

        ;#{:digit5}
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
        ;#{:digit6}
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
        ;#{:digit7}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)]
        ;  (when controlling-civ
        ;    (swap! *state update :world civ-actions/improve-tech-level controlling-civ)))

        #{:r}
        (do
          (reset! *state (new-state))
          (on-resize @*window))

        ;; (println :mini-panel key)
        nil))))

(defn on-key-pressed-svg-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @*state]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (println :tech-ui-panel raw-event))

    (when (= event :hui/mouse-button)
      (println :tech-ui-panel raw-event))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :tech-ui-panel key)
      (condp contains? key
        #{:d :right}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 0] + 20))

        #{:a :left}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 0] - 20))

        #{:s :down}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 1] + 20))

        #{:w :up}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 1] - 20))

        #{:minus}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 2] - 0.1))

        #{:equals}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! *state update-in [:svg-xyz 2] + 0.1))

        ;; (println :tech-ui-panel key)
        nil))))

#_
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
                  (ui/label (str (inc history-index) " of " history-size ": " (nth history (- (dec history-size) history-index))) {:font font-default :paint fill-text})))
              (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
                (ui/padding 3
                  (if (and (graph? economy) economy?)
                    (ui/valign 0.5
                      (ui/halign 0.5
                        (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                          (custom-ui/svg-canvas (economy/->svg economy)))))
                    (custom-ui/ui-canvas canvas-width canvas-height
                      {:on-paint #'draw-impl
                       :on-event #'on-key-pressed-impl}))))
              (ui/padding 10
                (ui/label (str "👋🌲🌳Camera: " (pr-str camera) " Year: " tick (when controlling (str " controlling " controlling))) {:font font-default :paint fill-text}))
              (ui/padding 10
                (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") {:font font-small :paint fill-text}))
              (ui/padding 10
                (ui/label (str "[WASD] or arrow keys: Pan the camera, [-]: Zoom Out, [+]: Zoom In") {:font font-small :paint fill-text})))))))))

(def padding 4)

(defn nested-limit
  ([coll limit] (nested-limit coll limit nil))
  ([coll limit elide-str]
   (let [cond-add-elide (fn [v] (if elide-str (conj v elide-str) v))]
     (reduce
       (fn [[rem v] item]
         (let [size (count item)
               rem' (- rem size)]
           (cond
             (< rem size) (reduced (conj v (cond-add-elide (into [] (take rem) item))))
             (> rem' 0) [rem' (conj v item)]
             (zero? rem') (reduced (conj v item)))))
       [limit []]
       coll))))

(comment
  ;; Can turn to tests later...
  (let [;; 3 => [[1 2] [3]]
        ;; 4 => [[1 2] [3 4]]
        coll [[1 2] [3 4] [5 6]]
        coll' [[1 2] [3 4 5 6 7 8 9 10]]]
    (and
      (= [[1 2] [3]]
        (nested-limit coll 3))
      (= [[1 2] [3 4]]
        (nested-limit coll 4))
      (= [[1 2] [3 4]]
        (nested-limit coll' 4)))))

(defn message-log-ui
  ([] (message-log-ui nil))
  ([limit]
   (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black scale]} ctx
                    {:keys [world-db]} @*state]
     (let [message-log (data/history-log-entries world-db)
           message-log' (if limit (take limit message-log) message-log)]
       (ui/column
         (ui/gap 0 padding)
         (ui/halign 0.5
           (ui/label "[ Message Log ]" {:font font-small :paint fill-black}))
         (ui/gap 0 (* padding 2))
         (ui/halign 0.5
           (ui/halign 0.5
             (ui/column
               [:stretch 1
                (ui/column
                  (interpose (ui/gap 2 padding)
                    (map
                      (fn [message]
                        (let [border (doto (Paint.)
                                       (.setColor (unchecked-int 0xFF000000))
                                       (.setMode PaintMode/STROKE)
                                       (.setStrokeWidth (* 1 scale)))]
                          (ui/halign 0.5
                            (ui/fill border
                              (ui/padding 5 5
                                (ui/label (str message) {:font font-small :paint fill-black}))))))
                      message-log')))]))))))))

(def top-bar-ui
  (ui/dynamic ctx [{:keys [font-small fill-black fill-yellow fill-white scale]} ctx]
    (ui/column
      [:stretch 1
       (ui/padding 0 0 0 10
         (ui/fill fill-yellow
           (ui/row
             (ui/padding 10 10
               (ui/dynamic ctx [tick (:tick @*state)]
                 (ui/label (str "Day " (inc tick)) {:font font-small :paint fill-black})))
             [:stretch 1 nil]
             (ui/fill fill-white
               (ui/clickable
                 #(reset! *state (new-state))
                 (ui/padding 10 10
                   (ui/label "↻ Restart" {:font font-small :paint fill-black})))))))])))

(def economy-ui-view
  (ui/on-key-down (juxt on-key-pressed-svg-impl on-key-pressed-mini-panel-impl)
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui x-scale y-scale]} ctx
                       {:keys [world-db camera tick svg-xyz]} @*state]
        (let [font-default (Font. face-default (float (* 18 scale)))
              font-large (Font. ^Typeface face-default (float (* scale 26)))
              font-small (Font. ^Typeface face-default (float (* scale 13)))
              fill-black (paint/fill 0xFF000000)
              fill-light-gray (paint/fill 0xFFD4D6DA)
              {::land/keys [economy]} (data/land-data world-db)
              [svg-x svg-y svg-z] svg-xyz
              canvas-width (* x-scale *canvas-width*)
              canvas-height (* y-scale *canvas-height*)]
          (ui/with-context
            {:font-default    font-default
             :font-large      font-large
             :font-small      font-small
             :fill-white      (paint/fill 0xFFFFFFFF)
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  (paint/fill 0xFF777C7E)
             :fill-green      (paint/fill 0xFF6AAA64)
             :fill-yellow     (paint/fill 0xFFC9B457)
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              top-bar-ui
              (ui/gap 0 padding)
              [:stretch 1
               (ui/row
                 [:stretch 1
                  (ui/valign 0.5
                    (ui/halign 0.5
                      (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                        (custom-ui/svg-canvas (economy/->svg economy)))))])])))))))

(defn camera->viewport [camera zoom content-width content-height]
  (let [w content-width h content-height
        vp 0.1 wv (* w vp zoom) hv (* h vp zoom)
        cell (int (* 10 zoom))
        width (quot content-width (* cell 2)) height (quot content-height (* cell 2)) #_(int (quot zoom 1.25))
        half-x (quot width 2) half-y (quot height 2)]
    {:w w :h h :wv wv :hv hv :cell cell :width width :height height :half-x half-x :half-y half-y :size [width height] :center [half-x half-y :+ camera] :lrtb [(- (first camera) half-x) (+ (first camera) half-x) (- (second camera) half-y) (+ (second camera) half-y)]}))

(def world-map
  (ui/dynamic ctx [{:keys [scale x-scale y-scale font-default emoji-font font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black]} ctx
                   {:keys [world-db tick camera zoom] :as state} @*state]
    (let [{::land/keys [terrain units area->units] :as d} (data/land-data world-db)
          territory (into #{} (map :area) (data/land-claims world-db))

          canvas-width (int (* x-scale *canvas-width*))
          canvas-height (int (* y-scale *canvas-height*))

          {:keys [cell lrtb]} (camera->viewport camera zoom canvas-width canvas-height)

          [left right top bottom] lrtb

          map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. emoji-face (float (* scale 8 zoom)))

          unit-data (fn [_tile x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x ;(+ (int (- x half-vw)) camera-x)
                            loc-y y ;(+ (int (- y half-vh)) camera-y)
                            loc [loc-x loc-y]
                            path [loc-y loc-x]

                            biome (get-in terrain path)

                            territory? (contains? territory loc)
                            {::civ/keys [symbol tint] :as civ} (when territory? (data/land-area->civ world-db loc))

                            things (data/land-area world-db [x y])
                            size (count things)
                            thing (when-not (zero? size) (:glyph (nth things (rem tick size))))]
                        (cond
                          thing [thing (if territory? tint (land/render-tile-colour biome)) emoji-font emoji-offset-x emoji-offset-y]
                          civ [symbol tint map-font font-offset-x font-offset-y]
                          territory? ["" tint map-font font-offset-x font-offset-y]
                          :else ["" (land/render-tile-colour biome) map-font font-offset-x font-offset-y])))]
      (ui/column
        (interpose (ui/gap 0 0)
          (for [y-idx (range top bottom)]
            (ui/row
              (interpose (ui/gap 0 0)
                (for [x-idx (range left right)]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [_ (when hovered?
                                (swap! *state assoc :hover-loc [x-idx y-idx]))
                            tile (get-in terrain [y-idx x-idx])
                            [glyph tile-colour font] (unit-data tile x-idx y-idx)]
                        (ui/fill (if hovered?
                                   (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                   (paint/fill tile-colour))
                          (ui/width cell
                            (ui/halign 0.5
                              (ui/height cell
                                (ui/valign 0.5
                                  (ui/label glyph {:font font :paint fill-white}))))))))))))))))))

(def map-ui-view
  (ui/on-key-down (juxt on-key-pressed-impl on-key-pressed-mini-panel-impl)
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx
                       {:keys [camera tick]} @*state]
        (let [font-default (Font. face-default (float (* 18 scale)))
              font-large (Font. ^Typeface face-default (float (* scale 26)))
              font-small (Font. ^Typeface face-default (float (* scale 13)))

              fill-black (paint/fill 0xFF000000)
              fill-light-gray (paint/fill 0xFFD4D6DA)]
          (ui/with-context
            {:font-default    font-default
             :font-large      font-large
             :font-small      font-small
             :fill-white      (paint/fill 0xFFFFFFFF)
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  (paint/fill 0xFF777C7E)
             :fill-green      (paint/fill 0xFF6AAA64)
             :fill-yellow     (paint/fill 0xFFC9B457)
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              top-bar-ui
              (ui/gap 0 padding)
              [:stretch 1 nil]
              world-map
              (ui/padding 5
                (ui/label (str "👋🌲🌳Camera: " (pr-str camera) " Year: " tick #_(when controlling (str " controlling " controlling))) {:font font-small :paint fill-black}))
              (ui/padding 5
                (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") {:font font-small :paint fill-black}))
              (ui/padding 5
                (ui/label (str "[WASD] or arrow keys: Pan the camera, [-]: Zoom Out, [+]: Zoom In") {:font font-small :paint fill-black})))))))))

(def messages-ui-view
  (ui/on-key-down #(on-key-pressed-impl (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx]
        (let [font-small (Font. ^Typeface face-default (float (* scale 13)))
              fill-black (paint/fill 0xFF000000)
              fill-yellow (paint/fill 0xFFC9B457)
              fill-light-gray (paint/fill 0xFFD4D6DA)]
          (ui/with-context
            {:font-large (Font. ^Typeface face-default (float (* scale 26)))
             :font-small font-small
             :fill-white (paint/fill 0xFFFFFFFF)
             :fill-black fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray (paint/fill 0xFF777C7E)
             :fill-green (paint/fill 0xFF6AAA64)
             :fill-yellow fill-yellow
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              top-bar-ui
              [:stretch 1
               (ui/vscrollbar
                 (ui/vscroll
                   (ui/column
                     (message-log-ui))))])))))))

(def basic-ui-view
  (ui/dynamic ctx [{:keys [scale x-scale y-scale]} ctx
                   {:keys [camera tick zoom]} @*state]
    (let [font-small (Font. ^Typeface face-default (float (* scale 13)))
          map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. emoji-face (float (* scale 8 zoom)))

          fill-white (paint/fill 0xFFFFFFFF)
          fill-black (paint/fill 0xFF000000)
          fill-light-gray (paint/fill 0xFFD4D6DA)
          fill-dark-gray (paint/fill 0xFF777C7E)
          fill-green (paint/fill 0xFF6AAA64)
          fill-yellow (paint/fill 0xFFC9B457)

          canvas-width (int (* x-scale *canvas-width*))
          canvas-height (int (* y-scale *canvas-height*))

          {:keys [cell lrtb]} (camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context {:font-small font-small
                        :map-font map-font
                        :emoji-font emoji-font
                        :lrtb lrtb
                        :cell cell
                        :tick tick
                        :fill-white fill-white
                        :fill-black fill-black
                        :fill-light-gray fill-light-gray
                        :fill-dark-gray fill-dark-gray
                        :fill-green fill-green
                        :fill-yellow fill-yellow}
        (ui/padding 20
          basic/ui-view)))))

(def ui-views
  ;; exploiting the fact that as long as array-map doesn't grow, it keeps insertion order
  (array-map
    "Basic" basic-ui-view
    "Map" map-ui-view
    "Economy" economy-ui-view
    "Log" messages-ui-view))

(def *selected-ui-view (atom (ffirst ui-views)))

(defonce *floating (atom false))

(add-watch *floating ::window
  (fn [_ _ _ floating]
    (when-some [window @*window]
      (if floating
        (window/set-z-order window :floating)
        (window/set-z-order window :normal)))))

(declare *menu)

(def game-screen
  (ui/dynamic ctx [scale (:scale ctx)
                   player-hp (:player-hp @*state)]
    (let [font-ui (Font. face-default (float (* 13 scale)))
          leading (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          emoji-font (Font. emoji-face (float 72))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/with-context {:face-ui face-default
                        :font-ui font-ui
                        :leading leading
                        :fill-text fill-text}
        (ui/row
          (ui/column
            (ui/padding 2 leading
              (ui/label game-glyph {:font emoji-font :paint fill-text}))
            [:stretch 1
             (ui/vscrollbar
               (ui/vscroll
                 (ui/column
                   (for [[name _ui] ui-views]
                     (ui/clickable
                       #(reset! *selected-ui-view name)
                       (ui/dynamic ctx [selected? (= name @*selected-ui-view)
                                        hovered? (:hui/hovered? ctx)]
                         (let [label (ui/padding 20 leading
                                       (ui/label name {:font font-ui :paint fill-text}))]
                           (cond
                             selected? (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFB2D7FE))) label)
                             hovered? (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA))) label)
                             :else label))))))))]
            (ui/padding 10 10
              (cui/atom-checkbox *floating "On top")))
          [:stretch 1
           (ui/dynamic _ [name @*selected-ui-view]
             (ui-views name))])))))

(def start-screen
  (ui/valign 0.5
    (ui/halign 0.5
      (ui/column
        (ui/padding 20
          (ui/valign 0.5
            (ui/halign 0.5
              (ui/label "Fruit Economy"))))
        (ui/button
          #(reset! *menu game-screen)
          (ui/padding 80 10 80 10
            (ui/label "Start")))))))

(defonce *menu (atom (if-not (debug?) game-screen start-screen)))

#_  ;; For debugging start-screen
(reset! *menu start-screen)

(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-ui   (Font. face-default (float (* 13 scale)))
          leading   (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (paint/fill 0xFF000000)]
      (ui/with-context {:face-ui        face-default
                        :font-ui        font-ui
                        :leading        leading
                        :fill-text      fill-text
                        :fill-cursor    fill-text
                        :fill-selection (paint/fill 0xFFB1D7FF)}
        (ui/dynamic ctx [screen @*menu]
          screen)))))

(comment
  (window/request-frame @*window))

(defn on-paint [window ^Canvas canvas]
  (canvas/clear canvas 0xFFF0F0F0)
  (let [bounds (window/content-rect window)
        {screen :work-area} (app/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))
        xy-scale (max x-scale y-scale)
        ctx {:bounds bounds :scale (window/scale window) :x-scale x-scale :y-scale y-scale :xy-scale xy-scale}
        app app]
    (hui/draw app ctx (IRect/makeXYWH 0 0 (:width bounds) (:height bounds)) canvas)
    (window/request-frame window)))

(defn on-event [window event]
  (when-let [changed? (hui/event app event)]
    (window/request-frame window)))

(defn on-resize [window]
  (let [[min-width min-height] [600 400]
        [window-width window-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) (window/window-rect window))
        bounds (window/content-rect window)
        [content-width content-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) bounds)
        {:keys [init-cell zoom]} @*state
        scale (max (float (/ *canvas-width* content-width)) (float (/ *canvas-height* content-height)))
        {screen :work-area} (app/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))

        ;; we're calling long because drawRect appears to draw at whole numbers so if we want no gaps,
        ;;   we should pass it whole numbers
        ;; TODO: need better names here, we want to be able to disambiguate the size of the tile, whether it's scaled or not and what those things depend on.
        cell' (* init-cell zoom)
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
  (let [window-width width
        window-height height
        window-left (- right window-width)
        window-top (-> y
                     (+ height)
                     (- (/ window-height 2)))]
    (doto window
      (window/set-window-size window-width window-height)
      (window/set-window-position window-left window-top))))

(defn small-window [window {:keys [width height right y] :as _work-area}]
  (let [window-width (/ width 2)
        window-height (/ height 2)
        window-left (- right window-width)
        window-top (-> y
                     (+ (/ height 2))
                     (- (/ window-height 2)))]
    (doto window
      (window/set-window-size window-width window-height)
      (window/set-window-position window-left window-top))))

(defn make-window []
  (let [{:keys [work-area]} (app/primary-screen)
        window-size-fn (if (debug?) small-window screen-sized-window)]
    (doto
      (window/make
        {:on-close (if (debug?) #(reset! *window nil) #(System/exit 0))
         :on-paint #'on-paint
         :on-event #'on-event
         :on-resize #'on-resize})
      (window/set-title "Fruit Economy 👋")
      (window-size-fn work-area)
      (window/set-visible true))))

(defn set-interval [callback ms]
  (future (while true (do (Thread/sleep ms) (callback)))))

(def clock (atom nil))

(defn tick-clock []
  (let [{:keys [tick-ms last-tick paused?]} @*state
        now (System/currentTimeMillis)]
    (when (and
            (not paused?)
            (> (- now last-tick) tick-ms))
      (swap! *state on-tick now))))

(defn start-clock [ms]
  (reset! clock (set-interval tick-clock ms)))

(defn stop-clock []
  (future-cancel @clock))


(defn -main [& args]
  ;; TODO: Display somewhere in the UI
  (println (str "VERSION: " (env :game-version) (when (debug?) "\nDEBUG BUILD")))
  (when (debug?)
    ;; Swap to require and resolve in one step!
    (future (apply (requiring-resolve 'nrepl.cmdline/-main) args)))
  (start-clock 1000)
  (app/start #(reset! *window (make-window))))

;; Helps with REPL dev, on ns load forces a redraw
(some-> @*window window/request-frame)

(comment
  (do
    (app/doui (some-> @*window window/close))
    (reset! *window (hui/doui (make-window))))

  (app/doui (window/set-z-order @*window :normal))
  (app/doui (window/set-z-order @*window :floating)))

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
