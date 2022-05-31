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
   [fruit-economy.state :as state]
   [fruit-economy.clock :as clock]
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
   [fruit-economy.sim.basic :as basic]
   [fruit-economy.ui.bits :as ui.bits :refer [padding show-map-ui]]
   [fruit-economy.ui.parts :as ui.parts]
   [fruit-economy.ui.controls :refer [on-key-pressed-impl]]
   [fruit-economy.ui.views :as ui.views]
   [fruit-economy.ui.screens :as ui.screens]
   [fruit-economy.screen-ui :as screen-ui]
   [fruit-economy.utils :refer [suppress-print]]
   [taoensso.timbre :refer [set-level! log]])
  (:import
   [io.github.humbleui.jwm EventMouseButton EventMouseMove EventMouseScroll EventKey KeyModifier]
   [io.github.humbleui.skija Surface Canvas Color4f FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint IRect Rect])
  (:gen-class))


(set! *warn-on-reflection* true)

(defn debug? [] (= (env :debug?) "true"))

;; TODO: Make this compile time using env vars for prod builds
(when-not (debug?)
  (set-level! :warn))

(defonce font-mgr (FontMgr/getDefault))

(when (debug?)
  (let [instrument (requiring-resolve 'clojure.spec.test.alpha/instrument)]
    (instrument 'fruit-economy.sim.market/load-order)))


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

(when (nil? @state/*state)
  (reset! state/*state (new-state)))
;; END GAME STATE

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
  (let [{:keys [camera peep world world-db zoom cell hovering viewport-width viewport-height half-vw half-vh tick paused? tick-ms last-tick render-ms last-render] :as state} @state/*state

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
      (swap! state/*state on-render now))

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

(defn draw-mini-panel-impl
  "Render for small panel, this will be a sort of global render
  context to ensure stuff like ticks still keep happening"
  [^Canvas canvas window-width window-height]
  (let [{:keys [tick tick-ms last-tick paused?] :as _state} @state/*state
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
      (swap! state/*state on-tick now))

    #_(println :mini-panel tick)

    ;; Put the clear here to show where the mini panel is,
    ;;   will probably use it in some other way later
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (.drawString canvas game-glyph emoji-offset-x (+ emoji-offset-y (/ cell-y 2)) emoji-font fill-text)))

(declare on-resize)

(defn on-key-pressed-mini-panel-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @state/*state]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (println :mini-panel raw-event))

    (when (= event :hui/mouse-button)
      (println :mini-panel raw-event))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :mini-panel key)
      (println (:peep @state/*state))
      (condp contains? key
        #{:q}
        (let [history-index (get state :history-index)
              history (data/history-log-entries (:world-db state))
              history-size (count history)]
          (swap! state/*state assoc :history-index (min (dec history-size) (inc history-index))))

        #{:e}
        (let [history-index (get state :history-index)]
          (swap! state/*state assoc :history-index (max 0 (dec history-index))))

        #{:t}
        (swap! state/*state update :economy? not)

        #{:y}
        (swap! state/*state update :world-db data/step-economy)

        #{:p}
        (swap! state/*state update :paused? not)

        #{:close-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! state/*state assoc :civ-index (rem (inc civ-index) size)))

        #{:open-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! state/*state assoc :civ-index (rem (+ (dec civ-index) size) size)))

        ;#{:digit5}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)
        ;
        ;      ordered-civs (data/ordered-civs (:world-db state))
        ;      controlling-civ-id (nth ordered-civs civ-index)
        ;      controlling-civ' (data/entity (:world-db state) controlling-civ-id)]
        ;  (when controlling-civ
        ;    (-> state/*state
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
        ;    (swap! state/*state update :world civ-actions/expand-territory controlling-civ)))
        ;
        ;#{:digit7}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)]
        ;  (when controlling-civ
        ;    (swap! state/*state update :world civ-actions/improve-tech-level controlling-civ)))

        #{:r}
        (do
          (reset! state/*state (new-state))
          (on-resize @state/*window))

        ;; (println :mini-panel key)
        nil))))

(defn on-key-pressed-svg-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @state/*state]

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
          (swap! state/*state update-in [:svg-xyz 0] + 20))

        #{:a :left}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 0] - 20))

        #{:s :down}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 1] + 20))

        #{:w :up}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 1] - 20))

        #{:minus}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 2] - 0.1))

        #{:equals}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 2] + 0.1))

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

(def economy-ui-view
  (ui/on-key-down (juxt on-key-pressed-svg-impl on-key-pressed-mini-panel-impl)
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [world-db svg-xyz]} @state/*state]
        (let [{::land/keys [economy]} (data/land-data world-db)
              [svg-x svg-y svg-z] svg-xyz]
          (ui/column
            ui.parts/top-bar-ui
            (ui/gap 0 padding)
            [:stretch 1
             (ui/row
               [:stretch 1
                (ui/valign 0.5
                  (ui/halign 0.5
                    ;; TODO: Current scroll / zoom broken on svg
                    (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                      (custom-ui/svg-canvas (economy/->svg economy)))))])]))))))

(def world-map
  (ui/dynamic ctx [{:keys [scale x-scale y-scale font-default emoji-font font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black]} ctx
                   {:keys [world-db tick camera zoom map-view] :as state} @state/*state]
    (let [{::land/keys [terrain temp elev raw-resource units area->units] :as d} (data/land-data world-db)
          territory (into #{} (map :area) (data/land-claims world-db))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)

          [left right top bottom] lrtb

          map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. emoji-face (float (* scale 8 zoom)))

          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x                         ;(+ (int (- x half-vw)) camera-x)
                            loc-y y                         ;(+ (int (- y half-vh)) camera-y)
                            loc [loc-x loc-y]
                            path [loc-y loc-x]

                            biome (get-in terrain path)

                            ;; temp + elevation map
                            ;; Distribute out food for foraging + rock for crafting
                            local-temp (get-in temp path)
                            local-elev (get-in elev path)
                            raw-resource (get-in raw-resource path)

                            local-temp' (if local-temp (int (quot (* (+ local-temp 1) 255) 2)) 0)
                            local-elev' (if local-elev (int (quot (* (+ local-elev 1) 255) 2)) 0)

                            territory? (contains? territory loc)
                            {::civ/keys [symbol tint] :as civ} (when territory? (data/land-area->civ world-db loc))

                            things (data/land-area world-db [x y])
                            size (count things)
                            thing (when-not (zero? size) (:glyph (nth things (rem tick size))))

                            default-tint (condp = map-view
                                           :temp-view (colour local-temp' 0 0)
                                           :elev-view (colour 0 local-elev' 0)
                                           :climate-view (colour local-temp' local-elev' 0)
                                           :forage-view (colour 0 (* (get raw-resource :food 0) 40) 0)
                                           :mine-view (colour 0 (* (get raw-resource :rock 0) 40) 0)
                                           :default-map-view (land/render-tile-colour biome)
                                           (colour 0 0 0))]
                        (cond
                          thing [thing (if territory? tint (land/render-tile-colour biome)) emoji-font emoji-offset-x emoji-offset-y]
                          civ [symbol tint map-font font-offset-x font-offset-y]
                          territory? ["" tint map-font font-offset-x font-offset-y]
                          :else ["" default-tint map-font font-offset-x font-offset-y])))]
      (ui/column
        (interpose (ui/gap 0 0)
          (for [y-idx (range top bottom)]
            (ui/row
              (interpose (ui/gap 0 0)
                (for [x-idx (range left right)]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [_ (when hovered?
                                (swap! state/*state assoc :hover-loc [x-idx y-idx]))
                            [glyph tile-colour font] (unit-data x-idx y-idx)]
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
      (ui/dynamic ctx [{:keys [scale font-small fill-white fill-black fill-green fill-dark-gray fill-light-gray]} ctx
                       {:keys [camera tick]} @state/*state]
        (ui/column
          ui.parts/top-bar-ui
          (ui/gap 0 padding)
          [:stretch 1 nil]
          world-map
          (ui/padding 5
            (ui/label (str "👋🌲🌳Camera: " (pr-str camera) " Year: " tick #_(when controlling (str " controlling " controlling))) {:font font-small :paint fill-black}))
          (ui/padding 5
            (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") {:font font-small :paint fill-black}))
          (ui/padding 5
            (ui/label (str "[WASD] or arrow keys: Pan the camera, [-]: Zoom Out, [+]: Zoom In") {:font font-small :paint fill-black})))))))


(reset! state/*selected-ui-view (ffirst ui.views/ui-views))

(when (nil? @state/*floating)
  (reset! state/*floating false))

(add-watch state/*floating ::window
  (fn [_ _ _ floating]
    (when-some [window @state/*window]
      (if floating
        (window/set-z-order window :floating)
        (window/set-z-order window :normal)))))

(defn clock-tick-fn []
  (do
    (basic/do-tick-world)
    (window/request-frame @state/*window)))

(add-watch state/*menu ::speed
  (fn [_ _ {old-paused? :paused? :as old} {new-paused? :paused? :as new}]
    (when (not= old-paused? new-paused?)
      (if new-paused?
        (clock/stop-clock)
        (clock/start-clock clock-tick-fn)))))


(when (nil? @state/*menu)
  (reset! state/*menu (if (debug?) {:screen ui.screens/game-screen :started? true :paused? true :speed-ms 5000} {:screen ui.screens/start-screen :started? false :paused? true :speed-ms 5000})))

#_  ;; For debugging start-screen
(reset! state/*menu {:screen ui.screens/start-screen :started? false :paused? true :speed-ms 5000})

(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-ui   (Font. face-default (float (* 13 scale)))
          leading   (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (paint/fill 0xFF000000)
          selection-colour 0xFFB1D7FF
          white-colour 0xFFFFFFFF
          black-colour 0xFF000000
          light-gray-colour 0xFFD4D6DA
          dark-gray-colour 0xFF777C7E
          blue-colour 0xFFB2D7FE
          green-colour 0xFF6AAA64
          yellow-colour 0xFFC9B457
          red-colour 0xFFD53F3F]
      (ui/with-context {:face-default   face-default
                        :emoji-face     emoji-face
                        :game-glyph     game-glyph
                        :face-ui        face-default
                        :font-ui        font-ui
                        :leading        leading
                        :fill-text      fill-text
                        :fill-cursor    fill-text
                        :fill-selection (paint/fill selection-colour)
                        :selection-colour selection-colour

                        :font-default (Font. face-default (float (* 18 scale)))
                        :font-large (Font. ^Typeface face-default (float (* scale 26)))
                        :font-small (Font. ^Typeface face-default (float (* scale 13)))

                        :fill-white (paint/fill white-colour)
                        :fill-black (paint/fill black-colour)
                        :fill-light-gray (paint/fill light-gray-colour)
                        :fill-dark-gray (paint/fill dark-gray-colour)
                        :fill-blue (paint/fill blue-colour)
                        :fill-green (paint/fill green-colour)
                        :fill-yellow (paint/fill yellow-colour)
                        :fill-red (paint/fill red-colour)

                        :white-colour white-colour
                        :black-colour black-colour
                        :light-gray-colour light-gray-colour
                        :dark-gray-colour dark-gray-colour
                        :blue-colour blue-colour
                        :green-colour green-colour
                        :yellow-colour yellow-colour
                        :red-colour red-colour

                        :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
                        :stroke-dark-gray (paint/stroke 0xFF777C7E (* 2 scale))}
        (ui/dynamic ctx [{:keys [screen]} @state/*menu]
          screen)))))

(comment
  (window/request-frame @state/*window))

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
    #_(window/request-frame window)))

(defn on-event [window event]
  (when-let [changed? (hui/event app event)]
    (window/request-frame window)))

(defn on-resize [window]
  (let [[min-width min-height] [600 400]
        [window-width window-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) (window/window-rect window))
        bounds (window/content-rect window)
        [content-width content-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) bounds)
        {:keys [init-cell zoom]} @state/*state
        scale (max (float (/ state/*canvas-width* content-width)) (float (/ state/*canvas-height* content-height)))
        {screen :work-area} (app/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))

        ;; we're calling long because drawRect appears to draw at whole numbers so if we want no gaps,
        ;;   we should pass it whole numbers
        ;; TODO: need better names here, we want to be able to disambiguate the size of the tile, whether it's scaled or not and what those things depend on.
        cell' (* init-cell zoom)
        canvas-width' (long (* x-scale state/*canvas-width*))
        canvas-height' (long (* y-scale state/*canvas-height*))
        viewport-width' (inc (quot canvas-width' cell'))
        viewport-height' (inc (quot canvas-height' cell'))
        half-vw' (quot viewport-width' 2)
        half-vh' (quot viewport-height' 2)]
      :scale scale :cell cell' :canvas-width canvas-width' :canvas-height canvas-height'
    (swap! state/*state assoc
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
        {:on-close (if (debug?) #(reset! state/*window nil) #(System/exit 0))
         :on-paint #'on-paint
         :on-event #'on-event
         :on-resize #'on-resize})
      (window/set-title "Fruit Economy 👋")
      (window-size-fn work-area)
      (window/set-visible true))))


(defn tick-clock []
  (let [{:keys [tick-ms last-tick paused?]} @state/*state
        now (System/currentTimeMillis)]
    (when (and
            (not paused?)
            (> (- now last-tick) tick-ms))
      (swap! state/*state on-tick now))))


(defn -main [& args]
  ;; TODO: Display somewhere in the UI
  (println (str "VERSION: " (env :game-version) (when (debug?) "\nDEBUG BUILD")))
  (when (debug?)
    ;; Swap to require and resolve in one step!
    (future (apply (requiring-resolve 'nrepl.cmdline/-main) args)))
  (app/start #(reset! state/*window (make-window))))

;; Helps with REPL dev, on ns load forces a redraw
(some-> @state/*window window/request-frame)

(comment
  (do
    (app/doui (some-> @state/*window window/close))
    (reset! state/*window (app/doui (make-window))))

  (app/doui (window/set-z-order @state/*window :normal))
  (app/doui (window/set-z-order @state/*window :floating)))

(comment
  (-main)
  (identity @*clicks)
  (get-in @*state [:world ::land/civ-name->civ])
  (identity @state/*window)

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
