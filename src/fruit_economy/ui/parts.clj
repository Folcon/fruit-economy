(ns fruit-economy.ui.parts
  (:require [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [fruit-economy.state :as state]
            [fruit-economy.clock :as clock]
            [fruit-economy.colour :refer [colour colour-noise]]
            [fruit-economy.humble-ui :as custom-ui]
            [fruit-economy.data.core :as data]
            [fruit-economy.ui.bits :as ui.bits :refer [padding]]
            [fruit-economy.land :as land]
            [fruit-economy.sim.basic :as basic]
            [taoensso.timbre :refer [log]])
  (:import [io.github.humbleui.skija Paint PaintMode]))


;; TODO: Create a UI element + add button when nil to select player
(def player-stats-ui
  (ui/dynamic ctx [{:keys [font-small fill-white fill-black world-db player-eid]} ctx]
    (if player-eid
      (let [player (data/entity world-db player-eid)]
        (ui/row
          (ui/padding 10 10
            (ui/label (str "Nation " (get-in player [:governs :settlement/name])) {:font font-small :paint fill-black}))
          (ui/padding 10 10
            (ui/label (str "Money $" (:money player)) {:font font-small :paint fill-black}))))
      (ui/fill fill-white
        (ui/clickable
          ;; Prevent circular dependency by only requiring later
          #(swap! state/*menu assoc :screen @(requiring-resolve 'fruit-economy.ui.screens/start-screen))
          (ui/padding 10 10
            (ui/label "Select Player" {:font font-small :paint fill-black})))))))

(def time-controls
  (ui/dynamic ctx [{:keys [font-small fill-white fill-black fill-yellow fill-green fill-dark-gray]} ctx
                   {:keys [paused? speed-ms]} @state/*menu]
    (ui/row
      (ui/clickable
        #(swap! state/*menu update :paused? not)
        (ui/hoverable
          (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
            (ui/fill (cond hovered? fill-yellow paused? fill-dark-gray :else fill-white)
              (ui/padding 10 10
                (ui/label (if paused? "▶️" "⏸️") {:font font-small :paint fill-black}))))))
      (if paused?
        (ui/row
          (ui/clickable
            basic/do-tick-world
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+1 Day" {:font font-small :paint fill-white}))))))
          (ui/clickable
            basic/tick-world-10x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+10 Days" {:font font-small :paint fill-white}))))))
          (ui/clickable
            basic/tick-world-100x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+100 Days" {:font font-small :paint fill-white}))))))
          (ui/clickable
            basic/tick-world-1000x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+1000 Days" {:font font-small :paint fill-white})))))))
        (ui/row
          (ui/clickable
            #(swap! state/*menu assoc :speed-ms 5000)
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (cond hovered? fill-yellow (= speed-ms 5000) fill-green :else fill-white)
                  (ui/padding 10 10
                    (ui/label "+" {:font font-small :paint fill-black}))))))
          (ui/clickable
            #(swap! state/*menu assoc :speed-ms 3000)
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (cond hovered? fill-yellow (= speed-ms 3000) fill-green :else fill-white)
                  (ui/padding 10 10
                    (ui/label "++" {:font font-small :paint fill-black}))))))
          (ui/clickable
            #(swap! state/*menu assoc :speed-ms 2000)
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (cond hovered? fill-yellow (= speed-ms 2000) fill-green :else fill-white)
                  (ui/padding 10 10
                    (ui/label "+++" {:font font-small :paint fill-black})))))))))))

(def top-bar-ui
  (ui/dynamic ctx [{:keys [font-small fill-white fill-black fill-yellow day]} ctx]
    (ui/column
      [:stretch 1
       (ui/padding 0 0 0 10
         (ui/fill fill-yellow
           (ui/row
             (ui/padding 10 10
               (ui/label (str "Day " day) {:font font-small :paint fill-black}))
             player-stats-ui
             [:stretch 1 nil]
             time-controls
             (ui/fill fill-white
               (ui/clickable
                 #(reset! state/*world (basic/reset-world))
                 (ui/padding 10 10
                   (ui/label "↻ Restart" {:font font-small :paint fill-black})))))))])))

(defn message-log-ui
  ([] (message-log-ui nil))
  ([limit]
   (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black scale]} ctx
                    {:keys [world-db]} @state/*state]
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

(def map-ui-view
 (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black cell tick lrtb map-font emoji-font]} ctx
                  {:keys [world-db map-view]} @state/*world]
   (let [[left right top bottom] lrtb
         terrain-tint (condp = map-view
                        :temp-view (fn [tile] (colour (colour-noise (get tile :temp)) 0 0))
                        :elev-view (fn [tile] (colour 0 (colour-noise (get tile :elev)) 0))
                        :climate-view (fn [tile] (colour (colour-noise (get tile :temp)) (colour-noise (get tile :elev)) 0))
                        :forage-view (fn [tile] (colour 0 (* (get tile :food 0) 40) 0))
                        :mine-view (fn [tile] (colour 0 (* (get tile :rock 0) 40) 0))
                        (fn [tile] (land/render-tile-colour (get tile :biome))))
         unit-data (fn [x y]
                     (let [;; pixel-x and pixel-y
                           coord-x x
                           coord-y y
                           coord [coord-x coord-y]

                           tile (basic/coord-q world-db coord)

                           things (when tile (basic/units-q world-db coord))
                           size (count things)
                           {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))

                           {name :settlement/name :as settlement} (when tile (first (basic/settlements-q world-db coord)))]
                       (when thing
                         (log :info :wealth thing (get thing :wealth))
                         (log :info :wealth settlement thing (get thing :wealth)))
                       (cond
                         thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                         (seq settlement) [name (colour 0 0 155) map-font font-offset-x font-offset-y]
                         :else ["" (terrain-tint tile) map-font font-offset-x font-offset-y])))]
     (ui/column
       (interpose (ui/gap 0 0)
         (for [y-idx (range top bottom)]
           (ui/row
             (interpose (ui/gap 0 0)
               (for [x-idx (range left right)]
                 (ui/hoverable
                   (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                     (let [[glyph tile-colour font] (unit-data x-idx y-idx)]
                       (ui/fill (if hovered?
                                  (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                  (paint/fill tile-colour))
                         (ui/width cell
                           (ui/halign 0.5
                             (ui/height cell
                               (ui/valign 0.5
                                 (ui/label glyph {:font font :paint fill-white}))))))))))))))))))

(def chart-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-black fill-green fill-yellow fill-light-gray fill-dark-gray]} ctx
                   {:keys [world-db]} @state/*world]
    (let [units (basic/units-q world-db nil)
          wealth-freqs (->> units
                         (map :wealth)
                         (frequencies)
                         (into (sorted-map))
                         (vec))
          size (count wealth-freqs)
          mx 30
          wealth-freqs-filtered (into [["$" "#"]]
                                  (if (< mx size)
                                    (into (subvec wealth-freqs 0 (- mx 3)) cat [[["" ""] ["..." (str (- size (dec mx)) "+")] ["" ""]] (subvec wealth-freqs (- size 2) size)])
                                    wealth-freqs))
          freq-avg-fn (fn [freq]
                        (let [[total size] (reduce (fn [v [val count]] (-> v (update 0 + (* val count)) (update 1 + count))) [0 0] freq)]
                          (float (/ total size))))
          padding 4]
      (ui/column
        (ui/fill fill-light-gray
          (ui/padding 4
            (ui/label "Meganeura Info")))
        (ui/gap 0 2)
        (ui/tooltip
          (ui/fill fill-light-gray
            (ui/padding 10
              (ui/column
                (ui/label "These are the titanic creatures beneath who's trails new settlements can form" {:font font-small}))))
          (if-not (seq units)
            (ui/gap 0 0)
            (ui/column
              (ui/row
                (ui/label (str "# " (reduce + (mapv second wealth-freqs))) {:font font-small :paint fill-black})
                (ui/gap padding 2)
                (ui/label (str "μ 👁: " (freq-avg-fn (frequencies (map :vision units)))) {:font font-small :paint fill-black})
                (ui/gap padding 2)
                (ui/label (str "μ 🍖: " (freq-avg-fn (frequencies (map :hunger units)))) {:font font-small :paint fill-black})
                (ui/gap 2 padding))
              (ui/padding 5
                (ui/column
                  (interpose (ui/gap padding 2)
                    (for [entry wealth-freqs-filtered
                          :let [[_wealth quantity] entry]]
                      (ui/row
                        (interpose (ui/gap 12 0)
                          (into
                            (if (number? quantity)
                              (custom-ui/<>
                                (ui/fill (paint/fill (colour 150 150 150))
                                  (ui/gap (* quantity 2) 2)))
                              (custom-ui/<>
                                (ui/gap 0 0)))
                            (for [val entry]
                              (ui/width 20
                                (ui/label (str val) {:font font-small :paint fill-black})))))))))))))))))

