(ns fruit-economy.ui.views
  (:require [io.github.humbleui.ui :as ui]
            [fruit-economy.state :as state]
            [fruit-economy.db.core :as db]
            [fruit-economy.data.core :as data]
            [fruit-economy.ui.bits :as ui.bits :refer [padding show-map-ui]]
            [fruit-economy.ui.parts :as ui.parts]
            [fruit-economy.ui.controls :refer [on-key-pressed-impl]]
            [fruit-economy.screen-ui :as screen-ui]
            [fruit-economy.sim.basic :as basic])
  (:import [io.github.humbleui.skija Font Typeface]))


(defn market-label-fn [market]
  (println market)
  (condp = market :food "Food" :clothes "Clothes" :labour "Labour"))

(defn market-keys-fn [market key]
  (condp = [market key]
    [:food :producers] [:food-factory]
    [:food :demanders] [:peep]
    [:food :last-produced] :food/last-produced
    [:food :last-consumed] :food/last-consumed
    [:food :price] :food/price
    [:food :price-history] :food/price-history
    [:clothes :producers] [:clothes-factory]
    [:clothes :demanders] [:peep]
    [:clothes :last-produced] :clothes/last-produced
    [:clothes :last-consumed] :clothes/last-consumed
    [:clothes :price] :clothes/price
    [:clothes :price-history] :clothes/price-history
    [:labour :producers] [:peep]
    [:labour :demanders] [:food-factory :clothes-factory]
    [:labour :last-produced] :labour/last-produced
    [:labour :last-consumed] :labour/last-consumed
    [:labour :price] :labour/price
    [:labour :price-history] :labour/price-history))

(defn lookup-by-kind [world-db] (fn [kind] (data/lookup-avet world-db :kind kind)))

(def kind->emoji
  {:food-factory "🍲🏭" :clothes-factory "👚🏭" :peep "🧑"})

(def market-production-view
  (ui/dynamic ctx [{:keys [world-db cities selected-city selected-market fill-dark-gray fill-light-gray]} ctx]
     (let [city (db/entity world-db selected-city)

           producers (into [] (comp (map (lookup-by-kind world-db)) cat (map db/touch)) (market-keys-fn selected-market :producers))
           demanders (into [] (comp (map (lookup-by-kind world-db)) cat (map db/touch)) (market-keys-fn selected-market :demanders))

           label (market-label-fn selected-market)
           price ((market-keys-fn selected-market :price) city)
           price-history ((market-keys-fn selected-market :price-history) city)
           produced ((market-keys-fn selected-market :last-produced) city)
           consumed ((market-keys-fn selected-market :last-consumed) city)
           total-production (reduce (fn [v m] (+ v ((market-keys-fn selected-market :last-produced) m))) 0 cities)
           percent-of-production (if (zero? total-production) 0 (* (/ produced total-production) 100))]
       (ui/column
         (ui/row
           (ui/padding 20
             (ui/column
               (ui/label label)
               (ui/gap 0 5)
               (ui/label (clojure.pprint/cl-format nil "~,2f% of world production" percent-of-production))))
           (ui/with-context
             {:price price :price-history price-history}
             screen-ui/price-chart-ui))
         (ui/row
           (ui/padding 20
             (ui/column
               (ui/label "Produced by:")
               (ui/gap 0 4)
               (ui/height 100
                 (ui/row
                   (ui/vscrollbar
                     (ui/vscroll
                       (ui/column
                         (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                           (for [producer producers]
                             (ui/tooltip {:anchor :top-right :shackle :top-right}
                               (ui/label (pr-str producer))
                               (ui/fill fill-light-gray
                                 (ui/padding 150 20 150 20
                                   (ui/label (str ((:kind producer) kind->emoji) " " (pr-str (select-keys producer [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed]))))))))))))))
               (ui/padding 20
                 (ui/label (str "Total Produced: " produced)))))
           (ui/padding 20
             (ui/column
               (ui/label "Used by:")
               (ui/gap 0 4)
               (ui/height 100
                 (ui/row
                   (ui/vscrollbar
                     (ui/vscroll
                       (ui/column
                         (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                           (for [demander demanders]
                             (ui/tooltip {:anchor :top-right :shackle :top-right}
                               (ui/label (pr-str demander))
                               (ui/fill fill-light-gray
                                 (ui/padding 150 20 150 20
                                   (ui/label (str ((:kind demander) kind->emoji) " " (pr-str (select-keys demander [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed]))))))))))))))
               (ui/padding 20
                 (ui/label (str "Total Used: " consumed))))))

         #_#_#_
         (ui/label "Cities:")
         (ui/gap 0 2)
         (ui/height 100
           (ui/row
             (ui/vscrollbar
               (ui/vscroll
                 (ui/column
                   (interpose (ui/fill fill-dark-gray
                                (ui/gap 0 4))
                     (for [{:keys [settlement/name] :as market} cities]
                       (ui/column
                         (ui/label name)
                         (ui/gap 0 2)
                         ;;(ui/label (pr-str (db/touch market)))
                         ;;(ui/gap 0 2)
                         (show-map-ui market font-small fill-black)))))))))))))

(defn city-view [settlement]
  (ui/dynamic ctx [{:keys [font-small fill-black fill-red fill-green]} ctx
                   {:keys [world-db map-view]} @state/*world]
    (ui/column
      (ui/row
        (interpose (ui/gap 4 0)
          (for [k [:settlement/name :settlement/place]]
            (ui/label (str (get settlement k)) {:font font-small :paint fill-black}))))
      (ui/gap 0 4)
      (ui/row
        (ui/column
          (ui/label (str "Money " (:money (first (get settlement :_governs)))))
          (ui/gap 0 2)
          (ui/row
            [:stretch 4 (ui/label "Food")]
            [:stretch 2 (ui/label (str (get settlement :food/price) "g") {:paint (ui.bits/compare->fill settlement :food/last-demand :food/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
            [:stretch 1 (ui/label (let [vel (get settlement :food/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
            [:stretch 1 nil]
            [:stretch 4 (ui/label "Clothes")]
            [:stretch 2 (ui/label (str (get settlement :clothes/price) "g") {:paint (ui.bits/compare->fill settlement :clothes/last-demand :clothes/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
            [:stretch 1 (ui/label (let [vel (get settlement :clothes/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
            [:stretch 1 nil]
            [:stretch 4 (ui/label "Labour")]
            [:stretch 2 (ui/label (str (get settlement :labour/price) "g") {:paint (ui.bits/compare->fill settlement :labour/last-demand :labour/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
            [:stretch 1 (ui/label (let [vel (get settlement :labour/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
            [:stretch 1 nil])
          (ui/gap 0 2)
          #_
          (interpose (ui/gap 0 4)
            (for [columns [["" "Food" "Clothes" "Labour"] ["Price" :food/price :clothes/price :labour/price] ["Float" :food/price-float :clothes/price-float :labour/price-float] ["Velocity" :food/price-velocity :clothes/price-velocity :labour/price-velocity] ["Demand" :food/last-demand :clothes/last-demand :labour/last-demand] ["Supply" :food/last-supply :clothes/last-supply :labour/last-supply]]]
              (ui/row
                (interpose (ui/gap 4 0)
                  (for [k columns
                        :let [label (if (string? k) k (str (get settlement k)))]]
                    (ui/padding 0 0 40 0
                      (ui/label label {:font font-small :paint fill-black})))))))
          (ui/padding 10
            (ui/column
              (interpose (ui/gap 4 0)
                (for [peep (mapv db/touch (data/lookup-avet world-db :hometown (:db/id settlement)))]
                  (ui/label (str (select-keys peep [:money :health :food :clothes :inventory :last-sold :planning :kind])) {:font font-small :paint fill-black}))))))))))


(def ui-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-green fill-yellow fill-light-gray fill-dark-gray]} ctx
                   {:keys [world-db map-view]} @state/*world]
    (ui/column
      (ui/row
        ui.parts/map-ui-view
        (ui/padding 10
          (ui/height 200
            (ui/column
              (ui/dynamic ctx [settlements (basic/settlements-q world-db nil)]
                (if-not (seq settlements)
                  ui.parts/chart-view
                  (ui/column
                    (ui/fill fill-light-gray
                      (ui/padding 4
                        (ui/label "Settlement Info")))
                    (ui/vscrollbar
                      (ui/vscroll
                        (ui/column
                          (interpose (ui/gap 4 0)
                            (for [settlement settlements]
                              (ui/padding 4
                                (basic/city-view settlement)))))))
                    (ui/gap 0 10)
                    basic/chart-view)))))))
      (ui/row
        (ui/clickable
          #(swap! state/*world assoc :map-view :default-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :default-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🗺️" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :temp-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :temp-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌡" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :elev-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :elev-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "📏" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :climate-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :climate-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌍" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :forage-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :forage-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🚜" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :mine-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :mine-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "⛏️" {:font font-small :paint fill-white}))))))))))

(def basic-ui-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow
                           green-colour yellow-colour dark-gray-colour]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   world @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            (ui/padding 20
              ui-view)
            (ui/dynamic ctx [{:keys [dbs world-db selected-city selected-market]} ctx]
              (let [db world-db
                    cities (data/lookup-avet db :kind :city)]
                (if-not (seq cities)
                  (ui/gap 0 0)
                  (ui/column
                    (ui/row
                      (interpose (ui/fill fill-dark-gray
                                   (ui/gap 0 0))
                        (for [city cities
                              :let [eid (:db/id city)]]
                          (ui/with-context
                            {:hui/active? (= selected-city eid)}
                            (ui/button
                              #(swap! state/*world assoc :selected-city eid)
                              {:bg-active green-colour
                               :bg-hovered yellow-colour
                               :bg dark-gray-colour
                               :p 10 :border-radius 0}
                              (ui/label (:settlement/name city) {:font font-small :paint fill-white}))))))
                    (ui/row
                      (interpose (ui/fill fill-dark-gray
                                   (ui/gap 0 0))
                        (for [market [:food :clothes :labour]]
                          (ui/column
                            (ui/with-context
                              {:hui/active? (= selected-market market)}
                              (ui/button
                                #(swap! state/*world assoc :selected-market market)
                                {:bg-active green-colour
                                 :bg-hovered yellow-colour
                                 :bg dark-gray-colour
                                 :p 10 :border-radius 0}
                                (ui/label (market-label-fn market) {:font font-small :paint fill-white})))))))
                    (when (and selected-city selected-market)
                      (ui/with-context
                        {:cities cities}
                        market-production-view))))))))))))

(def messages-ui-view
  (ui/on-key-down on-key-pressed-impl
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx]
        (ui/column
          ui.parts/top-bar-ui
          [:stretch 1
           (ui/vscrollbar
             (ui/vscroll
               (ui/column
                 (ui.parts/message-log-ui))))])))))


(def ui-views
  ;; exploiting the fact that as long as array-map doesn't grow, it keeps insertion order
  (array-map
    "World" basic-ui-view
    #_#_
    "Log" messages-ui-view))
