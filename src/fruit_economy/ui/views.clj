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


(def basic-ui-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow
                           green-colour yellow-colour dark-gray-colour]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   world @basic/*world]
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
              basic/ui-view)
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
                              #(swap! basic/*world assoc :selected-city eid)
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
                                #(swap! basic/*world assoc :selected-market market)
                                {:bg-active green-colour
                                 :bg-hovered yellow-colour
                                 :bg dark-gray-colour
                                 :p 10 :border-radius 0}
                                (ui/label (market-label-fn market) {:font font-small :paint fill-white})))))))
                    (when (and selected-city selected-market)
                      (ui/dynamic ctx [{:keys [world-db selected-city selected-market]} ctx]
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

                            (ui/label (pr-str city))
                            (ui/height 100
                              (ui/row
                                (ui/vscrollbar
                                  (ui/vscroll
                                    (ui/column
                                      (interpose (ui/fill fill-dark-gray
                                                   (ui/gap 0 4))
                                        (for [market cities]
                                          (show-map-ui market font-small fill-black))))))))))))))))))))))

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
    "Basic" basic-ui-view
    "Log" messages-ui-view))
