(ns fruit-economy.screen-ui
  (:require [io.github.humbleui.ui :as ui]
            [fruit-economy.data.core :as data]
            [fruit-economy.sim.basic :as basic]
            [fruit-economy.humble-ui :as custom-ui]))


;; TODO: Create a UI element + add button when nil to select player
(def player-stats-ui
  (ui/dynamic ctx [{:keys [font-small fill-black world-db player-eid]} ctx]
    (if player-eid
      (let [player (data/entity world-db player-eid)]
        (ui/row
          (ui/padding 10 10
            (ui/label (str "Nation " (get-in player [:governs :settlement/name])) {:font font-small :paint fill-black}))
          (ui/padding 10 10
            (ui/label (str "Money $" (:money player)) {:font font-small :paint fill-black}))))
      (ui/gap 0 0))))

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
             (ui/fill fill-white
               (ui/clickable
                 #(reset! basic/*world (basic/reset-world))
                 (ui/padding 10 10
                   (ui/label "↻ Restart" {:font font-small :paint fill-black})))))))])))


(def price-chart-ui
  (ui/dynamic ctx [{:keys [price price-history]} ctx]
    (ui/padding 20
      (ui/column
        (ui/label (str "Current Price:            " price " g"))
        (ui/height 40
          (if-not (seq price-history)
            (ui/label (pr-str price-history))
            (ui/column
              (let [mx (apply max price-history)
                    limit-fn (fn [val]
                               (reduce
                                 (fn [val [limit div]]
                                   (if (zero? (quot val limit))
                                     val
                                     (reduced div)))
                                 val
                                 [[1000 100] [100 10] [10 nil]]))
                    apply-limit-fn (fn [val limit] (if limit (float (/ val limit)) val))
                    limit (limit-fn mx)]
                #_
                  [:dali/page
                   [:dali/stack
                    {:position [10 10], :direction :right, :anchor :bottom-left, :gap 2}
                    (map (fn [h]
                           [:dali/stack
                            {:direction :up :gap 6}
                            [:rect {:stroke :none, :fill :darkorchid} :_ [20 (apply-limit-fn h limit)]]
                            [:text {:text-family "Verdana" :font-size 12} (str h)]])
                      price-history)]])))))))))
