(ns fruit-economy.screen-ui
  (:require [io.github.humbleui.ui :as ui]
            [fruit-economy.data.core :as data]
            [fruit-economy.sim.basic :as basic]))


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