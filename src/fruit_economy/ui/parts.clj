(ns fruit-economy.ui.parts
  (:require [io.github.humbleui.ui :as ui]
            [fruit-economy.state :as state]
            [fruit-economy.data.core :as data]
            [fruit-economy.ui.bits :as ui.bits :refer [padding]]
            [fruit-economy.sim.basic :as basic])
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
          #(swap! state/*menu assoc :screen nil)
          (ui/padding 10 10
            (ui/label "Select Player" {:font font-small :paint fill-black})))))))

(def time-controls
  (ui/dynamic ctx [{:keys [font-small fill-white fill-yellow fill-dark-gray]} ctx
                   {:keys [paused?]} @state/*menu]
    (ui/row
      (ui/clickable
        #(swap! state/*menu update :paused? not)
        (ui/hoverable
          (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
            (ui/fill (cond paused? fill-dark-gray hovered? fill-yellow :else fill-white)
              (ui/padding 10 10
                (ui/label (if paused? "⏸️" "▶️") {:font font-small :paint fill-white}))))))
      (if paused?
        (ui/row
          (ui/clickable
            basic/do-tick-world
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+ 1 Day" {:font font-small :paint fill-white}))))))
          (ui/clickable
            basic/tick-world-10x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+ 10 Days" {:font font-small :paint fill-white}))))))
          (ui/clickable
            basic/tick-world-100x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+ 100 Days" {:font font-small :paint fill-white}))))))
          (ui/clickable
            basic/tick-world-1000x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+ 1000 Days" {:font font-small :paint fill-white})))))))
        (ui/row
          (ui/clickable
            basic/tick-world-10x
            (ui/hoverable
              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                (ui/fill (if hovered? fill-yellow fill-dark-gray)
                  (ui/padding 10 10
                    (ui/label "+" {:font font-small :paint fill-white})))))))))))

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
                 #(reset! basic/*world (basic/reset-world))
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
