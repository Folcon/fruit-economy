(ns fruit-economy.ui.screens
  (:require [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [fruit-economy.state :as state]
            [fruit-economy.data.core :as data]
            [fruit-economy.sim.basic :as basic]
            [fruit-economy.ui.bits :as ui.bits]
            [fruit-economy.ui.parts :as ui.parts]
            [fruit-economy.ui.views :as ui.views]
            [fruit-economy.components :as cui]
            [fruit-economy.utils :refer [suppress-print]])
  (:import [io.github.humbleui.skija Font Typeface Paint]))


(def game-screen
  (ui/dynamic ctx [{:keys [scale face-default emoji-face game-glyph]} ctx
                   player-hp (:player-hp @state/*state)]
    (let [font-ui (Font. ^Typeface face-default (float (* 13 scale)))
          leading (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          emoji-font (Font. ^Typeface emoji-face (float 72))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/with-context {:face-ui face-default
                        :font-ui font-ui
                        :leading leading
                        :fill-text fill-text}
        (ui/row
          (ui/column
            (ui/padding 2 leading
              (ui/label {:font emoji-font :paint fill-text} game-glyph))
            [:stretch 1
             (ui/vscrollbar
               (ui/column
                 (for [[name _ui] ui.views/ui-views]
                   (ui/clickable
                     {:on-click (fn [_] (reset! state/*selected-ui-view name))}
                     (ui/dynamic ctx [selected? (= name @state/*selected-ui-view)
                                      hovered? (:hui/hovered? ctx)]
                       (let [label (ui/padding 20 leading
                                     (ui/label {:font font-ui :paint fill-text} name))]
                         (cond
                           selected? (ui/rect (doto (Paint.) (.setColor (unchecked-int 0xFFB2D7FE))) label)
                           hovered? (ui/rect (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA))) label)
                           :else label)))))))]
            (ui/padding 10 10
              (cui/atom-checkbox state/*floating "On top")))
          [:stretch 1
           (ui/dynamic _ [name @state/*selected-ui-view]
             (ui.views/ui-views name))])))))

(def start-screen
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [started?]} @state/*menu
                   {:keys [world-db]} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/valign 0.5
        (ui/halign 0.5
          (ui/column
            (ui/padding 20
              (ui/valign 0.5
                (ui/halign 0.5
                  (ui/label "Fruit Economy"))))
            (if-not started?
              (ui/button
                #(swap! state/*menu assoc :started? true)
                (ui/padding 80 10 80 10
                  (ui/label "New Game")))
              (ui/with-context {:map-font map-font
                                :emoji-font emoji-font
                                :lrtb lrtb
                                :cell cell
                                :tick tick}
                (ui/column
                  ui.parts/map-ui-view
                  (ui/halign 0.5
                    (ui/row
                      (ui/button
                        #(reset! state/*world (basic/reset-world))
                        (ui/padding 20 10 20 10
                          (ui/label "Gen World")))
                      (ui/gap 10 0)
                      (ui/button
                        #(dotimes [_ 50]
                           (suppress-print
                             (swap! state/*world basic/tick-world)))
                        (ui/padding 20 10 20 10
                          (ui/label "Simulate History")))))
                  (ui/gap 0 10)
                  (ui/halign 0.5
                    (let [history? (basic/history-started? world-db)
                          viable? (basic/viable-world? world-db)
                          select-city-btn (fn [city]
                                            (ui/dynamic ctx [{:keys [leading]} ctx
                                                             {:keys [player]} @state/*world]
                                              (let [{name :settlement/name
                                                     governed-by :_governs} city
                                                    government (first governed-by)
                                                    coord (get-in city [:settlement/place :coord])
                                                    gov-eid (:db/id government)]
                                                (ui/clickable
                                                  {:on-click (fn [_]
                                                               (do
                                                                 (swap! state/*state assoc :camera coord)
                                                                 (swap! state/*world assoc :player-eid gov-eid)))}
                                                  (ui/clip-rrect 4
                                                    (ui/dynamic ctx [{:keys [hui/active? hui/hovered?]} ctx]
                                                      (ui/rect
                                                        (cond
                                                          (or active? (= player gov-eid)) (paint/fill 0xFFA2C7EE)
                                                          hovered? (paint/fill 0xFFCFE8FC)
                                                          :else    (paint/fill 0xFFB2D7FE))
                                                        (ui/padding 20 leading 20 leading
                                                          (ui/halign 0.5
                                                            (ui/with-context
                                                              {:hui/active? false
                                                               :hui/hovered? false}
                                                              (ui/label (str name " $" (:money government)))))))))))))]
                      (cond
                        viable?
                        (ui/column
                          (ui/label "Pick your starting city or try again")
                          (ui/gap 0 10)
                          (ui/height 100
                            (ui/vscrollbar
                              (ui/column
                                (for [city (data/lookup-avet world-db :kind :city)]
                                  (ui/padding 0 2 0 2
                                    (select-city-btn city))))))
                          (ui/gap 0 10)
                          (ui/button
                            #(swap! state/*menu assoc :screen game-screen)
                            (ui/padding 80 10 80 10
                              (ui/label "Start"))))

                        (not history?)
                        (ui/label "Gen a world that looks interesting and start simulating history!")

                        (and history? (not viable?))
                        (ui/label "Everything Died, Try again?")))))))))))))
