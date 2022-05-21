(ns fruit-economy.ui.parts
  (:require [io.github.humbleui.ui :as ui]
            [fruit-economy.state :as state]
            [fruit-economy.data.core :as data]
            [fruit-economy.ui.bits :as ui.bits :refer [padding]])
  (:import [io.github.humbleui.skija Paint PaintMode]))


(def top-bar-ui
  (ui/dynamic ctx [{:keys [font-small fill-black fill-yellow fill-white scale]} ctx]
    (ui/column
      [:stretch 1
       (ui/padding 0 0 0 10
         (ui/fill fill-yellow
           (ui/row
             (ui/padding 10 10
               (ui/dynamic ctx [tick (:tick @state/*state)]
                 (ui/label (str "Day " (inc tick)) {:font font-small :paint fill-black})))
             [:stretch 1 nil]
             (ui/fill fill-white
               (ui/clickable
                 #(reset! state/*state (new-state))
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
