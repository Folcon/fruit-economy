(ns fruit-economy.clock
  (:require [fruit-economy.state :as state]))


(defn set-interval [callback]
  (future (while true (do (Thread/sleep (:speed-ms @state/*menu)) (callback)))))

(defn start-clock [tick-fn]
  (reset! state/*clock (set-interval tick-fn)))

(defn stop-clock []
  ;; TODO: Have a continuous clock which does nothing?
  ;;   How perceivable / problematic is the delay in clock speeding up / slowing down after interaction?
  (when-let [clock @state/*clock]
    (future-cancel clock)))
