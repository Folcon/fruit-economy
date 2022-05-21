(ns fruit-economy.clock
  (:require [fruit-economy.state :as state]))


(defn set-interval [callback ms]
  (future (while true (do (Thread/sleep ms) (callback)))))

(defn start-clock [ms tick-fn]
  (reset! state/*clock (set-interval tick-fn ms)))

(defn stop-clock []
  (future-cancel @state/*clock))
