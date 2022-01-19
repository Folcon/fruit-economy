(ns fruit-economy.game
  (:require [fruit-economy.rand :as rand]
            [fruit-economy.civ :as civ]))


(defn land-tick [land-data]
  (println :land-tick)
  land-data)

(defn on-tick [land-data]
  (println :on-tick)
  (cond-> (land-tick land-data)
    (= (rand/roll 20) 20)
    (civ/try-spawn-new-civs (rand/roll 6))))
