(ns fruit-economy.game
  (:require [fruit-economy.rand :as rand]
            [fruit-economy.land :as land]
            [fruit-economy.civ :as civ]))


(defn unit-tick [land-data]
  (println :unit-tick)
  (-> land-data
    (update ::land/area->units #(into {} (map (fn [[area {:keys [on-tick] :as unit}]] (if on-tick (on-tick unit land-data) unit))) %))))

(defn land-tick [land-data]
  (println :land-tick)
  (-> land-data
    (unit-tick)))

(defn on-tick [land-data]
  (println :on-tick)
  (cond-> (land-tick land-data)
    (= (rand/roll 20) 20)
    (civ/try-spawn-new-civs (rand/roll 6))))
