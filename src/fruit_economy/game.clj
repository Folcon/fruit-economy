(ns fruit-economy.game
  (:require [fruit-economy.rand :as rand]
            [fruit-economy.land :as land]
            [fruit-economy.civ :as civ]
            [fruit-economy.data.core :as data]))


(defn unit-tick [world-db]
  (println :unit-tick)
  (let [area->units {::land/area->units #(into {} (map (fn [[area {:keys [on-tick] :as unit}]] (if on-tick (on-tick unit world-db) unit))) %)}]
    (data/update-land-data world-db area->units)))

(defn land-tick [world-db]
  (println :land-tick)
  (-> world-db
    (unit-tick)))

(defn on-tick [world-db]
  (println :on-tick)
  (cond-> (land-tick world-db)
    (= (rand/roll 20) 20)
    (data/upsert-land-data (civ/try-spawn-new-civs (data/land-data world-db) (rand/roll 6)))))
