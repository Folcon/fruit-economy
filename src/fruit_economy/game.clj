(ns fruit-economy.game
  (:require [fruit-economy.rand :as rand]
            [fruit-economy.land :as land]
            [fruit-economy.civ :as civ]
            [fruit-economy.data.core :as data]))


(defn unit-tick [world-db]
  (println :unit-tick)
  (let [tickable (data/on-tick world-db)
        ticked (into [] (comp (map (fn [{:keys [on-tick] :as unit}] (on-tick unit world-db))) cat) tickable)]
    (data/update-ticked world-db ticked)))

(defn land-tick [world-db]
  (println :land-tick)
  (-> world-db
    (unit-tick)))

(defn on-tick [world-db]
  (println :on-tick)
  (cond-> (land-tick world-db)
    (= (rand/roll 20) 20)
    (data/upsert-land-data (civ/try-spawn-new-civs (data/land-data world-db) (rand/roll 6)))))
