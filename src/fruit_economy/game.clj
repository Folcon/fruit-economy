(ns fruit-economy.game
  (:require [fruit-economy.rand :as rand]
            [fruit-economy.land :as land]
            [fruit-economy.civ :as civ]
            [fruit-economy.data.core :as data]))


(defn unit-tick [world-db]
  (println :unit-tick)
  (let [tickable (data/tickable world-db)
        ticked (into [] (comp (map (fn [{:keys [on-tick] :as unit}] (on-tick unit world-db))) cat) tickable)]
    (data/update-ticked world-db ticked)))

(defn land-tick [world-db]
  (println :land-tick)
  (-> world-db
    (unit-tick)))

(defn on-tick [world-db]
  (println :on-tick)
  (let [world-db' (land-tick world-db)]
    (cond-> world-db'
      (= (rand/roll 20) 20)
      (data/update-ticked (civ/try-spawn-new-civs-tx world-db' (rand/roll 6))))))
