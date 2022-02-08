(ns fruit-economy.data.core
  (:require [fruit-economy.db.core :as db]
            [fruit-economy.economy :as economy]))



(defn land-data [world-db]
  (db/q '[:find (pull ?e [*]) . :where [?e :fruit-economy.land/terrain]] world-db))

(defn land-area [world-db area]
  (db/q '[:find [(pull ?e [*]) ...] :where [?e :area ?a] :in $ ?a] world-db area))

(defn upsert-land-data [world-db attrs]
  (let [civ-id (db/q '[:find ?e . :where [?e :fruit-economy.land/terrain]] world-db)]
    (db/db-bulk-insert world-db
      [(assoc attrs :db/id civ-id)])))

(defn update-land-data [world-db attrs]
  (let [ent (db/q '[:find (pull ?e ?attrs) . :where [?e :fruit-economy.land/terrain] :in $ ?attrs] world-db (into [:db/id] (keys attrs)))]
    (db/db-bulk-insert world-db
      [(reduce-kv (fn [e k f] (update e k f)) ent attrs)])))

(defn log-history [world-db message]
  (let [[id history] (db/q '[:find [?e ?value] :where [?e :fruit-economy.land/history ?value]] world-db)]
    (db/db-bulk-insert world-db
      [{:db/id id :fruit-economy.land/history (conj history message)}])))

(defn history-log-entries [world-db]
  (db/q '[:find ?value . :where [?e :fruit-economy.land/history ?value]] world-db))

(defn upsert-civ [world-db civ-name attrs]
  (let [civ-id (db/q '[:find ?e . :where [?e :fruit-economy.civ/name ?civ-name] :in $ ?civ-name] world-db civ-name)]
    (db/db-bulk-insert world-db
      [(assoc attrs :db/id civ-id)])))

(defn update-civ [world-db civ-name attrs]
  (let [ent (db/q '[:find (pull ?e ?attrs) . :where [?e :fruit-economy.civ/name ?civ-name] :in $ ?civ-name ?attrs] world-db civ-name (into [:db/id] (keys attrs)))]
    (db/db-bulk-insert world-db
      [(reduce-kv (fn [e k f] (update e k f)) ent attrs)])))

(defn civ-count [world-db]
  (db/q '[:find (count ?value) . :where [?e :land/civs ?value]] world-db))

(defn on-tick [world-db]
  (db/q '[:find [(pull ?e [*]) ...] :where [?e :on-tick]] world-db))

(defn update-ticked [world-db ticked]
  (db/db-bulk-insert world-db ticked))

(defn step-economy [world-db]
  (let [[id economy] (db/q '[:find [?e ?value] :where [?e :fruit-economy.land/economy ?value]] world-db)
        economy' (economy/step-economy economy)]
    (db/db-bulk-insert world-db [{:db/id id :fruit-economy.land/economy economy'}])))

