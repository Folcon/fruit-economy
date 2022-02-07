(ns fruit-economy.data.core
  (:require [fruit-economy.db.core :as db]))



(defn land-data [world-db]
  (db/q '[:find (pull ?e [*]) . :where [?e :fruit-economy.land/terrain]] world-db))

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
  (let [ent (db/q '[:find (pull ?e ?attrs) . :where [?e ::civ/name ?civ-name] :in $ ?civ-name ?attrs] world-db civ-name (into [:db/id] (keys attrs)))]
    (db/db-bulk-insert world-db
      [(reduce-kv (fn [e k f] (update e k f)) ent attrs)])))

(defn civ-count [world-db]
  (db/q '[:find (count ?value) . :where [?e :land/civs ?value]] world-db))

