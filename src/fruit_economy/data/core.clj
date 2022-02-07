(ns fruit-economy.data.core
  (:require [fruit-economy.db.core :as db]))



(defn land-data [world-db]
  (db/q '[:find (pull ?e [*]) . :where [?e ::land/terrain]] world-db))

(defn history-log-entries [world-db]
  (db/q '[:find ?value . :where [?e ::land/history ?value]] world-db))

