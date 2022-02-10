(ns fruit-economy.data.core
  (:require [fruit-economy.db.core :as db]
            [fruit-economy.economy :as economy]))


(defn entity
  ([world-db id] (entity world-db '[*] id))
  ([world-db attrs id]
   (ffirst (db/q '[:find (pull ?e ?attrs) :in $ ?e ?attrs] world-db id attrs))))

(defn land-data [world-db]
  (db/q '[:find (pull ?e [*]) . :where [?e :fruit-economy.land/terrain]] world-db))

(defn land-resources [world-db]
  (db/q '[:find [(pull ?v [:area *]) ...] :where [?e :land/resources ?v]] world-db))

(defn land-area [world-db area]
  (db/q '[:find [(pull ?e [*]) ...] :where [?e :area ?a] :in $ ?a] world-db area))

(defn land-claims [world-db]
  (db/q '[:find [(pull ?v [*]) ...] :where [?e :civ/territory ?v]] world-db)
  #_(db/q '[:find ?value . :where [?e :fruit-economy.land/area->civ-name ?value]] world-db))

(defn land-claims->civ [world-db claim-id]
  (db/q '[:find (pull ?e [*]) . :where [?e :civ/territory ?claim-id] :in $ ?claim-id] world-db claim-id))

(defn land-area->civ [world-db area]
  (db/q '[:find (pull ?c [* {:civ/territory [*] :civ/peeps [*]}]) . :where [?e :area ?a] [?c :civ/territory ?e] :in $ ?a] world-db area))

(defn upsert-land-data [world-db attrs]
  (let [land-id (db/q '[:find ?e . :where [?e :fruit-economy.land/terrain]] world-db)]
    (db/db-bulk-insert world-db
      [(assoc attrs :db/id land-id)])))

(defn update-land-data [world-db attrs]
  (let [ent (db/q '[:find (pull ?e ?attrs) . :where [?e :fruit-economy.land/terrain] :in $ ?attrs] world-db (into [:db/id] (keys attrs)))]
    (db/db-bulk-insert world-db
      [(reduce-kv (fn [e k f] (update e k f)) ent attrs)])))

(defn log-history [message]
  [{:db/ident :land :land/history [{:history/entry message}]}])

(defn history-log-entries [world-db]
  (into [] (map :history) (sort-by :id (db/q '[:find [(pull ?v [[:db/id :as :id] [:history/entry :as :history]]) ...] :where [?e :land/history ?v]] world-db))))

(defn civ-name->civ
  ([world-db civ-name] (civ-name->civ world-db civ-name '[*]))
  ([world-db civ-name attrs]
   (db/q '[:find (pull ?e ?attrs) . :where [?e :fruit-economy.civ/power] [?e :fruit-economy.civ/name ?civ-name] :in $ ?civ-name ?attrs] world-db civ-name (conj attrs :db/id))))

(defn upsert-civ [world-db civ-name attrs]
  (let [civ-id (db/q '[:find ?e . :where [?e :fruit-economy.civ/name ?civ-name] :in $ ?civ-name] world-db civ-name)]
    (db/db-bulk-insert world-db
      [(assoc attrs :db/id civ-id)])))

(defn update-civ [world-db civ-name attrs]
  (let [ent (db/q '[:find (pull ?e ?attrs) . :where [?e :fruit-economy.civ/name ?civ-name] :in $ ?civ-name ?attrs] world-db civ-name (into [:db/id] (keys attrs)))]
    (db/db-bulk-insert world-db
      [(reduce-kv (fn [e k f] (update e k f)) ent attrs)])))

(defn peep->civ-peeps [world-db peep-id]
  (db/q '[:find (pull ?e [{:civ/peeps [*]}]) . :where [?e :civ/peeps ?peep-id] :in $ ?peep-id] world-db peep-id))

(defn civ-count [world-db]
  (db/q '[:find (count ?value) . :where [?e :land/civs ?value]] world-db))

;; TODO: Need to think of better names
(defn tickable [world-db]
  (db/q '[:find [(pull ?e [*]) ...] :where [?e :on-tick]] world-db))

(defn update-ticked [world-db ticked]
  (db/db-bulk-insert world-db ticked))

(defn step-economy [world-db]
  (let [[id economy] (db/q '[:find [?e ?value] :where [?e :fruit-economy.land/economy ?value]] world-db)
        economy' (economy/step-economy economy)]
    (db/db-bulk-insert world-db [{:db/id id :fruit-economy.land/economy economy'}])))

