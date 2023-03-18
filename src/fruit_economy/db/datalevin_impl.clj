(ns fruit-economy.db.datalevin-impl
  (:require [datalevin.core :as d]
            [fruit-economy.db.protocol :as p])
  (:import [datalevin.db DB]))



(extend-type
  DB
  p/DBOps
  (--query
    ([db query]
     (d/q query db))
    ([db query inputs]
     (apply d/q query db inputs)))
  (--datoms
    ([db index] (d/datoms db index))
    ([db index c1] (d/datoms db index c1))
    ([db index c1 c2] (d/datoms db index c1 c2))
    ([db index c1 c2 c3] (d/datoms db index c1 c2 c3))
    ([db index c1 c2 c3 c4] (d/datoms db index c1 c2 c3 c4)))
  (--entity [db eid] (d/entity db eid))
  (--touch [e] (d/touch e))
  (--db-with [db tx-data] (d/db-with db tx-data)))

(defn --empty-db
  ([] (d/empty-db))
  ([{:keys [dir] :as opts}] (if dir (d/empty-db dir) (d/empty-db)))
  ([schema {:keys [dir] :as opts}] (d/empty-db dir schema opts)))

