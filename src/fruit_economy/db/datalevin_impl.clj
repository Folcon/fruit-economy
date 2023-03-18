(ns fruit-economy.db.datalevin-impl
  (:require [datalevin.core :as d]
            [fruit-economy.db.protocol :as p])
  (:import [datalevin.db DB]))



(extend-type
  DB
  p/DBOps
  (query
    ([db query]
     (d/q query db))
    ([db query inputs]
     (apply d/q query db inputs))))
