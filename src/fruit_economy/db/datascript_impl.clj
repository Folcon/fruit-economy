(ns fruit-economy.db.datascript-impl
  (:require [datascript.core :as d]
            [fruit-economy.db.protocol :as p])
  (:import [datascript.db DB]))



(extend-type
  DB
  p/DBOps
  (query
    ([db query]
     (d/q query db))
    ([db query inputs]
     (apply d/q query db inputs))))
