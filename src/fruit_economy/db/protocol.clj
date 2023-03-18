(ns fruit-economy.db.protocol)



(defprotocol DBOps
  (query [db query] [db query inputs]))
