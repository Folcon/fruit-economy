(ns fruit-economy.db.protocol)



(defprotocol DBOps
  (--query [db query] [db query inputs])
  (--datoms [db index] [db index c1] [db index c1 c2] [db index c1 c2 c3] [db index c1 c2 c3 c4])
  (--entity [db eid])
  (--touch [e])
  (--db-with [db tx-data]))
