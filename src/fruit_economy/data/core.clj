(ns fruit-economy.data.core
  (:require [fruit-economy.land :as land]
            [fruit-economy.civ :as civ]
            [fruit-economy.economy :as economy]
            [fruit-economy.db.core :as db]))


(defn init-world [world-name width height]
  (-> (land/make-land world-name width height)
    (land/gen-land)
    (land/populate 50 #_100)
    (land/spawn-units 10)
    (economy/add-resources)
    (civ/try-spawn-new-civs 10)))

