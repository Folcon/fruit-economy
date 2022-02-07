(ns fruit-economy.fruit-economy.sim.core-test
  (:require [clojure.test :refer :all]
            [fruit-economy.db.core :refer [query init-db]]
            [fruit-economy.land :as land]))


(comment
  ;; So we now have a sort of API that we'd like to use and this obviously doesn't work
  (let [width 1 height 1
        land-data (-> (land/make-land "World" width height)
                    (land/gen-land))]
    (query (init-db)
      [[:insert land-data]]))

  ,)
