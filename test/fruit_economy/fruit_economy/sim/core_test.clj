(ns fruit-economy.fruit-economy.sim.core-test
  (:require [clojure.test :refer :all]
            [fruit-economy.db.core :refer [q query init-db db-bulk-insert]]
            [fruit-economy.land :as land]
            [fruit-economy.language :as lang]
            [fruit-economy.civ :as civ]))


(deftest adding-land-data-to-db-test
  (let [width 1 height 1
        world-name "World"
        land-data (-> (land/make-land world-name width height)
                    (land/gen-land))
        db (-> (init-db)
             (db-bulk-insert [land-data]))]
    (testing "Testing that I can query out land data after dumping it into the db"
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/name ?value]]
                 db))
            world-name))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/width ?value]]
                 db))
            width))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/height ?value]]
                 db))
            height))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/terrain ?value]]
                 db))
            [[:ocean]]))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/temp ?value]]
                 db))
            [[0.1]]))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/elev ?value]]
                 db))
            [[0.0]]))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::land/curr-civ-id ?value]]
                 db))
            0)))))

(deftest adding-civ-data-to-db-test
  (let [width 1 height 1
        world-name "World"
        land-data (-> (land/make-land world-name width height)
                    (land/gen-land))

        {::land/keys [name terrain curr-civ-id civ-letters lang] :as land-data} land-data
        x (rand-int width) y (rand-int height)
        civ-loc [x y]
        civ-path [y x]
        parent-civ nil
        symbol (first civ-letters)
        civ-name (lang/make-word lang)
        biome (get-in terrain civ-path)
        new-civ (civ/make-civ curr-civ-id civ-name symbol civ-loc name biome parent-civ [])

        db (-> (init-db)
             (db-bulk-insert [(assoc land-data :db/id -1)
                              (assoc new-civ :db/id -2)
                              [:db/add -1 :land/civs -2]]))]
    (testing "Testing that I can query out civ data after dumping it into the db"
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::civ/name ?value]]
                 db))
            civ-name))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::civ/symbol ?value]]
                 db))
            symbol))
      (is (= (ffirst
               (q '[:find ?value
                    :where [?e ::civ/home-biome ?value]]
                 db))
            biome)))))


(comment
  ;; So we now have a sort of API that we'd like to use and this obviously doesn't work
  (let [width 1 height 1
        land-data (-> (land/make-land "World" width height)
                    (land/gen-land))]
    (db-bulk-insert (init-db) [land-data]))

  ,)
