(ns fruit-economy.fruit-economy.sim.basic-test
  (:require [fruit-economy.sim.basic :as basic]
            [clojure.test :refer :all]
            [datascript.core :as d]))


(comment
  (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                    {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :id 0, :pos [:loc [0 0]]}]
        db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                   :loc {:db/unique :db.unique/identity}})
             world-data)]
    (is (= (set [[:db/add 4 :pos 3]])
          (set (basic/rewrite hunt-rule db))))))

(deftest hunt-test
  (testing "Testing that hunter will not if there's no better choices"
    (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 0 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :id 0, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's more food where they are"
    (let [world-data [{:food 1 :loc [0 0]} {:food 0 :loc [1 0]} {:food 0 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :id 0, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's the same amount of food where they are"
    (let [world-data [{:food 1 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :id 0, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter will move if there's more food elsewhere"
    (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :id 0, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 4 :pos 3]])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if they can't see there's more food elsewhere"
    (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                      {:food 0 :loc [0 1]} {:food 1 :loc [1 1]} {:food 1 :loc [2 1]}
                      {:glyph "🐞", :wealth 0, :vision 1, :hunger 3, :id 0, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db)))))))

