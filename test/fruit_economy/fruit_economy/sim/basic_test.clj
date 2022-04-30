(ns fruit-economy.fruit-economy.sim.basic-test
  (:require [fruit-economy.sim.basic :as basic]
            [clojure.test :refer :all]
            [datascript.core :as d]))


(comment
  (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                    {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
        db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                   :loc {:db/unique :db.unique/identity}})
             world-data)]
    (is (= (set [[:db/add 4 :pos 3]])
          (set (basic/rewrite hunt-rule db))))))

(deftest hunt-test
  (testing "Testing that hunter will not if there's no better choices"
    (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 0 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's more food where they are"
    (let [world-data [{:food 1 :loc [0 0]} {:food 0 :loc [1 0]} {:food 0 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's the same amount of food where they are"
    (let [world-data [{:food 1 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter will move if there's more food elsewhere"
    (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 4 :pos 3]])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if they can't see there's more food elsewhere"
    (let [world-data [{:food 0 :loc [0 0]} {:food 0 :loc [1 0]} {:food 1 :loc [2 0]}
                      {:food 0 :loc [0 1]} {:food 1 :loc [1 1]} {:food 1 :loc [2 1]}
                      {:glyph "🐞", :wealth 0, :vision 1, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db)))))))

(deftest try-eat-test
  (testing "Testing that hunter will go hungry if they have no food where they are or wealth and gathering food doesn't make the food negative"
    (let [world-data [{:food 0 :loc [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 2 :wealth -3]
                   [:db/add 1 :food 0]])
            (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will go hungry if they have some food where they are, but not enough"
    (let [world-data [{:food 2 :loc [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
          db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                     :loc {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 2 :wealth -1]
                   [:db/add 1 :food 0]])
            (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will not go hungry if they have enough food where they are"
      (let [world-data [{:food 3 :loc [0 0]}
                        {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
            db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                       :loc {:db/unique :db.unique/identity}})
                 world-data)]
        (is (= (set [[:db/add 2 :wealth 0]
                     [:db/add 1 :food 0]])
              (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will not go hungry if they have no food where they are, but have enough wealth"
        (let [world-data [{:food 0 :loc [0 0]}
                          {:glyph "🐞", :wealth 3, :vision 2, :hunger 3, :pos [:loc [0 0]]}]
              db (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                                         :loc {:db/unique :db.unique/identity}})
                   world-data)]
          (is (= (set [[:db/add 2 :wealth 0]
                       [:db/add 1 :food 0]])
                (set (basic/rewrite basic/try-eat-rule db)))))))
