(ns fruit-economy.fruit-economy.sim.basic-test
  (:require [fruit-economy.sim.basic :as basic]
            [clojure.test :refer :all]
            [clojure.set :as set]
            [datascript.core :as d]))


(comment
  (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                    {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
        db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                   :coord {:db/unique :db.unique/identity}})
             world-data)]
    (is (= (set [[:db/add 4 :place 3]])
          (set (basic/rewrite hunt-rule db))))))

(deftest hunt-test
  (testing "Testing that hunter will not if there's no better choices"
    (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 0 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's more food where they are"
    (let [world-data [{:food 1 :coord [0 0]} {:food 0 :coord [1 0]} {:food 0 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's the same amount of food where they are"
    (let [world-data [{:food 1 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter will move if there's more food elsewhere"
    (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 4 :place 3]])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if they can't see there's more food elsewhere"
    (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                      {:food 0 :coord [0 1]} {:food 1 :coord [1 1]} {:food 1 :coord [2 1]}
                      {:glyph "🐞", :wealth 0, :vision 1, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db)))))))

(let [world-data [{:food 2 :coord [0 0]}
                  {:glyph "🐞", :wealth -1, :vision 2, :hunger 3, :place [:coord [0 0]]}]
      db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                 :coord {:db/unique :db.unique/identity}})
           world-data)]
  (basic/rewrite basic/remove-starving-rule db)
  #_(is (= (set [])
          (set (basic/rewrite basic/try-eat-rule db)))))

(deftest try-eat-test
  (testing "Testing that hunter will go hungry if they have no food where they are or wealth and gathering food doesn't make the food negative"
    (let [world-data [{:food 0 :coord [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 2 :wealth -3]
                   [:db/add 1 :food 0]])
            (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will go hungry if they have some food where they are, but not enough"
    (let [world-data [{:food 2 :coord [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 2 :wealth -1]
                   [:db/add 1 :food 0]])
            (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will not go hungry if they have enough food where they are"
      (let [world-data [{:food 3 :coord [0 0]}
                        {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
            db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                       :coord {:db/unique :db.unique/identity}})
                 world-data)]
        (is (= (set [[:db/add 2 :wealth 0]
                     [:db/add 1 :food 0]])
              (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will not go hungry if they have no food where they are, but have enough wealth"
        (let [world-data [{:food 0 :coord [0 0]}
                          {:glyph "🐞", :wealth 3, :vision 2, :hunger 3, :place [:coord [0 0]]}]
              db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                         :coord {:db/unique :db.unique/identity}})
                   world-data)]
          (is (= (set [[:db/add 2 :wealth 0]
                       [:db/add 1 :food 0]])
                (set (basic/rewrite basic/try-eat-rule db)))))))

(deftest remove-starving-test
  (testing "Testing that not hungry hunter will not be removed"
    (let [world-data [{:food 0 :coord [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/remove-starving-rule db))))))
  (testing "Testing that hungry hunter will be removed"
    (let [world-data [{:food 0 :coord [0 0]}
                      {:glyph "🐞", :wealth -1, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/retractEntity 2]])
            (set (basic/rewrite basic/remove-starving-rule db)))))))

(deftest grow-food-test
  (testing "Testing food will not grow if at init-food"
    (let [world-data [{:food 1 :coord [0 0] :init-food 1}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/grow-food-rule db))))))
  (testing "Testing food will grow if not at init-food"
    (let [world-data [{:food 0 :coord [0 0] :init-food 1}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 1 :food 1]])
            (set (basic/rewrite basic/grow-food-rule db))))))
  (testing "Testing food will not grow if greater then init-food"
      (let [world-data [{:food 2 :coord [0 0] :init-food 1}]
            db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                       :coord {:db/unique :db.unique/identity}})
                 world-data)]
        (is (= (set [])
              (set (basic/rewrite basic/grow-food-rule db)))))))

(defn db->datoms [db]
  (into [] (map (juxt (constantly :db/add) :e :a :v)) (d/datoms db :eavt)))

(def peep-shop-schema
  {:good {:db/index true}
   :hometown {:db/valueType :db.type/ref}})

(deftest peep-shop-test
  (testing "Testing peeps don't shop when they have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 10 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 10] [:db/add 2 :clothes 10]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps do shop when they don't have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 5}
                      {:money 1000 :food 10 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 10}
                      {:money 1000 :food 0 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 20] [:db/add 2 :clothes 10]
                  [:db/add 3 :food 50] [:db/add 3 :clothes 20]
                  [:db/add 4 :food 60] [:db/add 4 :clothes 40]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps who don't have enough money can't shop"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 0 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 5}
                      {:money 0 :food 10 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 10}
                      {:money 0 :food 0 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 0]
                  [:db/add 3 :food 10] [:db/add 3 :clothes 0]
                  [:db/add 4 :food 0] [:db/add 4 :clothes 10]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps don't increase demand when they have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 10 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/demand 1] [:db/add 1 :clothes/demand 1]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps do increase demand when they don't have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/demand 61] [:db/add 1 :clothes/demand 31]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1))))))))

(def peep-consume-schema
  {:hometown {:db/valueType :db.type/ref}})

(deftest peep-consume-test
  (testing "Testing food and clothes get consumed"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:food 2 :clothes 2 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 1]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1)))))))
  (testing "Testing if insufficient food or clothes to consume, health lost"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:food 0 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}
                      {:food 0 :clothes 2 :health 10 :min-food 2 :min-clothes 1 :hometown 1}
                      {:food 2 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 0] [:db/add 2 :health 9]
                  [:db/add 3 :food 0] [:db/add 3 :clothes 1] [:db/add 3 :health 9]
                  [:db/add 4 :food 0] [:db/add 4 :clothes 0] [:db/add 4 :health 9]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1)))))))
  (testing "Testing if sufficient food and clothes to consume, produces enough supply"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:food 2 :clothes 1 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :labour/supply 10]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1)))))))
  (testing
    #_"Testing if insufficient food or clothes to consume, produce proportionally less supply"
    "Testing if insufficient food or clothes to consume, no change in supply"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:db/id 2 :labour/supply 0}
                      {:db/id 3 :labour/supply 0}
                      {:food 0 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}
                      {:food 0 :clothes 2 :health 10 :min-food 2 :min-clothes 1 :hometown 2}
                      {:food 2 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 3}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [#_#_#_
                  [:db/add 1 :labour/supply 0]
                  [:db/add 2 :labour/supply 5]
                  [:db/add 3 :labour/supply 5]
                  [:db/add 1 :labour/supply 10]
                  [:db/add 2 :labour/supply 10]
                  [:db/add 3 :labour/supply 10]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1))))))))

;; TODO: Update test cases to be inline with the current `update-price-velocity`
(deftest update-prices-test
  (testing "Testing prices don't change when demand equal to supply"
    (let [world-data [{:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/price-velocity 0
                       :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0
                       :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 0 :labour/price-velocity 0}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 100]
                  [:db/add 1 :clothes/price 2]
                  [:db/add 1 :clothes/price-float 200]
                  [:db/add 1 :labour/price 3]
                  [:db/add 1 :labour/price-float 300]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1)))))))
  (testing "Testing prices increase when demand greater than supply"
    (let [world-data [{:food/price 1 :food/price-float 100 :food/demand 1 :food/supply 0 :food/price-velocity 0
                       :clothes/price 2 :clothes/price-float 200 :clothes/demand 1 :clothes/supply 0 :clothes/price-velocity 0
                       :labour/price 3 :labour/price-float 300 :labour/demand 1 :labour/supply 0 :labour/price-velocity 0}
                      {:food/price 1 :food/price-float 199 :food/demand 1 :food/supply 0 :food/price-velocity 0
                       :clothes/price 2 :clothes/price-float 299 :clothes/demand 1 :clothes/supply 0 :clothes/price-velocity 0
                       :labour/price 3 :labour/price-float 399 :labour/demand 1 :labour/supply 0 :labour/price-velocity 0}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 101]
                  [:db/add 1 :clothes/price 2]
                  [:db/add 1 :clothes/price-float 201]
                  [:db/add 1 :labour/price 3]
                  [:db/add 1 :labour/price-float 301]
                  [:db/add 2 :food/price 2]
                  [:db/add 2 :food/price-float 200]
                  [:db/add 2 :clothes/price 3]
                  [:db/add 2 :clothes/price-float 300]
                  [:db/add 2 :labour/price 4]
                  [:db/add 2 :labour/price-float 400]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1)))))))
  (testing "Testing prices decrease when demand lower than supply"
    (let [world-data [{:food/price 2 :food/price-float 200 :food/demand 0 :food/supply 1 :food/price-velocity 0
                       :clothes/price 3 :clothes/price-float 300 :clothes/demand 0 :clothes/supply 1 :clothes/price-velocity 0
                       :labour/price 4 :labour/price-float 400 :labour/demand 0 :labour/supply 1 :labour/price-velocity 0}
                      {:food/price 1 :food/price-float 101 :food/demand 0 :food/supply 1 :food/price-velocity 0
                       :clothes/price 2 :clothes/price-float 201 :clothes/demand 0 :clothes/supply 1 :clothes/price-velocity 0
                       :labour/price 3 :labour/price-float 301 :labour/demand 0 :labour/supply 1 :labour/price-velocity 0}
                      {:food/price 2 :food/price-float 200 :food/demand 0 :food/supply 1 :food/price-velocity 0
                       :clothes/price 3 :clothes/price-float 300 :clothes/demand 0 :clothes/supply 1 :clothes/price-velocity 0
                       :labour/price 4 :labour/price-float 400 :labour/demand 0 :labour/supply 1 :labour/price-velocity 0}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 199]
                  [:db/add 1 :clothes/price 2]
                  [:db/add 1 :clothes/price-float 299]
                  [:db/add 1 :labour/price 3]
                  [:db/add 1 :labour/price-float 399]
                  [:db/add 2 :food/price 1]
                  [:db/add 2 :food/price-float 100]
                  [:db/add 2 :clothes/price 2]
                  [:db/add 2 :clothes/price-float 200]
                  [:db/add 2 :labour/price 3]
                  [:db/add 2 :labour/price-float 300]
                  [:db/add 3 :food/price 1]
                  [:db/add 3 :food/price-float 199]
                  [:db/add 3 :clothes/price 2]
                  [:db/add 3 :clothes/price-float 299]
                  [:db/add 3 :labour/price 3]
                  [:db/add 3 :labour/price-float 399]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1)))))))
  (testing "Testing minimum price is 1"
    (let [world-data [{:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 1 :food/price-velocity 0
                       :clothes/price 1 :clothes/price-float 100 :clothes/demand 0 :clothes/supply 1 :clothes/price-velocity 0
                       :labour/price 1 :labour/price-float 100 :labour/demand 0 :labour/supply 1 :labour/price-velocity 0}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 99]
                  [:db/add 1 :clothes/price 1]
                  [:db/add 1 :clothes/price-float 99]
                  [:db/add 1 :labour/price 1]
                  [:db/add 1 :labour/price-float 99]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1))))))))

;; Test supply equal to total inventory and demand is equal to total planning
(deftest supply-demand-test
  ;; TODO: Correct this, probably as a generative test
  (testing "Testing supply of food and clothes is equal to total inventory"
    (let [world-data [{:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/price-velocity 0
                       :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0
                       :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 100 :labour/price-velocity 0}
                      {:good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}
                      {:good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/supply 4]
                  [:db/add 1 :clothes/supply 1]
                  [:db/add 2 :inventory 4]
                  [:db/add 3 :inventory 1]])
            (set (db->datoms (basic/infer db [basic/craft-rule] 1)))))
      ;; TODO: Fix as we don't take into account decay
      (is (set/subset?
            (set [[:db/add 1 :food/supply 8]
                  [:db/add 1 :clothes/supply 2]
                  [:db/add 2 :inventory 7]
                  [:db/add 3 :inventory 1]])
            (set (db->datoms (basic/infer db [basic/craft-rule] 2)))))))
  (testing "Testing supply of labour is equal to total labour"
    (let [world-data [{:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/price-velocity 0
                       :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0
                       :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 0 :labour/price-velocity 0}
                      {:food 0 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :labour/supply 10]
                  [:db/add 2 :labour 10]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1))))))))

