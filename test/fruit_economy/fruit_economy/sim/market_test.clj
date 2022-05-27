(ns fruit-economy.fruit-economy.sim.market-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]
            [fruit-economy.sim.market :refer [empty-order-book load-orders match-orders]]))


(def small-buyer {:price 1 :size 1 :side :buys :id :small-buyer})
(def medium-buyer {:price 1 :size 2 :side :buys :id :medium-buyer})
(def rich-buyer {:price 2 :size 1 :side :buys :id :rich-buyer})
(def small-seller {:price 1 :size 1 :side :sells :id :small-seller})
(def medium-seller {:price 1 :size 2 :side :sells :id :medium-seller})
(def pricey-seller {:price 1.5 :size 2 :side :sells :id :pricy-seller})
(def large-seller {:price 2 :size 3 :side :sells :id :large-seller})


(deftest orders-add-on-correct-side-test
  (testing "Testing orders are added to the correct side of the order book"
    (let [orders [small-buyer rich-buyer small-seller]
          order-book (load-orders (empty-order-book) orders)]
      (is (= 2 (count (:buys order-book))))
      (is (= 1 (count (:sells order-book)))))))

(deftest orders-prioritised-by-price-test
  (testing "cheaper sells first and more expensive buys first"
    (let [orders [small-buyer medium-buyer rich-buyer small-seller medium-seller large-seller]
          order-book (load-orders (empty-order-book) orders)]
      (is (= [[:small-seller {:price 1, :size 1, :side :sells, :id :small-seller}]
              [:medium-seller {:price 2, :size 1.5, :side :sells, :id :medium-seller}]
              [:large-seller {:price 3, :size 2, :side :sells, :id :large-seller}]]
             (reduce conj [] (:sells order-book))))
      (is (= [[:rich-buyer {:price 2, :size 1, :side :buys, :id :rich-buyer}]
              [:small-buyer {:price 1, :size 1, :side :buys, :id :small-buyer}]
              [:medium-buyer {:price 1, :size 2, :side :buys, :id :medium-buyer}]]
             (reduce conj [] (:buys order-book)))))))

(deftest orders-matched-test
  (testing "Test equal orders on opposite sides match"
    (let [orders [small-buyer small-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (zero? (count (:buys order-book'))))
      (is (zero? (count (:sells order-book'))))
      (is (= [{:price 1
               :size 1
               :seller :small-seller
               :buyer :small-buyer
               :buy-order {:price 1 :size 1 :side :buys :id :small-buyer}
               :sell-order {:price 1 :size 1 :side :sells :id :small-seller}}]
             (:matched order-book')))))

  (testing "Test equal sized orders with different prices on opposite sides match"
    (let [orders [rich-buyer small-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (zero? (count (:buys order-book'))))
      (is (zero? (count (:sells order-book'))))
      (is (= [{:price 1
               :size 1
               :seller :small-seller
               :buyer :rich-buyer
               :buy-order {:price 2 :size 1 :side :buys :id :rich-buyer}
               :sell-order {:price 1 :size 1 :side :sells :id :small-seller}}]
             (:matched order-book')))))

  (testing "Test multiple orders can match as long as all buyer prices are over seller prices"
    (let [orders [small-buyer rich-buyer
                  medium-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (zero? (count (:buys order-book'))))
      (is (zero? (count (:sells order-book'))))
      (is (= [{:price 1
               :size 1
               :seller :medium-seller
               :buyer :rich-buyer
               :buy-order {:price 2 :size 1 :side :buys :id :rich-buyer}
               :sell-order {:price 1 :size 2 :side :sells :id :medium-seller}}
              {:price 1
               :size 1
               :seller :medium-seller
               :buyer :small-buyer
               :buy-order {:price 1 :size 1 :side :buys :id :small-buyer}
               :sell-order {:price 1 :size 1 :side :sells :id :medium-seller}}]
             (:matched order-book')))))

  (testing "Test sellers with too high a price will not find buyers"
    (let [orders [medium-buyer
                  pricey-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (= 1 (count (:buys order-book'))))
      (is (= 1 (count (:sells order-book'))))
      (is (= []
             (:matched order-book'))))))

;;  (prop/for-all [market (gen/hash-map
;;                          :buy-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer))
;;                          :sell-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer)))]
;;    (= (sort-orders market) (sort-orders (sort-orders market)))))
;;
;;
;;
;;
;;#_(let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
;;                     :clothes/price 1 :clothes/demand 1 :clothes/supply 1
;;                     :labour/supply 1}
;;                    {:money 1000 :food 10 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
;;        db (d/db-with (d/empty-db peep-shop-schema)
;;             world-data)]
;;    (is (set/subset?
;;          (set [[:db/add 2 :food 10] [:db/add 2 :clothes 10]])
;;          (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1))))))
