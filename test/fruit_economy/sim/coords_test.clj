(ns fruit-economy.sim.coords-test
  (:require [clojure.test :refer :all]
            [fruit-economy.land :as land]
            [fruit-economy.sim.coords :as coords]))


(deftest neighbours-test
  (testing "Does neighbours generate all neighbours, including ones that could be out of bounds ones?"
    (is (= (vec (coords/neighbours 0 0))
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))
    (is (= (vec (coords/neighbours [0 0]))
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))))

(deftest get-territory-borders-test
  (testing "Does get-territory-borders generate in bounds coords adjacent to the territory but not inside it?"
    (let [land-data {::land/width 2 ::land/height 2}
          territory #{[1 1]}]
      (is (= (coords/get-territory-borders land-data territory)
             [[0 0] [0 1] [1 0]])))
    (let [land-data {::land/width 2 ::land/height 2}
          territory #{[2 2]}]
      (is (= (coords/get-territory-borders land-data territory)
            [[1 1]])))))
