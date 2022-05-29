(ns fruit-economy.sim.market
  (:require [clojure.data.priority-map :refer [priority-map-keyfn priority-map-keyfn-by]]))


(defn empty-buys [] (priority-map-keyfn-by :price (comp - compare)))
(defn empty-sells [] (priority-map-keyfn :price))

(defn empty-order-book []
  {;; order from expensive => cheap
   :buys  (empty-buys)
   ;; order from cheap => expensive
   :sell (empty-sells)
   :matched []})

(defn load-order [order-book {:keys [side id] :as order}]
  (assoc-in order-book [side id] order))

(defn load-orders [order-book orders]
  (reduce load-order order-book orders))

(defn match-orders [order-book]
  (loop [{seller-id :id sell-price :price sell-size :size :as sell-order} (second (peek (:sell order-book)))
         {buyer-id :id buy-price :price buy-size :size :as buy-order} (second (peek (:buys order-book)))
         order-book (assoc order-book :sold 0)
         limit 0]
    (cond
      (> limit 100)
      order-book

      ;; stuff for sale is too expensive
      (< buy-price sell-price)
      order-book

      (= sell-size buy-size)
      (let [order-book' (-> order-book
                          (update :matched conj {:price sell-price :size buy-size :seller seller-id :buyer buyer-id :buy-order buy-order :sell-order sell-order})
                          (update :sell pop)
                          (update :buys pop)
                          (assoc :current-price (:price sell-order))
                          (update :sold + buy-size))
            sell-order' (second (peek (:sell order-book')))
            buy-order' (second (peek (:buys order-book')))]
        (if (and buy-order' sell-order')
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          order-book'))


      (> sell-size buy-size)
      (let [order-book' (-> order-book
                          (update :matched conj {:price sell-price :size buy-size :seller seller-id :buyer buyer-id :buy-order buy-order :sell-order sell-order})
                          (update :buys pop)
                          (update-in [:sell seller-id :size] - buy-size)
                          (update :sold + buy-size)
                          (assoc :current-price (:price sell-order)))
            sell-order' (update sell-order :size - buy-size)
            buy-order' (second (peek (:buys order-book')))]
        (if buy-order'
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          order-book'))

      (< sell-size buy-size)
      (let [order-book' (-> order-book
                          (update :matched conj {:price sell-price :size sell-size :seller seller-id :buyer buyer-id :buy-order buy-order :sell-order sell-order})
                          (update :sell pop)
                          (update-in [:buys buyer-id :size] - sell-size)
                          (update :sold + sell-size)
                          (assoc :current-price sell-price))
            sell-order' (second (peek (:sell order-book')))
            buy-order' (update buy-order :size - sell-size)]
        (if sell-order'
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          order-book')))))

