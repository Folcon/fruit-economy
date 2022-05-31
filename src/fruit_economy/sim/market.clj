(ns fruit-economy.sim.market
  (:require [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map-keyfn priority-map-keyfn-by]]))


(s/def :order/id (s/or :num number? :kw keyword?))
(s/def :order/price pos-int?)
(s/def :order/size pos-int?)
(s/def :order/side #{:buys :sell})
(s/def :order/order (s/keys :req-un [:order/id :order/price :order/size :order/side]))

(defn empty-buys [] (priority-map-keyfn-by :price (comp - compare)))
(defn empty-sells [] (priority-map-keyfn :price))

(defn empty-order-book []
  {;; order from expensive => cheap
   :buys  (empty-buys)
   ;; order from cheap => expensive
   :sell (empty-sells)
   :matched []
   :current-price 1})

(s/fdef load-order
  :args (s/cat :order-book any? :order :order/order))

(defn load-order [order-book {:keys [side id] :as order}]
  (assoc-in order-book [side id] order))

(defn load-orders [order-book orders]
  (reduce load-order order-book orders))

(defn remove-order [order-book side id]
  (update order-book side dissoc id))

(defn match-orders [order-book]
  (loop [{seller-id :id sell-price :price sell-size :size :as sell-order} (second (peek (:sell order-book)))
         {buyer-id :id buy-price :price buy-size :size :as buy-order} (second (peek (:buys order-book)))
         order-book (assoc order-book :sold 0)
         limit 0]
    (cond
      (> limit 100)
      order-book

      ;; nothing to match against
      (or (nil? sell-order) (nil? buy-order))
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

