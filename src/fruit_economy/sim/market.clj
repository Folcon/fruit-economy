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

(defn market-stats
  "track OHLCV stats for market"
  [market price size]
  (-> market
    (update :open-price (fn [open] (if (nil? open) price open)))
    (assoc :close-price price)
    (update :high-price (fnil max Long/MIN_VALUE) price)
    (update :low-price (fnil min Long/MAX_VALUE) price)
    (update :sold + size)))

(defn calculate-current-price [{:keys [matched high-price low-price close-price] :as market}]
  (let [avg (fn [orders]
              (let [[sum num] (reduce (fn [[n num] {:keys [price size]}] [(+ n (* price size)) (+ num size)]) [0 0] orders)
                    q (quot sum num)
                    r (rem sum num)]
                (if (zero? r)
                  q
                  (inc q))))
        matches? (seq matched)]
    (cond
      (and matches? (= high-price low-price))
      (assoc market :current-price close-price)

      matches?
      (assoc market :current-price (avg matched))

      :else
      market)))

(defn match-orders [order-book]
  (loop [{seller-id :id sell-price :price sell-size :size :as sell-order} (second (peek (:sell order-book)))
         {buyer-id :id buy-price :price buy-size :size :as buy-order} (second (peek (:buys order-book)))
         order-book (assoc order-book :sold 0)
         limit 0]
    (cond
      (> limit 100)
      (calculate-current-price order-book)

      ;; nothing to match against
      (or (nil? sell-order) (nil? buy-order))
      (calculate-current-price order-book)

      ;; stuff for sale is too expensive
      (< buy-price sell-price)
      (calculate-current-price order-book)

      ;; TODO: With self trade issues - maybe offer different defaults to cancel ie: :self-trade-expire [:buy|:sell|:old|:new], if key is set, then follow, with priority to follow :sell if both set.
      ;; check that a self-trade isn't occurring, if so, kill the buy side
      (= buyer-id seller-id)
      (let [order-book' (-> order-book
                          (update :buys pop))
            sell-order' (second (peek (:sell order-book')))
            buy-order' (second (peek (:buys order-book')))]
        (if buy-order'
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          (calculate-current-price order-book')))

      (= sell-size buy-size)
      (let [order-book' (-> order-book
                          (update :matched conj {:price sell-price :size buy-size :seller seller-id :buyer buyer-id :buy-order buy-order :sell-order sell-order})
                          (update :sell pop)
                          (update :buys pop)
                          (market-stats (:price sell-order) buy-size))
            sell-order' (second (peek (:sell order-book')))
            buy-order' (second (peek (:buys order-book')))]
        (if (and buy-order' sell-order')
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          (calculate-current-price order-book')))


      (> sell-size buy-size)
      (let [order-book' (-> order-book
                          (update :matched conj {:price sell-price :size buy-size :seller seller-id :buyer buyer-id :buy-order buy-order :sell-order sell-order})
                          (update :buys pop)
                          (update-in [:sell seller-id :size] - buy-size)
                          (market-stats (:price sell-order) buy-size))
            sell-order' (second (peek (:sell order-book')))
            buy-order' (second (peek (:buys order-book')))]
        (if buy-order'
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          (calculate-current-price order-book')))

      (< sell-size buy-size)
      (let [order-book' (-> order-book
                          (update :matched conj {:price sell-price :size sell-size :seller seller-id :buyer buyer-id :buy-order buy-order :sell-order sell-order})
                          (update :sell pop)
                          (update-in [:buys buyer-id :size] - sell-size)
                          (market-stats (:price sell-order) sell-size))
            sell-order' (second (peek (:sell order-book')))
            buy-order' (second (peek (:buys order-book')))]
        (if sell-order'
          (recur
            sell-order'
            buy-order'
            order-book'
            (inc limit))
          (calculate-current-price order-book'))))))

(defn collect-price-levels [market-side]
  (when (seq market-side)
    (reduce
      (fn [m [_ {:keys [price] :as order}]]
        (update m price (fnil conj []) order))
      (sorted-map)
      market-side)))

(defn collect-price-sizes [market-side]
  (when (seq market-side)
    (reduce
      (fn [m [_ {:keys [price size] :as order}]]
        (update m price (fnil + 0) size))
      {}
      market-side)))

(defn collect-price-stats [market-side]
  (when (seq market-side)
    (let [high+low-price (into [] (comp (map (juxt (comp :price second))) cat) ((juxt first last) market-side))]
      {:max (apply max high+low-price)
       :min (apply min high+low-price)})))

(defn market-summary [{:keys [buys sell] :as market}]
  (cond-> market
    (seq buys)
    (->
      (assoc-in [:summary :level :buys] (collect-price-levels buys))
      (assoc-in [:summary :size :buys] (collect-price-sizes buys))
      (assoc-in [:summary :stat :buys] (collect-price-stats buys)))
    (seq sell)
    (->
      (assoc-in [:summary :level :sell] (collect-price-levels sell))
      (assoc-in [:summary :size :sell] (collect-price-sizes sell))
      (assoc-in [:summary :stat :sell] (collect-price-stats sell)))))

