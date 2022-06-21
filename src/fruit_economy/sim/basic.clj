(ns fruit-economy.sim.basic
  (:require [clojure.walk :as walk]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [datascript.core :as d]
            [fruit-economy.state :as state]
            [fruit-economy.gen-land :refer [make-temp-noise-map make-elev-noise-map process-noise-map]]
            [fruit-economy.data.core :refer [lookup-avet]]
            [fruit-economy.sim.market :refer [empty-order-book load-order load-orders remove-order match-orders]]
            [taoensso.timbre :refer [log]]
            [fruit-economy.infer.core :refer [rewrite infer infer-conn]])
  (:import [io.github.humbleui.skija Paint]))


(defn coord+entity->entities-coll [coord+entity]
  (reduce
    (fn [m [coord entity]]
      (conj m (assoc entity :place [:coord coord])))
    []
    coord+entity))

(defn make-bug []
  {:glyph "🐞" :wealth 0 :vision (inc (rand-int 4)) :hunger (inc (rand-int 4)) :max-age (+ (rand-int 5) 20)})

(defn resource-fn [local-elev sea-level x] (if (>= local-elev sea-level) (int (* (/ (+ x 1) 2) 5)) 0))

(defn decide-biome [local-temp local-elev sea-level]
  (cond
    (and (>= local-elev sea-level) (< local-elev 0.1)) :beach
    (and (>= local-elev 0.4) (< local-temp -0.2)) :snow-mountain
    (>= local-elev 0.4) :mountain
    ;; temperate region
    (> local-elev sea-level)
    (cond
      (< local-temp -0.2) :snow
      (< local-temp 0) :tundra
      (< local-temp 0.1) :grassland
      (< local-temp 0.2) :forest
      (< local-temp 0.3) :jungle
      :else :desert)
    :else :ocean))

(defn gen-bug-world [size n-peeps]
  (let [temp-noise (make-temp-noise-map size size)
        elev-noise (make-elev-noise-map size size)
        temp-mod 0.1 elev-mod 0
        temp (process-noise-map temp-noise temp-mod)
        elev (process-noise-map elev-noise elev-mod)
        sea-level (rand-nth (range 0.001 0.009 0.001))]
    (into
      [{:day 0}]
      (concat
        (for [x (range size)
              y (range size)]
          (let [local-temp (get-in temp [y x]) local-elev (get-in elev [y x])
                food (resource-fn local-elev sea-level local-temp)
                rock (resource-fn local-elev sea-level local-elev)]
            {:init-food food :food food :rock rock :coord [x y] :temp local-temp :elev local-elev :biome (decide-biome local-temp local-elev sea-level)}))
        (coord+entity->entities-coll
          (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)])))))))


(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(defn reset-world-db []
  (d/db-with (d/empty-db {:day {:db/index true}
                          :money {:db/index true}
                          :kind {:db/index true}
                          :good {:db/index true}
                          :place {:db/valueType :db.type/ref}
                          :coord {:db/unique :db.unique/identity}
                          :settlement/place {:db/valueType :db.type/ref}
                          :hometown {:db/valueType :db.type/ref}
                          :governs {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/one}})
    (gen-bug-world bug-world-size bug-count)))

(defn reset-world []
  (let [db (reset-world-db)]
    {:world-db db :world-conn (d/conn-from-db db) :map-view :default-view :day 0}))

(defn apply-rules [world-conn decision-rules reaction-rules]
  (-> world-conn
    (infer-conn decision-rules 1)
    (infer-conn reaction-rules 1)))

(defn lookup-day [db]
  (:day (first (lookup-avet db :day nil))))

(defn gen-summary [db]
  (let [peeps (lookup-avet db :kind :peep)
        food-factories (lookup-avet db :kind :food-factory)
        clothes-factories (lookup-avet db :kind :clothes-factory)
        cities (lookup-avet db :kind :city)
        money (lookup-avet db :money nil)
        day (lookup-day db)]
    {:peeps peeps
     :food-factories food-factories
     :clothes-factories clothes-factories
     :cities cities
     :money money
     :day day}))

(defn add-stats [stats world-db]
  (let [{:keys [peeps] :as summary} (gen-summary world-db)]
    (if (seq peeps)
      (conj stats summary)
      stats)))

(defn track-db [dbs world-db]
  (let [cities (lookup-avet world-db :kind :city)]
    (if (seq cities)
      (conj dbs world-db)
      dbs)))

(when (nil? @state/*world)
  (reset! state/*world (reset-world)))

(defn touch [e]
  (if e
    (d/touch e)
    e))

(defn coord-q [db coord]
  (-> (lookup-avet db :coord coord)
    first
    touch))

(defn units-q
  "coord can be nil or [x y]"
  [db coord]
  (->> (when coord
         [:coord coord])
    (lookup-avet db :place)
    (mapv :db/id)
    (d/pull-many db '[* {:place [:coord]}])
    (mapv #(update % :place :coord))))

(defn settlements-q
  "coord can be nil or [x y]"
  [db coord]
  (->> (when coord
         [:coord coord])
    (lookup-avet db :settlement/place)
    (mapv :db/id)
    (d/pull-many db '[* {:settlement/place [:coord] :_governs [*]}])
    (mapv #(update % :settlement/place :coord))))

(def days-pass-rule
  {:when '[[?e :day ?day]
           [(inc ?day) ?next-day]]
   :then '[[:db/add ?e :day ?next-day]]})

(defn sees [coord vision]
  (let [[x y] coord]
    (into []
      cat
      [(for [x (range (- x vision) (inc (+ x vision)))
             :when (not= [x y] coord)]
         [x y])
       (for [y (range (- y vision) (inc (+ y vision)))
             :when (not= [x y] coord)]
         [x y])])))

(defn best-food [db coord sees]
  (let [get-food (fn [coord]
                   (-> (lookup-avet db :coord coord [:food])
                     (first)
                     (get :food 0)))]
    (reduce
      (fn [[coord food] target]
        (let [target-food (get-food target)]
          (if (> target-food food)
            [target target-food]
            [coord food])
          (if (> target-food food)
            [target target-food]
            [coord food])))
      [coord (get-food coord)]
      sees)))

(defn hunt [db coord vision]
  (let [[best-food-coord _food] (best-food db coord (sees coord vision))]
    best-food-coord))

(def hunt-rule
  {:when '[[?e :place ?ce]
           [?e :vision ?vision]
           [?ce :coord ?coord]
           [?ce :food ?food]
           [(hunt $ ?coord ?vision) ?target]
           [(not= ?target ?coord)]
           [?te :coord ?target]]
   :then '[[:db/add ?e :place ?te]]
   :args {'hunt hunt}})

(defn gathered [food] (min food (+ (quot food 2) 2)))

(def try-eat-rule
  {:when '[[?e :place ?ce]
           [?e :wealth ?wealth]
           [?e :hunger ?hunger]
           [?ce :coord ?coord]
           [?ce :food ?food]
           [(gathered ?food) ?gather]
           [(- ?food ?gather) ?rem-food]
           [(+ ?wealth ?gather) ?gain-wealth]
           [(- ?gain-wealth ?hunger) ?rem-wealth]]
   :then '[[:db/add ?e :wealth ?rem-wealth]
           [:db/add ?ce :food ?rem-food]]
   :args {'gathered gathered}})

(def remove-starving-rule
  '{:when [[?e :wealth ?wealth]
           [(< ?wealth 0)]]
    :then [[:db/retractEntity ?e]]})

(defn grow-food [food init-food] (min (inc food) init-food))

(def grow-food-rule
  {:when '[[?e :food ?food]
           [?e :init-food ?init-food]
           [(< ?food ?init-food)]
           [(grow-food ?food ?init-food) ?gain-food]]
   :then '[[:db/add ?e :food ?gain-food]]
   :args {'grow-food grow-food}})

(defn spawn-factory-tx [idx attrs]
  (let [base-planning (rand-nth [1 3 5 8 15])]
    (into [[:db/add idx :hometown -1]
           [:db/add idx :money 10000]
           [:db/add idx :owed-tax 0]
           [:db/add idx :inventory 0]
           [:db/add idx :sold 0]
           [:db/add idx :last-sold 0]
           [:db/add idx :earned 0]
           [:db/add idx :last-earned 0]
           [:db/add idx :min-labour 2]
           [:db/add idx :labour-bought 0]
           [:db/add idx :base-planning base-planning]
           [:db/add idx :planning base-planning]
           [:db/add idx :labour/consumed 0]
           [:db/add idx :labour/last-consumed 0]]
      (map (fn [[k v]] [:db/add idx k v]))
      attrs)))

(def create-settlement-rule
  {:when '[[?e :wealth ?wealth]
           [(get-else $ ?e :settlement :db/missing) ?s]
           [(= :db/missing ?s)]
           [(>= ?wealth 10)]
           [(- ?wealth 10) ?rem-wealth]
           [?e :place ?place]
           [(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ?letter]
           [(str ?letter) ?name]]
   :then (into
           '[[:db/add ?e :wealth ?rem-wealth]
             [:db/add ?e :settlement -1]
             [:db/add -1 :kind :city]
             [:db/add -1 :settlement/name ?name]
             [:db/add -1 :settlement/place ?place]
             [:db/add -1 :food/market ?empty-order-book]
             [:db/add -1 :clothes/market ?empty-order-book]
             [:db/add -1 :labour/market ?empty-order-book]
             [:db/add -1 :food/market-price 0]
             [:db/add -1 :clothes/market-price 0]
             [:db/add -1 :labour/market-price 0]
             [:db/add -1 :food/price 1]
             [:db/add -1 :food/price-float 1]
             [:db/add -1 :food/price-velocity 1]
             [:db/add -1 :food/price-history []]
             [:db/add -1 :food/demand 1]
             [:db/add -1 :food/supply 1]
             [:db/add -1 :food/last-demand 1]
             [:db/add -1 :food/last-supply 1]
             [:db/add -1 :food/produced 0]
             [:db/add -1 :food/consumed 0]
             [:db/add -1 :food/last-produced 0]
             [:db/add -1 :food/last-consumed 0]
             [:db/add -1 :clothes/price 1]
             [:db/add -1 :clothes/price-float 1]
             [:db/add -1 :clothes/price-velocity 1]
             [:db/add -1 :clothes/price-history []]
             [:db/add -1 :clothes/demand 1]
             [:db/add -1 :clothes/supply 1]
             [:db/add -1 :clothes/last-demand 1]
             [:db/add -1 :clothes/last-supply 1]
             [:db/add -1 :clothes/produced 0]
             [:db/add -1 :clothes/consumed 0]
             [:db/add -1 :clothes/last-produced 0]
             [:db/add -1 :clothes/last-consumed 0]
             [:db/add -1 :labour/price 1]
             [:db/add -1 :labour/price-float 1]
             [:db/add -1 :labour/price-velocity 1]
             [:db/add -1 :labour/price-history []]
             [:db/add -1 :labour/demand 1]
             [:db/add -1 :labour/supply 1]
             [:db/add -1 :labour/last-demand 1]
             [:db/add -1 :labour/last-supply 1]
             [:db/add -1 :labour/produced 0]
             [:db/add -1 :labour/consumed 0]
             [:db/add -1 :labour/last-produced 0]
             [:db/add -1 :labour/last-consumed 0]
             [:db/add -2 :tax-rate 10]
             [:db/add -2 :money 10000]
             [:db/add -2 :governs -1]]
           (comp cat cat)
           [;; peeps
            (for [idx (range 10)]
              [[:db/add (- idx 100) :kind :peep]
               [:db/add (- idx 100) :hometown -1]
               [:db/add (- idx 100) :money 10000]
               [:db/add (- idx 100) :owed-tax 0]
               [:db/add (- idx 100) :labour 10]
               [:db/add (- idx 100) :health 10]
               [:db/add (- idx 100) :food 10]
               [:db/add (- idx 100) :clothes 10]
               [:db/add (- idx 100) :min-food 2]
               [:db/add (- idx 100) :min-clothes 1]
               [:db/add (- idx 100) :planning (rand-nth [1 3 5 8 15])]
               [:db/add (- idx 100) :sold 0]
               [:db/add (- idx 100) :last-sold 0]
               [:db/add (- idx 100) :earned 0]
               [:db/add (- idx 100) :last-earned 0]
               [:db/add (- idx 100) :labour/produced 0]
               [:db/add (- idx 100) :food/consumed 0]
               [:db/add (- idx 100) :clothes/consumed 0]
               [:db/add (- idx 100) :labour/last-produced 0]
               [:db/add (- idx 100) :food/last-consumed 0]
               [:db/add (- idx 100) :clothes/last-consumed 0]])
            ;; food factories
            (for [idx (range 5)]
              (spawn-factory-tx (- idx 200) {:kind :food-factory :good :food :decay 0.98 :production 0.7
                                             :food/produced 0 :food/last-produced 0}))
            ;; clothes factories
            (for [idx (range 5)]
              (spawn-factory-tx (- idx 300) {:kind :clothes-factory :good :clothes :decay 0.9 :production 2
                                             :clothes/produced 0 :clothes/last-produced 0}))])
   :args {'rand-nth rand-nth
          '?empty-order-book (empty-order-book)}})

(defn like-to-buy [money price percentage]
  (let [budget (int (* money percentage))]
    (quot budget price)))

(defn gen-orders [quantity-wanted price factories]
  (->
    (reduce
      (fn [[orders to-buy] {:keys [inventory labour] :as from}]
        (let [buyable (or inventory labour)]
          (cond
            (< buyable 1) [orders to-buy]
            (> to-buy 0)
            (let [buying (min to-buy buyable)
                  rem (- to-buy buying)
                  orders' (conj orders {:from from :buy buying :price price})]
              (if (<= rem 0)
                (reduced [orders' 0])
                [orders' rem]))
            :else [orders to-buy])))
      [[] quantity-wanted]
      factories)
    first))

(def peep-shop-rule
  (let [shop
        (fn [db peep-eid]
          (let [{:keys [money planning min-food min-clothes food clothes] :as peep} (d/entity db peep-eid)
                food-plan (* min-food planning) clothes-plan (* min-clothes planning)
                {home-eid :db/id
                 food-price :food/price
                 clothes-price :clothes/price
                 food-market :food/market
                 clothes-market :clothes/market
                 :as home} (get peep :hometown)

                existing-food-order? (get-in food-market [:buys peep-eid])
                food-price' (if existing-food-order?
                              (inc food-price)
                              (rand-nth [food-price (max (dec food-price) 1)]))

                existing-clothes-order? (get-in clothes-market [:buys peep-eid])
                clothes-price' (if existing-clothes-order?
                                 (inc clothes-price)
                                 (rand-nth [clothes-price (max (dec clothes-price) 1)]))

                food-like (like-to-buy money food-price' 0.4)
                clothes-like (like-to-buy money clothes-price' 0.2)
                food-want (min food-like food-plan) clothes-want (min clothes-like clothes-plan)]
            (log :info peep-eid :shop food-want clothes-want (d/touch peep))
            (log :info :food-like food-like :food-plan food-plan :clothes-like clothes-like :clothes-plan clothes-plan :food/demand (:food/demand home) :clothes/demand (:clothes/demand home))
            (log :info (mapv d/touch (lookup-avet db :good nil)))
            (cond-> []
              (> food-want 0)
              (conj [:db/add home-eid :food/market (load-order food-market {:price food-price' :size food-want :side :buys :id peep-eid :good-kw :food})])
              (> clothes-want 0)
              (conj [:db/add home-eid :clothes/market (load-order clothes-market {:price clothes-price' :size clothes-want :side :buys :id peep-eid :good-kw :clothes})]))))]
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :food ?food]
             [?e :clothes ?clothes]
             [?e :min-food ?min-food]
             [?e :min-clothes ?min-clothes]
             [(<= ?food ?min-food) ?not-enough-food]
             [(<= ?clothes ?min-clothes) ?not-enough-clothes]
             [(or ?not-enough-food ?not-enough-clothes)]
             [?home :food/price ?food-price]
             [?home :clothes/price ?clothes-price]]
     :then '[[:db.fn/call shop ?e]]
     :call {'shop shop}}))

(def peep-consume-rule
  (let [consume (fn [db peep-eid]
                  (let [{:keys [min-food min-clothes food clothes] :as peep} (d/entity db peep-eid)
                        base-labour 10 ;; reset labour

                        food-rem (max (- food min-food) 0)
                        clothes-rem (max (- clothes min-clothes) 0)

                        food-had (- food food-rem)
                        clothes-had (- clothes clothes-rem)

                        enough-food? (>= food-had min-food)
                        enough-clothes? (>= clothes-had min-clothes)
                        sold (:last-sold peep)
                        hometown (:hometown peep)
                        hometown-eid (:db/id hometown)
                        labour-market (:labour/market hometown)
                        labour-price (:labour/price hometown)
                        labour-produced (:labour/produced hometown)
                        food-consumed (:food/consumed hometown)
                        clothes-consumed (:clothes/consumed hometown)

                        existing-order? (get-in labour-market [:sell peep-eid])
                        no-work? (zero? sold)
                        fully-working? (and (not existing-order?) (> sold 0))
                        labour-price' (cond
                                        no-work?
                                        (max (dec labour-price) 1)

                                        fully-working?
                                        (inc labour-price)

                                        :else labour-price)]
                    (cond-> [[:db/add peep-eid :food food-rem]
                             [:db/add peep-eid :clothes clothes-rem]
                             [:db/add peep-eid :labour base-labour]
                             [:db/add peep-eid :labour/produced (+ (:labour/produced peep) base-labour)]
                             [:db/add peep-eid :food/consumed (+ (:food/consumed peep) food-had)]
                             [:db/add peep-eid :clothes/consumed (+ (:clothes/consumed peep) clothes-had)]
                             [:db/add hometown-eid :labour/market (load-order labour-market {:price labour-price' :size base-labour :side :sell :id peep-eid :good-kw :labour})]
                             [:db/add hometown-eid :labour/produced (+ labour-produced base-labour)]
                             [:db/add hometown-eid :food/consumed (+ food-consumed food-had)]
                             [:db/add hometown-eid :clothes/consumed (+ clothes-consumed clothes-had)]]
                      (or (not enough-food?) (not enough-clothes?))
                      (conj [:db/add peep-eid :health (max (dec (:health peep)) 0)])

                      (and enough-food? enough-clothes?)
                      (conj [:db/add peep-eid :health (min (inc (:health peep)) 10)])

                      #_#_;; let's not have labour ability be dynamic, we can worry about that later...
                      (or enough-food? enough-clothes?)
                      (conj [:db/add (:db/id hometown) :labour/supply (+ labour-supply (if enough-food? 5 2) (if enough-clothes? 5 2))]))))]
    {:when '[[?e :food _]
             [?e :clothes _]]
     :then '[[:db.fn/call consume ?e]]
     :call {'consume consume}}))

(def adjust-factories-planning-rule
  (let [adjust-factory (fn [_db factory-eid sold wanted planning base-planning]
                         (if (> sold wanted)
                           [[:db/add factory-eid :planning (inc planning)]]
                           [[:db/add factory-eid :planning (max (dec planning) base-planning)]]))]
    {:when '[[?e :last-sold ?sold]
             [?e :inventory ?inventory]
             [?e :planning ?planning]
             [?e :base-planning ?base-planning]
             [(* ?planning 10) ?wanted]
             [(not= ?sold ?wanted)]]
     :then '[[:db.fn/call adjust-factory ?e ?sold ?wanted ?planning ?base-planning]]
     :call {'adjust-factory adjust-factory}}))

(def reset-entity-rule
  (let [entity-sold (fn [db eid]
                      (let [{food-produced :food/produced
                             food-consumed :food/consumed
                             clothes-produced :clothes/produced
                             clothes-consumed :clothes/consumed
                             labour-produced :labour/produced
                             labour-consumed :labour/consumed
                             :keys [sold]} (d/entity db eid)]
                        (cond-> []
                          sold
                          (into [[:db/add eid :sold 0]
                                 [:db/add eid :last-sold sold]])
                          food-produced
                          (into [[:db/add eid :food/produced 0]
                                 [:db/add eid :food/last-produced food-produced]])
                          food-consumed
                          (into [[:db/add eid :food/consumed 0]
                                 [:db/add eid :food/last-consumed food-consumed]])
                          clothes-produced
                          (into [[:db/add eid :clothes/produced 0]
                                 [:db/add eid :clothes/last-produced clothes-produced]])
                          clothes-consumed
                          (into [[:db/add eid :clothes/consumed 0]
                                 [:db/add eid :clothes/last-consumed clothes-consumed]])
                          labour-produced
                          (into [[:db/add eid :labour/produced 0]
                                 [:db/add eid :labour/last-produced labour-produced]])
                          labour-consumed
                          (into [[:db/add eid :labour/consumed 0]
                                 [:db/add eid :labour/last-consumed labour-consumed]]))))]
    {:when '[(or
               [?e :sold _]
               [?e :food/consumed _]
               [?e :clothes/consumed _]
               [?e :labour/produced _])]
     :then '[[:db.fn/call entity-sold ?e]]
     :call {'entity-sold entity-sold}}))

(def hire-rule
  (let [hire (fn [db factory-eid]
               (println :hire)
               (let [{:keys [money last-sold planning min-labour labour-bought inventory decay production good] :as factory} (d/entity db factory-eid)
                     _ (log :info :sold last-sold)
                     labour-plan (* min-labour planning 10)
                     {home-eid :db/id
                      labour-price :labour/price
                      labour-market :labour/market
                      :as home} (get factory :hometown)

                     existing-order? (get-in labour-market [:buys factory-eid])
                     no-hires? (zero? labour-bought)
                     fully-hired? (and (not existing-order?) (> labour-bought 0))
                     labour-price' (cond
                                     fully-hired?
                                     (max (dec labour-price) 1)

                                     no-hires?
                                     (inc labour-price)

                                     :else labour-price)

                     labour-like (like-to-buy money labour-price' 0.8)
                     labour-want (min labour-like labour-plan)
                     _ (log :info :hire :labour-like labour-like :money money :labour-want labour-want)]
                 (when (> labour-want 0)
                   [[:db/add home-eid :labour/market (load-order labour-market {:price labour-price' :size labour-want :side :buys :id factory-eid :good-kw :labour-bought})]])))]
    ;; This models ad-hoc labour, finding work, which could be dice roll based etc, is separate to this, peeps keep doing ad-hoc work while "looking" for a job until they find one, then they stop, unless they switch again.
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :inventory ?inventory]
             [(< ?inventory 100)]]
     :then '[[:db.fn/call hire ?e]]
     :call {'hire hire}}))

(def craft-rule
  (let [craft (fn [db factory-eid]
                (let [{:keys [money last-sold min-labour labour-bought inventory decay production good] :as factory} (d/entity db factory-eid)
                      _ (log :info :sold last-sold)
                      {home-eid :db/id
                       labour-consumed :labour/consumed
                       :as home} (get factory :hometown)
                      produced (long (* production (Math/log (+ labour-bought 1))))
                      decayed-inventory (long (* inventory decay))
                      inventory' (+ decayed-inventory produced)
                      good-market-key (condp = good :food :food/market :clothes :clothes/market)
                      good-price-key (condp = good :food :food/price :clothes :clothes/price)
                      good-produced-key (condp = good :food :food/produced :clothes :clothes/produced)

                      market (good-market-key home)
                      existing-order? (get-in market [:sell factory-eid])

                      price (good-price-key home)
                      sold-nothing? (zero? last-sold)
                      sold-out? (and (not existing-order?) (> last-sold 0))
                      price' (cond
                               sold-nothing?
                               (max (dec price) 1)

                               sold-out?
                               (inc price)

                               :else price)]
                  (log :info factory-eid :craft :labour-bought labour-bought (d/touch factory))
                  (cond-> [[:db/add home-eid good-produced-key (+ (good-produced-key home) produced)]
                           [:db/add factory-eid :inventory inventory']]

                    (> inventory' 0)
                    (conj [:db/add home-eid good-market-key (load-order market {:price price' :size inventory' :side :sell :id factory-eid :good-kw :inventory})])

                    (and (zero? inventory') existing-order?)
                    (conj [:db/add home-eid good-market-key (remove-order market :sell factory-eid)])

                    (>= labour-bought min-labour)
                    (into
                      [[:db/add home-eid :labour/consumed (+ labour-consumed labour-bought)]
                       [:db/add factory-eid :labour/consumed (+ (:labour/consumed factory) labour-bought)]
                       [:db/add factory-eid good-produced-key (+ (good-produced-key factory) produced)]]))))]
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :inventory ?inventory]
             [(< ?inventory 100)]]
     :then '[[:db.fn/call craft ?e]]
     :call {'craft craft}}))

(defn update-attrs [db eid attr-fn-vals]
  (let [ent (d/entity db eid)]
    (into []
      (map (fn [[attr f val]]
             [:db/add eid attr (f (get ent attr) val)]))
      attr-fn-vals)))

(defn process-matched [db matches]
  (reduce
    (fn [tx {:keys [buyer seller size price] :as order}]
      (let [cost (* size price)
            bought-kw (get-in order [:buy-order :good-kw])
            sold-kw (get-in order [:sell-order :good-kw])]
        (into tx
          [[:db.fn/call update-attrs seller [[:money + cost]
                                             [:sold + size]
                                             [:earned + cost]
                                             [sold-kw - size]]]
           [:db.fn/call update-attrs buyer [[:money - cost]
                                            [bought-kw + size]]]])))
    []
    matches))

(def match-markets-rule
  (let [match-market
        (fn [db town-eid]
          (println :match-market)
          (let [{food-market :food/market
                 clothes-market :clothes/market
                 labour-market :labour/market
                 :as town} (d/entity db town-eid)
                market->tx (fn [town-eid market {:keys [market-key demand-key supply-key market-price-key]}]
                             (let [{matched :matched :keys [sold current-price] :as market'} (match-orders market)]
                               (if (and (= market market') (empty? matched))
                                 ;; There's no change
                                 []
                                 (let [process-matched-tx (process-matched db matched)
                                       market'' (assoc market' :matched [] :sold 0)
                                       [demand supply] ((juxt (comp count :buys) (comp count :sell)) market'')]
                                   (into process-matched-tx
                                     [[:db/add town-eid market-key market'']
                                      [:db/add town-eid demand-key demand]
                                      [:db/add town-eid supply-key supply]
                                      [:db/add town-eid market-price-key current-price]])))))]
            ;; TODO: Just have this running once? So we only have one market match, but for now maybe run it twice
            (println :town town)
            (into []
              cat
              [(when (seq food-market)
                 (market->tx town-eid food-market {:market-key :food/market :demand-key :food/demand :supply-key :food/supply :market-price-key :food/market-price}))
               (when (seq clothes-market)
                 (market->tx town-eid clothes-market {:market-key :clothes/market :demand-key :clothes/demand :supply-key :clothes/supply :market-price-key :clothes/market-price}))
               (when (seq labour-market)
                 (market->tx town-eid labour-market {:market-key :labour/market :demand-key :labour/demand :supply-key :labour/supply :market-price-key :labour/market-price}))])))]
    {:when '[(or
               [?e :food/market _]
               [?e :clothes/market _]
               [?e :labour/market _])]
     :then '[[:db.fn/call match-market ?e]]
     :call {'match-market match-market}}))

(defn conj-to-limit
  "conj's a value to a vector to the specified limit"
  [v limit x]
  (let [v' (conj v x)
        size (count v')]
    (if (> size limit)
      (subvec v' (- size limit) size)
      v')))

(def price-history-limit (* 30 36))
(def max-price (long 1e12))

(def update-prices-rule
  (let [;; Post some experimentation growth factor should probably sit between 1.0001 and 1.001,
        ;;   which gives us between 5.4% and 37.78% price increase in a year in conditions of constant demand.
        ;;   We'll set it at 1.0005 for now, which is 11.35%.
        ;; I'll defer doing the same for the deflation until I have a better situation to build a model for it.
        inflate-by 1.0005
        deflate-by -0.5
        update-price-velocity (fn [supply demand price-velocity]
                                (let [more-demand? (> demand supply)
                                      more-supply? (< demand supply)]
                                  (cond
                                    (and more-demand? (> price-velocity 0))
                                    ;; TODO: use whole numbers instead of
                                    (max (* price-velocity inflate-by) 0.01)

                                    more-demand?
                                    (max (* price-velocity deflate-by) 0.01)

                                    (and more-supply? (< price-velocity 0))
                                    (min (* price-velocity inflate-by) -0.01)

                                    more-supply?
                                    (min (* price-velocity deflate-by) -0.01)

                                    :else 0)))
        #_#_
        update-price-velocity (fn [supply demand price-velocity]
                                (let [more-demand? (> demand supply)
                                      more-supply? (< demand supply)]
                                  (cond
                                    (and more-demand? (> price-velocity 0))
                                    (max (+ (quot price-velocity 2) price-velocity) 1)

                                    more-demand?
                                    (max (- (quot price-velocity 2)) 1)

                                    (and more-supply? (< price-velocity 0))
                                    (min (+ (quot price-velocity 2) price-velocity) -1)

                                    more-supply?
                                    (min (- (quot price-velocity 2)) -1)

                                    :else 0)))
        update-prices (fn [db town-eid]
                        (let [{food-price-float :food/price-float food-price-velocity :food/price-velocity food-price :food/price food-market-price :food/market-price food-price-history :food/price-history food-supply :food/supply food-demand :food/demand food-produced :food/produced food-consumed :food/consumed
                               clothes-price-float :clothes/price-float clothes-price-velocity :clothes/price-velocity clothes-price :clothes/price clothes-market-price :clothes/market-price clothes-price-history :clothes/price-history clothes-supply :clothes/supply clothes-demand :clothes/demand clothes-produced :clothes/produced clothes-consumed :clothes/consumed
                               labour-price-float :labour/price-float labour-price-velocity :labour/price-velocity labour-price :labour/price labour-market-price :labour/market-price labour-price-history :labour/price-history labour-supply :labour/supply labour-demand :labour/demand labour-produced :labour/produced labour-consumed :labour/consumed
                               :as town} (d/entity db town-eid)
                              food-price-velocity' (update-price-velocity food-supply food-demand food-price-velocity)
                              food-price-float' (+ food-price-float food-price-velocity')
                              clothes-price-velocity' (update-price-velocity clothes-supply clothes-demand clothes-price-velocity)
                              clothes-price-float' (+ clothes-price-float clothes-price-velocity')
                              labour-price-velocity' (update-price-velocity labour-supply labour-demand labour-price-velocity)
                              labour-price-float' (+ labour-price-float labour-price-velocity')

                              food-price-float'' (min (max food-price-float' 1) max-price)
                              food-price' (min (max (long food-price-float') 1) max-price)
                              clothes-price-float'' (min (max clothes-price-float' 1) max-price)
                              clothes-price' (min (max (long clothes-price-float') 1) max-price)
                              labour-price-float'' (min (max labour-price-float' 1) max-price)
                              labour-price' (min (max (long labour-price-float') 1) max-price)]
                          [[:db/add town-eid :food/price-velocity food-price-velocity']
                           #_[:db/add town-eid :food/price-float (max food-price-float' 1)]
                           #_[:db/add town-eid :food/price (max (quot food-price-float' 100) 1)]
                           [:db/add town-eid :food/supply 0]
                           [:db/add town-eid :food/demand 0]
                           [:db/add town-eid :food/last-supply food-supply]
                           [:db/add town-eid :food/last-demand food-demand]
                           [:db/add town-eid :food/produced 0]
                           [:db/add town-eid :food/consumed 0]
                           [:db/add town-eid :food/last-produced food-produced]
                           [:db/add town-eid :food/last-consumed food-consumed]
                           [:db/add town-eid :clothes/price-velocity clothes-price-velocity']
                           #_[:db/add town-eid :clothes/price-float (max clothes-price-float' 1)]
                           #_[:db/add town-eid :clothes/price (max (quot clothes-price-float' 100) 1)]
                           [:db/add town-eid :clothes/supply 0]
                           [:db/add town-eid :clothes/demand 0]
                           [:db/add town-eid :clothes/last-supply clothes-supply]
                           [:db/add town-eid :clothes/last-demand clothes-demand]
                           [:db/add town-eid :clothes/produced 0]
                           [:db/add town-eid :clothes/consumed 0]
                           [:db/add town-eid :clothes/last-produced clothes-produced]
                           [:db/add town-eid :clothes/last-consumed clothes-consumed]
                           [:db/add town-eid :labour/price-velocity labour-price-velocity']
                           #_[:db/add town-eid :labour/price-float (max labour-price-float' 1)]
                           #_[:db/add town-eid :labour/price (max (quot labour-price-float' 100) 1)]
                           [:db/add town-eid :labour/supply 0]
                           [:db/add town-eid :labour/demand 0]
                           [:db/add town-eid :labour/last-supply labour-supply]
                           [:db/add town-eid :labour/last-demand labour-demand]
                           [:db/add town-eid :labour/produced 0]
                           [:db/add town-eid :labour/consumed 0]
                           [:db/add town-eid :labour/last-produced labour-produced]
                           [:db/add town-eid :labour/last-consumed labour-consumed]

                           ;#_#_#_#_#_#_#_#_#_
                           #_#_
                           [:db/add town-eid :food/price-float food-price-float'']
                           [:db/add town-eid :food/price food-price']
                           [:db/add town-eid :food/price-history (conj-to-limit food-price-history price-history-limit food-price)]
                           #_#_
                           [:db/add town-eid :clothes/price-float clothes-price-float'']
                           [:db/add town-eid :clothes/price clothes-price']
                           [:db/add town-eid :clothes/price-history (conj-to-limit clothes-price-history price-history-limit clothes-price)]
                           #_#_
                           [:db/add town-eid :labour/price-float labour-price-float'']
                           [:db/add town-eid :labour/price labour-price']
                           [:db/add town-eid :labour/price-history (conj-to-limit labour-price-history price-history-limit labour-price)]

                           [:db/add town-eid :food/price food-market-price]
                           [:db/add town-eid :clothes/price clothes-market-price]
                           [:db/add town-eid :labour/price labour-market-price]]))]

    {:when '[[?e :food/price _]]
     :then '[[:db.fn/call update-prices ?e]]
     :call {'update-prices update-prices}}))

(defn gen-tax-tx [gov-ent gov-money tax-rate tax-day? citizens]
  (let [{:keys [pay-tx tax-earnings]} (reduce
                                        (fn [state ent]
                                          (let [ent-id (:db/id ent)
                                                money (:money ent)
                                                earned (:earned ent)
                                                owed-tax (:earned ent)
                                                tax (long (* tax-rate earned))
                                                owed-tax' (+ owed-tax tax)
                                                peep-tax-tx (cond-> [[:db/add ent-id :money (- money tax)]
                                                                     [:db/add ent-id :earned 0]
                                                                     [:db/add ent-id :last-earned earned]]
                                                              (not tax-day?)
                                                              (conj [:db/add ent-id :owed-tax owed-tax'])
                                                              tax-day?
                                                              (conj [:db/add ent-id :owed-tax 0]))]
                                            (log :info :money-tax! :eid ent-id :money money :tax tax :ent (touch ent))
                                            (-> state
                                              (update :pay-tx into peep-tax-tx)
                                              (cond->
                                                tax-day?
                                                (update :tax-earnings + owed-tax')))))
                                        {:pay-tx [] :tax-earnings 0}
                                        citizens)]
    (conj pay-tx [:db/add gov-ent :money (+ gov-money tax-earnings)])))

(def state-tax-rule
  (let [tax (fn [db day]
              (let [governments (lookup-avet db :governs nil)
                    tax-day? (zero? (mod day 30))]
                (log :info :state-tax)
                (reduce
                  (fn [v {gov-ent :db/id
                          gov-money :money
                          :keys [governs tax-rate] :as gov}]
                    (let [tax-rate (/ tax-rate 100)
                          tax-tx (gen-tax-tx gov-ent gov-money tax-rate tax-day? (:_hometown governs))]
                      (log :info (mapv (juxt :db/id :money) (:_hometown governs)) (:governs gov) tax-tx)
                      (into v tax-tx)))
                  []
                  governments)))]
    {:when '[[?e :day ?day]]
     :then '[[:db.fn/call tax ?day]]
     :call {'tax tax}}))

(def decision-rules
  [days-pass-rule
   hunt-rule
   try-eat-rule
   grow-food-rule

   create-settlement-rule
   peep-shop-rule
   peep-consume-rule
   adjust-factories-planning-rule
   hire-rule
   craft-rule
   match-markets-rule
   update-prices-rule
   reset-entity-rule
   state-tax-rule])

(def reaction-rules
  [remove-starving-rule])

(def *sim-broken (atom nil))

(defn tick-world [*world]
  (if (nil? @*sim-broken)
    (try
      (as-> *world $
        (update $ :world-conn apply-rules decision-rules reaction-rules)
        (assoc $ :world-db (d/db (:world-conn $)))
        (assoc $ :day (lookup-day (:world-db $)))
        (update $ :dbs (fnil track-db []) (:world-db $))
        (update $ :stats (fnil add-stats []) (:world-db $)))
      (catch Exception e
        (println :BROKE!)
        (reset! *sim-broken e)
        *world))
    *world))

(comment
  (identity @*sim-broken))

(defn do-tick-world []
  (swap! state/*world tick-world))

(defn tick-world-10x []
  (dotimes [_ 10]
    (do-tick-world)))

(defn tick-world-100x []
  (dotimes [_ 100]
    (do-tick-world)))

(defn tick-world-1000x []
  (dotimes [_ 1000]
    (do-tick-world)))

(defn history-started? [world-db]
  (< 30 (lookup-day world-db)))

(defn viable-world? [world-db]
  (seq (lookup-avet world-db :kind :city)))


