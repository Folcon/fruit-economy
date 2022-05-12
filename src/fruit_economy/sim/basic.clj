(ns fruit-economy.sim.basic
  (:require [clojure.walk :as walk]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [datascript.core :as d]
            [fruit-economy.gen-land :refer [make-temp-noise-map make-elev-noise-map process-noise-map]]
            [fruit-economy.land :refer [render-tile-colour]]
            [fruit-economy.colour :refer [colour colour-noise]]
            [fruit-economy.data.core :refer [lookup-avet]]
            [fruit-economy.humble-ui :as custom-ui])
  (:import [io.github.humbleui.skija Paint]))


(defn rewrite
  "Tries to match rule against db
  Returns tx-data or empty list if rule does not match"
  [{:keys [when then args call]} db]
  (let [ds-var? (fn [sym] (= (first (name sym)) \?))
        syms (for [row then el row :when (and (symbol? el) (ds-var? el))] el)
        results (apply d/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args))]
    (for [match results tx then]
      (let [swaps (merge
                    (zipmap syms match)
                    call)
            f (fn [x] (if (coll? x) x (get swaps x x)))]
        (walk/postwalk f tx)))))

(defn infer
  "Returns a new db with all inferred facts and a list of tx-data.
  Stops when no more rules apply or after 100 iterations"
  ([db rules] (infer db rules 100))
  ([db rules max-iter]
   (loop [db db max-iter max-iter]
     (let [tx-data (for [rule rules tx (rewrite rule db)] tx)]
       (cond
         (empty? tx-data) db
         (zero? max-iter) db
         :else (recur (d/db-with db tx-data) (dec max-iter)))))))


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
            {:init-food food :food food :rock rock :coord [x y] :temp local-temp :elev local-elev :biome (decide-biome local-temp local-elev sea-level)})))
      (coord+entity->entities-coll
        (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)]))))))


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
                          :hometown {:db/valueType :db.type/ref}})
    (gen-bug-world bug-world-size bug-count)))

(defn reset-world []
  {:world-db (reset-world-db) :map-view :default-view})

(defn apply-rules [world-db decision-rules reaction-rules]
  (-> world-db
    (infer decision-rules 1)
    (infer reaction-rules 1)))

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
     :day (:day (first day))}))

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

(defonce *world (atom (reset-world)))

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
    (d/pull-many db '[* {:settlement/place [:coord]}])
    (mapv #(update % :settlement/place :coord))))

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
             [:db/add -1 :food/price 1]
             [:db/add -1 :food/price-float 1]
             [:db/add -1 :food/price-velocity 1]
             [:db/add -1 :food/demand 1]
             [:db/add -1 :food/supply 1]
             [:db/add -1 :food/last-demand 1]
             [:db/add -1 :food/last-supply 1]
             [:db/add -1 :clothes/price 1]
             [:db/add -1 :clothes/price-float 1]
             [:db/add -1 :clothes/price-velocity 1]
             [:db/add -1 :clothes/demand 1]
             [:db/add -1 :clothes/supply 1]
             [:db/add -1 :clothes/last-demand 1]
             [:db/add -1 :clothes/last-supply 1]
             [:db/add -1 :labour/price 1]
             [:db/add -1 :labour/price-float 1]
             [:db/add -1 :labour/price-velocity 1]
             [:db/add -1 :labour/demand 1]
             [:db/add -1 :labour/supply 1]
             [:db/add -1 :labour/last-demand 1]
             [:db/add -1 :labour/last-supply 1]]
           (comp cat cat)
           [;; peeps
            (for [idx (range 10)]
              [[:db/add (- idx 100) :kind :peep]
               [:db/add (- idx 100) :hometown -1]
               [:db/add (- idx 100) :money 10000]
               [:db/add (- idx 100) :labour 10]
               [:db/add (- idx 100) :health 10]
               [:db/add (- idx 100) :food 10]
               [:db/add (- idx 100) :clothes 10]
               [:db/add (- idx 100) :min-food 2]
               [:db/add (- idx 100) :min-clothes 1]
               [:db/add (- idx 100) :planning (rand-nth [1 3 5 8 15])]])
            ;; food factories
            (for [idx (range 1)]
              [[:db/add (- idx 200) :kind :food-factory]
               [:db/add (- idx 200) :hometown -1]
               [:db/add (- idx 200) :good :food]
               [:db/add (- idx 200) :decay 0.98]
               [:db/add (- idx 200) :production 0.7]
               [:db/add (- idx 200) :money 10000]
               [:db/add (- idx 200) :inventory 0]
               [:db/add (- idx 200) :min-labour 2]
               [:db/add (- idx 200) :planning (rand-nth [1 3 5 8 15])]])
            ;; clothes factories
            (for [idx (range 1)]
              [[:db/add (- idx 300) :kind :clothes-factory]
               [:db/add (- idx 300) :hometown -1]
               [:db/add (- idx 300) :good :clothes]
               [:db/add (- idx 300) :decay 0.9]
               [:db/add (- idx 300) :production 2]
               [:db/add (- idx 300) :money 10000]
               [:db/add (- idx 300) :inventory 0]
               [:db/add (- idx 300) :min-labour 2]
               [:db/add (- idx 300) :planning (rand-nth [1 3 5 8 15])]])])
   :args {'rand-nth rand-nth}})

(defn like-to-buy [money price percentage]
  (let [budget (int (* money percentage))]
    (quot budget price)))

(def peep-shop-rule
  (let [shop
        (fn [db peep-eid]
          (let [{:keys [money planning min-food min-clothes food clothes] :as peep} (d/entity db peep-eid)
                food-plan (* min-food planning) clothes-plan (* min-clothes planning)
                {food-price :food/price
                 clothes-price :clothes/price
                 :as home} (get peep :hometown)
                food-like (like-to-buy money food-price 0.4)
                clothes-like (like-to-buy money clothes-price 0.2)
                food-want (min food-like food-plan) clothes-want (min clothes-like clothes-plan)
                food-cost (* food-want food-price) clothes-cost (* clothes-want clothes-price)
                food-bought food-want clothes-bought clothes-want]
            (println peep-eid :shop food-want clothes-want :food-bought food-bought food :clothes-bought clothes-bought clothes (d/touch peep))
            (println :food-like food-like :food-plan food-plan :clothes-like clothes-like :clothes-plan clothes-plan :food/demand (:food/demand home) :clothes/demand (:clothes/demand home))
            (println (mapv d/touch (lookup-avet db :good nil)))
            [[:db/add (:db/id home) :food/demand (+ (:food/demand home) food-want)]
             [:db/add (:db/id home) :clothes/demand (+ (:clothes/demand home) clothes-want)]
             [:db/add peep-eid :money (- money food-cost clothes-cost)]
             [:db/add peep-eid :food (+ food food-bought)]
             [:db/add peep-eid :clothes (+ clothes clothes-bought)]]))]
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

                        food-rem (- food min-food)
                        clothes-rem (- clothes min-clothes)

                        enough-food? (>= food-rem 0)
                        enough-clothes? (>= clothes-rem 0)
                        hometown (:hometown peep)
                        labour-supply (:labour/supply hometown)]
                    (cond-> [[:db/add peep-eid :food (max food-rem 0)]
                             [:db/add peep-eid :clothes (max clothes-rem 0)]
                             [:db/add peep-eid :labour base-labour]
                             [:db/add (:db/id hometown) :labour/supply (+ labour-supply base-labour)]]
                      (or (not enough-food?) (not enough-clothes?))
                      (conj [:db/add peep-eid :health (max (dec (:health peep)) 0)])

                      (and enough-food? enough-clothes?)
                      (conj [:db/add peep-eid :health (min (inc (:health peep)) 10)])

                      #_#_ ;; let's not have labour ability be dynamic, we can worry about that later...
                      (or enough-food? enough-clothes?)
                      (conj [:db/add (:db/id hometown) :labour/supply (+ labour-supply (if enough-food? 5 2) (if enough-clothes? 5 2))]))))]
    {:when '[[?e :food _]
             [?e :clothes _]]
     :then '[[:db.fn/call consume ?e]]
     :call {'consume consume}}))

(def craft-rule
  (let [craft (fn [db factory-eid]
                (let [{:keys [money planning min-labour inventory decay production good] :as factory} (d/entity db factory-eid)
                      labour-plan (* min-labour planning)
                      {labour-price :labour/price
                       labour-supply :labour/supply
                       :as home} (get factory :hometown)
                      labour-like (like-to-buy money labour-price 0.8)
                      labour-want (min labour-like labour-plan)
                      labour-bought (min labour-want labour-supply)
                      labour-cost (* labour-bought labour-price)
                      produced (long (* production (Math/log (+ labour-bought 1))))
                      decayed-inventory (long (* inventory decay))
                      good-supply-key (condp = good :food :food/supply :clothes :clothes/supply)]
                  (println factory-eid :craft labour-want :labour-bought labour-bought (d/touch factory))
                  (cond-> [[:db/add (:db/id home) good-supply-key (+ (good-supply-key home) produced)]
                           [:db/add (:db/id home) :labour/demand (+ (:labour/demand home) labour-want)]
                           [:db/add factory-eid :inventory (+ decayed-inventory produced)]]

                    (>= labour-bought min-labour)
                    (into [[:db/add (:db/id home) :labour/supply (- labour-supply labour-bought)]
                           [:db/add factory-eid :money (- money labour-cost)]]))))]
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :inventory ?inventory]
             [(< ?inventory 100)]]
     :then '[[:db.fn/call craft ?e]]
     :call {'craft craft}}))

(def update-prices-rule
  (let [update-price-velocity (fn [supply demand price-velocity]
                                (let [more-demand? (> demand supply)
                                      more-supply? (< demand supply)]
                                  (cond
                                    (and more-demand? (> price-velocity 0))
                                    ;; TODO: use whole numbers instead of
                                    (max (* price-velocity 1.5) 0.01)

                                    more-demand?
                                    (max (* price-velocity -0.5) 0.01)

                                    (and more-supply? (< price-velocity 0))
                                    (max (* price-velocity 1.5) -0.01)

                                    more-supply?
                                    (min (* price-velocity -0.5) -0.01)

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
                                   (max (+ (quot price-velocity 2) price-velocity) -1)

                                   more-supply?
                                   (min (- (quot price-velocity 2)) -1)

                                   :else 0)))
        update-prices (fn [db town-eid]
                        (let [{food-price-float :food/price-float food-price-velocity :food/price-velocity food-supply :food/supply food-demand :food/demand
                               clothes-price-float :clothes/price-float clothes-price-velocity :clothes/price-velocity clothes-supply :clothes/supply clothes-demand :clothes/demand
                               labour-price-float :labour/price-float labour-price-velocity :labour/price-velocity labour-supply :labour/supply labour-demand :labour/demand
                               :as town} (d/entity db town-eid)
                              food-price-velocity' (update-price-velocity food-supply food-demand food-price-velocity)
                              food-price-float' (+ food-price-float food-price-velocity')
                              clothes-price-velocity' (update-price-velocity clothes-supply clothes-demand clothes-price-velocity)
                              clothes-price-float' (+ clothes-price-float clothes-price-velocity')
                              labour-price-velocity' (update-price-velocity labour-supply labour-demand labour-price-velocity)
                              labour-price-float' (+ labour-price-float labour-price-velocity')]
                          [[:db/add town-eid :food/price-velocity food-price-velocity']
                           #_[:db/add town-eid :food/price-float (max food-price-float' 1)]
                           #_[:db/add town-eid :food/price (max (quot food-price-float' 100) 1)]
                           [:db/add town-eid :food/supply 0]
                           [:db/add town-eid :food/demand 0]
                           [:db/add town-eid :food/last-supply food-supply]
                           [:db/add town-eid :food/last-demand food-demand]
                           [:db/add town-eid :clothes/price-velocity clothes-price-velocity']
                           #_[:db/add town-eid :clothes/price-float (max clothes-price-float' 1)]
                           #_[:db/add town-eid :clothes/price (max (quot clothes-price-float' 100) 1)]
                           [:db/add town-eid :clothes/supply 0]
                           [:db/add town-eid :clothes/demand 0]
                           [:db/add town-eid :clothes/last-supply clothes-supply]
                           [:db/add town-eid :clothes/last-demand clothes-demand]
                           [:db/add town-eid :labour/price-velocity labour-price-velocity']
                           #_[:db/add town-eid :labour/price-float (max labour-price-float' 1)]
                           #_[:db/add town-eid :labour/price (max (quot labour-price-float' 100) 1)]
                           [:db/add town-eid :labour/supply 0]
                           [:db/add town-eid :labour/demand 0]
                           [:db/add town-eid :labour/last-supply labour-supply]
                           [:db/add town-eid :labour/last-demand labour-demand]

                           ;#_#_#_#_#_#_
                           [:db/add town-eid :food/price-float (max food-price-float' 1)]
                           [:db/add town-eid :food/price (max (long food-price-float') 1)]
                           [:db/add town-eid :clothes/price-float (max clothes-price-float' 1)]
                           [:db/add town-eid :clothes/price (max (long clothes-price-float') 1)]
                           [:db/add town-eid :labour/price-float (max labour-price-float' 1)]
                           [:db/add town-eid :labour/price (max (long labour-price-float') 1)]]))]

    {:when '[[?e :food/price _]]
     :then '[[:db.fn/call update-prices ?e]]
     :call {'update-prices update-prices}}))

(def decision-rules
  [hunt-rule
   try-eat-rule
   grow-food-rule

   create-settlement-rule
   peep-shop-rule
   peep-consume-rule
   craft-rule
   update-prices-rule])

(def reaction-rules
  [remove-starving-rule])

(def map-ui-view
  (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black cell tick lrtb map-font emoji-font]} ctx
                   {:keys [world-db map-view]} @*world]
    (let [[left right top bottom] lrtb
          terrain-tint (condp = map-view
                         :temp-view (fn [tile] (colour (colour-noise (get tile :temp)) 0 0))
                         :elev-view (fn [tile] (colour 0 (colour-noise (get tile :elev)) 0))
                         :climate-view (fn [tile] (colour (colour-noise (get tile :temp)) (colour-noise (get tile :elev)) 0))
                         :forage-view (fn [tile] (colour 0 (* (get tile :food 0) 40) 0))
                         :mine-view (fn [tile] (colour 0 (* (get tile :rock 0) 40) 0))
                         (fn [tile] (render-tile-colour (get tile :biome))))
          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            coord-x x
                            coord-y y
                            coord [coord-x coord-y]

                            tile (coord-q world-db coord)

                            things (when tile (units-q world-db coord))
                            size (count things)
                            {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))

                            {name :settlement/name :as settlement} (when tile (first (settlements-q world-db coord)))]
                        (when thing
                          (println :wealth thing (get thing :wealth))
                          (println :wealth settlement thing (get thing :wealth)))
                        (cond
                          thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                          (seq settlement) [name (colour 0 0 155) map-font font-offset-x font-offset-y]
                          :else ["" (terrain-tint tile) map-font font-offset-x font-offset-y])))]
      (ui/column
        (interpose (ui/gap 0 0)
          (for [y-idx (range top bottom)]
            (ui/row
              (interpose (ui/gap 0 0)
                (for [x-idx (range left right)]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [[glyph tile-colour font] (unit-data x-idx y-idx)]
                        (ui/fill (if hovered?
                                   (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                   (paint/fill tile-colour))
                          (ui/width cell
                            (ui/halign 0.5
                              (ui/height cell
                                (ui/valign 0.5
                                  (ui/label glyph {:font font :paint fill-white}))))))))))))))))))

(def chart-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-black fill-green fill-yellow fill-dark-gray]} ctx
                   {:keys [world-db]} @*world]
    (let [units (units-q world-db nil)
          wealth-freqs (->> units
                         (map :wealth)
                         (frequencies)
                         (into (sorted-map))
                         (vec))
          size (count wealth-freqs)
          mx 30
          wealth-freqs-filtered (into [["$" "#"]]
                                  (if (< mx size)
                                    (into (subvec wealth-freqs 0 (- mx 3)) cat [[["" ""] ["..." (str (- size (dec mx)) "+")] ["" ""]] (subvec wealth-freqs (- size 2) size)])
                                    wealth-freqs))
          freq-avg-fn (fn [freq]
                        (let [[total size] (reduce (fn [v [val count]] (-> v (update 0 + (* val count)) (update 1 + count))) [0 0] freq)]
                          (float (/ total size))))
          padding 4]
      (ui/padding 10
        (if-not (seq units)
          (ui/gap 0 0)
          (ui/column
            (ui/row
              (ui/label (str "# " (reduce + (mapv second wealth-freqs))) {:font font-small :paint fill-black})
              (ui/gap padding 2)
              (ui/label (str "μ 👁: " (freq-avg-fn (frequencies (map :vision units)))) {:font font-small :paint fill-black})
              (ui/gap padding 2)
              (ui/label (str "μ 🍖: " (freq-avg-fn (frequencies (map :hunger units)))) {:font font-small :paint fill-black})
              (ui/gap 2 padding))
            (ui/padding 5
              (ui/column
                (interpose (ui/gap padding 2)
                  (for [entry wealth-freqs-filtered
                        :let [[_wealth quantity] entry]]
                    (ui/row
                      (interpose (ui/gap padding 0)
                        (into
                          (if (number? quantity)
                            (custom-ui/<>
                              (ui/fill (paint/fill (colour 150 150 150))
                                (ui/gap (* quantity 2) 2)))
                            (custom-ui/<>
                              (ui/gap 0 0)))
                          (for [val entry]
                            (ui/width 20
                              (ui/label (str val) {:font font-small :paint fill-black}))))))))))))))))

(defn city-view [settlement]
  (ui/dynamic ctx [{:keys [font-small fill-black]} ctx
                   {:keys [world-db map-view]} @*world]
    (ui/column
      (ui/column
        (interpose (ui/gap 4 0)
          (for [peep (mapv d/touch (lookup-avet world-db :hometown (:db/id settlement)))]
            (ui/label (str (select-keys peep [:money :health :food :clothes :inventory :planning])) {:font font-small :paint fill-black}))))
      (ui/row
        (interpose (ui/gap 4 0)
          (for [k [:settlement/name :settlement/place]]
            (ui/label (str (get settlement k)) {:font font-small :paint fill-black}))))
      (ui/gap 0 4)
      (ui/row
        (ui/column
          (interpose (ui/gap 0 4)
            (for [columns [["" "Food" "Clothes" "Labour"] ["Price" :food/price :clothes/price :labour/price] ["Float" :food/price-float :clothes/price-float :labour/price-float] ["Velocity" :food/price-velocity :clothes/price-velocity :labour/price-velocity] ["Demand" :food/demand :clothes/demand :labour/demand] ["Supply" :food/supply :clothes/supply :labour/supply]]]
              (ui/row
                (interpose (ui/gap 4 0)
                  (for [k columns
                        :let [label (if (string? k) k (str (get settlement k)))]]
                    (ui/padding 0 0 40 0
                      (ui/label label {:font font-small :paint fill-black}))))))))))))

(defn tick-world [*world]
  (as-> *world $
    (update $ :world-db apply-rules decision-rules reaction-rules)
    (update $ :dbs (fnil track-db []) (:world-db $))
    (update $ :stats (fnil add-stats []) (:world-db $))))

(defn do-tick-world []
  (swap! *world tick-world))

(defn tick-world-10x []
  (dotimes [_ 10]
    (do-tick-world)))


(def ui-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-green fill-yellow fill-dark-gray]} ctx
                   {:keys [world-db map-view]} @*world]
    (ui/column
      (ui/row
        map-ui-view
        (ui/height 200
          (ui/column
            chart-view
            (ui/vscrollbar
              (ui/vscroll
                (ui/column
                  (interpose (ui/gap 4 0)
                    (for [settlement (settlements-q world-db nil)]
                      (ui/padding 4
                        (city-view settlement))))))))))
      (ui/row
        (ui/clickable
          #(reset! *world (reset-world))
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-yellow fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "RESET!" {:font font-small :paint fill-white}))))))
        (ui/clickable
          do-tick-world
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-yellow fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "+ 1 Day" {:font font-small :paint fill-white}))))))
        (ui/clickable
          tick-world-10x
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-yellow fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "+ 10 Day" {:font font-small :paint fill-white})))))))
      (ui/row
        (ui/clickable
          #(swap! *world assoc :map-view :default-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :default-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🗺️" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! *world assoc :map-view :temp-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :temp-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌡" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! *world assoc :map-view :elev-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :elev-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "📏" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! *world assoc :map-view :climate-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :climate-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌍" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! *world assoc :map-view :forage-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :forage-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🚜" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! *world assoc :map-view :mine-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :mine-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "⛏️" {:font font-small :paint fill-white}))))))))))

