(ns fruit-economy.sim.basic
  (:require [clojure.walk :as walk]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [datascript.core :as d]
            [fruit-economy.colour :refer [colour]]
            [fruit-economy.data.core :refer [lookup-avet]])
  (:import [io.github.humbleui.skija Paint]))


(defn rewrite
  "Tries to match rule against db
  Returns tx-data or empty list if rule does not match"
  [{:keys [when then args]} db]
  (let [syms (for [row then el row :when (symbol? el)] el)
        results (apply d/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args))]
    (for [match results tx then]
      (let [swaps (zipmap syms match)
            f (fn [x] (if (coll? x) x (get swaps x x)))]
        (walk/postwalk f tx)))))

(defn infer
  "Returns a new db with all inferred facts and a list of tx-data.
  Stops when no more rules apply or after 100 iterations"
  [db rules]
  (loop [db db max-iter 100]
    (let [tx-data (for [rule rules tx (rewrite rule db)] tx)]
      (cond
        (empty? tx-data) db
        (zero? max-iter) db
        :else (recur (d/db-with db tx-data) (dec max-iter))))))


(defn loc+entity->entities-coll [loc+entity]
  (reduce
    (fn [m [loc entity]]
      (conj m (assoc entity :pos [:loc loc])))
    []
    loc+entity))

(defn make-bug []
  {:glyph "🐞" :wealth 0 :vision (inc (rand-int 4)) :hunger (inc (rand-int 4))})

(defn gen-bug-world [size n-peeps]
  (into
    (vec
      (for [x (range size)
            y (range size)]
        (let [food #_(inc (rand-int 4)) (- 5 (int (Math/sqrt (rand-int 32))))]
          {:init-food food :food food :max-age (+ (rand-int 5) 20) :loc [x y]})))
    (loc+entity->entities-coll
      (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)])))))


(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(defn reset-world []
  (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                          :loc {:db/unique :db.unique/identity}})
    (gen-bug-world bug-world-size bug-count)))

(def *world (atom {:world-db (reset-world)}))

(defn loc-q [db loc]
  (-> (lookup-avet db :loc loc)
    first
    d/touch))

(defn units-q
  "loc can be nil or [x y]"
  [db loc]
  (->> (when loc
         [:loc loc])
    (lookup-avet db :pos)
    (mapv :db/id)
    (d/pull-many db '[* {:pos [:loc]}])
    (mapv #(update % :pos :loc))))

(defn sees [loc vision]
  (let [[x y] loc]
    (into []
      cat
      [(for [x (range (- x vision) (inc (+ x vision)))
             :when (not= [x y] loc)]
         [x y])
       (for [y (range (- y vision) (inc (+ y vision)))
             :when (not= [x y] loc)]
         [x y])])))

(defn best-food [db loc sees]
  (let [get-food (fn [loc]
                   (-> (lookup-avet db :loc loc [:food])
                     (first)
                     (get :food 0)))]
    (reduce
      (fn [[loc food] target]
        (let [target-food (get-food target)]
          (if (> target-food food)
            [target target-food]
            [loc food])
          (if (> target-food food)
            [target target-food]
            [loc food])))
      [loc (get-food loc)]
      sees)))

(defn hunt [db loc vision]
  (let [[best-food-loc _food] (best-food db loc (sees loc vision))]
    best-food-loc))

(def hunt-rule
  (-> '{:when [[?e :pos ?le]
               [?e :vision ?vision]
               [?le :loc ?loc]
               [?le :food ?food]
               [(hunt $ ?loc ?vision) ?target]
               [(not= ?target ?loc)]
               [?te :loc ?target]]
        :then [[:db/add ?e :pos ?te]]}
    (merge {:args {'hunt hunt}})))

(defn gathered [food] (min food (+ (quot food 2) 2)))

(def try-eat-rule
  (-> '{:when [[?e :pos ?le]
               [?e :wealth ?wealth]
               [?e :hunger ?hunger]
               [?le :loc ?loc]
               [?le :food ?food]
               [(gathered ?food) ?gather]
               [(- ?food ?gather) ?rem-food]
               [(+ ?wealth ?gather) ?gain-wealth]
               [(- ?gain-wealth ?hunger) ?rem-wealth]]
        :then [[:db/add ?e :wealth ?rem-wealth]
               [:db/add ?le :food ?rem-food]]}
    (merge {:args {'gathered gathered}})))


(def rules
  [hunt-rule])

(def map-ui-view
  (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black cell tick lrtb map-font emoji-font]} ctx
                   {:keys [world-db]} @*world]
    (let [[left right top bottom] lrtb
          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x
                            loc-y y
                            loc [loc-x loc-y]

                            tile (loc-q world-db loc)

                            things (units-q world-db loc)
                            size (count things)
                            {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))]
                        (when thing
                          (println :wealth thing (get thing :wealth)))
                        (cond
                          thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                          :else ["" (colour 0 (* (get tile :food 0) 40) 0) map-font font-offset-x font-offset-y])))]
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
                                  (ui/label glyph font fill-white))))))))))))))))))

(def ui-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-green fill-dark-gray]} ctx]
    (ui/column
      map-ui-view
      (ui/row
        (ui/clickable
          #(reset! *world {:world-db (reset-world)})
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-green fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "RESET!" font-small fill-white))))))
        (ui/clickable
          #(swap! *world update :world-db infer rules)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-green fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "Apply Rules" font-small fill-white))))))))))
