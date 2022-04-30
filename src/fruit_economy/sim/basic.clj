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

(defn gen-bug-world [size n-peeps]
  (into
    (vec
      (for [x (range size)
            y (range size)]
        (let [food #_(inc (rand-int 4)) (- 5 (int (Math/sqrt (rand-int 32))))]
          {:init-food food :food food :coord [x y]})))
    (coord+entity->entities-coll
      (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)])))))


(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(defn reset-world []
  (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                          :coord {:db/unique :db.unique/identity}})
    (gen-bug-world bug-world-size bug-count)))

(def *world (atom {:world-db (reset-world)}))

(defn coord-q [db coord]
  (-> (lookup-avet db :coord coord)
    first
    d/touch))

(defn units-q
  "coord can be nil or [x y]"
  [db coord]
  (->> (when coord
         [:coord coord])
    (lookup-avet db :place)
    (mapv :db/id)
    (d/pull-many db '[* {:place [:coord]}])
    (mapv #(update % :place :coord))))


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
  (-> '{:when [[?e :place ?ce]
               [?e :vision ?vision]
               [?ce :coord ?coord]
               [?ce :food ?food]
               [(hunt $ ?coord ?vision) ?target]
               [(not= ?target ?coord)]
               [?te :coord ?target]]
        :then [[:db/add ?e :place ?te]]}
    (merge {:args {'hunt hunt}})))

(defn gathered [food] (min food (+ (quot food 2) 2)))

(def try-eat-rule
  (-> '{:when [[?e :place ?ce]
               [?e :wealth ?wealth]
               [?e :hunger ?hunger]
               [?ce :coord ?coord]
               [?ce :food ?food]
               [(gathered ?food) ?gather]
               [(- ?food ?gather) ?rem-food]
               [(+ ?wealth ?gather) ?gain-wealth]
               [(- ?gain-wealth ?hunger) ?rem-wealth]]
        :then [[:db/add ?e :wealth ?rem-wealth]
               [:db/add ?ce :food ?rem-food]]}
    (merge {:args {'gathered gathered}})))

(def remove-starving-rule
  '{:when [[?e :wealth ?wealth]
           [(< ?wealth 0)]]
    :then [[:db/retractEntity ?e]]})

(defn grow-food [food init-food] (min (inc food) init-food))

(def grow-food-rule
  (-> '{:when [[?e :food ?food]
               [?e :init-food ?init-food]
               [(< ?food ?init-food)]
               [(grow-food ?food ?init-food) ?gain-food]]
        :then [[:db/add ?e :food ?gain-food]]}
    (merge {:args {'grow-food grow-food}})))

(def rules
  [hunt-rule
   try-eat-rule
   remove-starving-rule
   grow-food-rule])

(def map-ui-view
  (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black cell tick lrtb map-font emoji-font]} ctx
                   {:keys [world-db]} @*world]
    (let [[left right top bottom] lrtb
          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            coord-x x
                            coord-y y
                            coord [coord-x coord-y]

                            tile (coord-q world-db coord)

                            things (units-q world-db coord)
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
          #(swap! *world update :world-db infer rules 1)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-green fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "Apply Rules" font-small fill-white))))))))))
