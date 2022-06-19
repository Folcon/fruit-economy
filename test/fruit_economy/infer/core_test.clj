(ns fruit-economy.infer.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [datascript.core :as ds]
            [fruit-economy.infer.core :as infer]))


;;; Lookup FNS
(defn lookup-avet
  ([world-db attr value] (lookup-avet world-db attr value nil))
  ([world-db attr value attrs]
   (reduce
     (fn [v datom]
       (let [eid (first datom)]
         (conj v
           (cond-> (ds/entity world-db eid)
             attrs
             (select-keys attrs)))))
     []
     (ds/datoms world-db :avet attr value))))

;;; Comparison FNS
(defn db->datoms
  ([db] (db->datoms db []))
  ([db coll]
   (into coll (map (juxt (constantly :db/add) :e :a :v)) (ds/datoms db :eavt))))

;;; Basic Test Rules
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

(deftest infer-and-infer-posh-equal-test
  (testing "Testing that using posh doesn't break anything"
    (let [schema {:place {:db/valueType :db.type/ref}
                  :coord {:db/unique :db.unique/identity}}
          world-data [{:init-food 2 :food 2 :coord [0 0]} {:init-food 3 :food 1 :coord [1 0]} {:init-food 2 :food 1 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 1, :place [:coord [0 0]]}]
          db (ds/db-with (ds/empty-db schema)
               world-data)
          conn (ds/create-conn schema)
          _ (infer/posh-init! conn)
          _ (infer/posh-transact! conn world-data)
          rules [hunt-rule try-eat-rule remove-starving-rule grow-food-rule]
          limit 10]
      (loop [db db
             n 0]
        (println n)
        (let [db' (infer/infer db rules)
              _ (infer/infer-posh conn rules)
              ds-datoms (db->datoms db' #{})
              posh-datoms (db->datoms @conn #{})]
          (cond
            (<= limit n)
            db

            (not= ds-datoms posh-datoms)
            [(set/difference ds-datoms posh-datoms)
             (set/difference posh-datoms ds-datoms)]

            :else
            (recur db' (inc n))))))))
