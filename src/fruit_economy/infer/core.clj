(ns fruit-economy.infer.core
  (:require [clojure.walk :as walk]
            [datascript.core :as d]))


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
