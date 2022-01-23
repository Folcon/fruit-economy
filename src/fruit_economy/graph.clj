(ns fruit-economy.graph
  (:require [ubergraph.core :as uber]
            [com.rpl.specter :refer [select-any keypath]]))


(defn node-ids [g] (vec (#'uber/nodes g)))

(defn nodes [g] (into [] (map (fn [node-id] (select-any (keypath :attrs node-id) g))) (node-ids g)))

(defn edges [g] (vec (#'uber/edges g)))

(def submap?
  "[m1 m2]

   Checks m1 is submap of m2"
  #'uber/submap?)

(def add-node-with-attrs
  "[g [node attr-map]]

   Adds node to g with a given attribute map. Takes a [node attribute-map] pair."
  #'uber/add-node-with-attrs)

(defn add-directed-edge
  ([g src dest] (add-directed-edge g src dest nil))
  ([g src dest attributes]
   (#'uber/add-directed-edge g src dest attributes)))

(defn make
  "creates an ubergraph"
  [{:keys [nodes edges]}]
  (-> (uber/ubergraph true false)
    (as-> $
      (reduce
        (fn [graph {:keys [id] :as node}]
          (add-node-with-attrs graph [id node]))
        $
        nodes)
      (reduce
        (fn [graph [to from attrs]]
          (add-directed-edge graph to from attrs))
        $
        edges))))

(defn ->svg
  "generates an svg string from an ubergraph"
  [g]
  (with-redefs [dorothy.core/save! (fn [graph _f & [_options]]
                                     (dorothy.core/render graph {:format :svg}))]
    (uber/viz-graph g {:save {:format :svg}
                       ;; available :layout opts
                       #_[:dot :neato :twopi :circo :fdp :osage :patchwork :sfdp]
                       ;; by preference
                       #_[:circo :sfdp :osage :twopi :neato :fdp]
                       :layout :circo})))

(def find-edges
  "[g src dest] [g query]

   Returns all edges that match the query"
  #'uber/find-edges)

(defn find-nodes
  "Returns all nodes that match the query
   query can take the form of {:a :b}
   or several query values [{:a :b} {:a :c}]"
  [g attrs-coll]
  (let [attrs-coll (if (map? attrs-coll) [attrs-coll] attrs-coll)]
    (reduce
      (fn [v node-id]
        ;; it seems faster to use node-ids and specter than just
        ;;   use the nodes fn
        (let [node (select-any (keypath :attrs node-id) g)]
          (if (some #(submap? % node) attrs-coll)
            (conj v node)
            v)))
      []
      (node-ids g))))
