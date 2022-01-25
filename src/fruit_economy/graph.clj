(ns fruit-economy.graph
  (:require [ubergraph.core :as uber]
            [com.rpl.specter :refer [select-any keypath]]
            [fruit-economy.graph.ubergraph :as uber-wrapper]
            [fruit-economy.graph.viz :refer [ubergraph->svg]]))


(defn graph? [g] (contains? g :ubergraph))

(def node-ids uber-wrapper/node-ids)

(defn id->node [g id] (select-any (keypath :attrs id) g))

(def nodes uber-wrapper/nodes)

(def edges uber-wrapper/edges)

(def submap?
  "[m1 m2]

   Checks m1 is submap of m2"
  #'uber/submap?)

(def add-node-with-attrs
  "[g [node attr-map]]

   Adds node to g with a given attribute map. Takes a [node attribute-map] pair."
  #'uber-wrapper/add-node-with-attrs)

(def add-directed-edge
  "[g src dest]
   [g src dest attributes]"
  #'uber-wrapper/add-directed-edge)

(def remove-node
  "[g node]"
  #'uber/remove-node)

(def add-attrs
  "[g node-or-edge attribute-map]
   [g n1 n2 attribute-map]

   Merges an attribute map with the existing attributes of a node or edge"
  uber/add-attrs)

(def set-attrs
  "[g node-or-edge attribute-map]
   [g n1 n2 attribute-map]

   Sets the attribute map of a node or edge, overwriting existing attribute map"
  uber/set-attrs)

(def remove-attrs
  "[g node-or-edge attributes]
   [g n1 n2 attributes]

   Removes the attributes from the node or edge"
  uber/remove-attrs)

(def make
  "[{:keys [nodes edges]}]

   creates an ubergraph"
  uber-wrapper/make)

#_
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
                       :layout :fdp})))
(defn ->svg
  "generates an svg string from an ubergraph"
  [g]
  (ubergraph->svg g))

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

(defn update-labels [g {:keys [nodes edges]}]
  (as-> g $
    (reduce
      (fn [g {:keys [id label-fn] :as _n}]
        (let [node (id->node g id)]
          (add-attrs g id {:label (label-fn node)})))
      $
      nodes)
    (reduce
      (fn [g {:keys [src dest label-fn] :as _e}]
        (let [edge (find-edges g src dest)]
          (add-attrs g src dest {:label (label-fn edge)})))
      $
      edges)))
