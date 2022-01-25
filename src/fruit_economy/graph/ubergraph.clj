(ns fruit-economy.graph.ubergraph
  (:require [ubergraph.core :as uber]
            [com.rpl.specter :refer [select-any keypath]]
            [fruit-economy.graph.viz :refer [ubergraph->svg]]))

(defn node-ids [g] (vec (#'uber/nodes g)))

(defn nodes [g] (into [] (map (fn [node-id] (select-any (keypath :attrs node-id) g))) (node-ids g)))

(defn edges [g] (vec (#'uber/edges g)))

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
