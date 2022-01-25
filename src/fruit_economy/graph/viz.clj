(ns fruit-economy.graph.viz
  (:require [fruit-economy.graph.ubergraph :as graph]
            [hiccup.core :refer [html]])
  (:import [guru.nidi.graphviz.model Factory Link MutableNode MutableGraph]
           [guru.nidi.graphviz.engine Format Graphviz Engine]
           [guru.nidi.graphviz.attribute Color Label]))


(defn ubergraph->svg [ubergraph]
  (let [g (-> (Factory/mutGraph)
            (.setDirected true))
        data {:graph g :nodes {} :edges {}}]
    (as-> data $
      (reduce
        (fn [g {:keys [id color label] :as node}]
          (let [node-id (name id)
                new-node (cond-> (Factory/mutNode ^String node-id)
                           color
                           (.add (Color/named (name color)))
                           label
                           (.add (Label/html (html label))))]
            (-> g
              (update :nodes assoc node-id new-node)
              (update :graph #(.add % [new-node])))))
        $
        (graph/nodes ubergraph))

      (reduce
        (fn [{:keys [nodes] :as g} {:keys [id src dest] :as edge}]
          (let [edge-id (str id)
                src-id (name src)
                dest-id (name dest)
                src-node (get nodes src-id)
                dest-node (get nodes dest-id)
                _ (.addLink ^MutableNode src-node [(Link/to dest-node)])]
            g))
        $
        (graph/edges ubergraph))

      (-> (Graphviz/fromGraph ^MutableGraph (:graph $))
        (.engine Engine/FDP)
        (.width 1200)
        (.height 800)
        (.render Format/SVG)
        (.toString)))))

