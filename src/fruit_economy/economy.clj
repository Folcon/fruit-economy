(ns fruit-economy.economy
  (:require [ubergraph.core :as uber]
            [fruit-economy.land :as land]
            [fruit-economy.graph :as graph]))


(defn dec-or-dissoc [m k]
  (let [v (dec (get m k))]
    (if (zero? v)
      (dissoc m k)
      (assoc m k v))))

(defn make-civ-node [{:fruit-economy.civ/keys [name tint] :as civ} sources]
  {:id (str name) :color "blue"
   :label (into [:TABLE {:BORDER 0}
                 [:TR [:TD {:BORDER 1} (str name)]]]
            (map (fn [[k v]] [:TR [:TD (:name k)] [:TD {:BORDER 1} v]]))
            sources)})

(defn assign-source->civ [{::land/keys [economy area->civ-name area->units] :as land-data} {:fruit-economy.civ/keys [name] :as _civ} area]
  (let [existing-owner (area->civ-name area)
        unit (area->units area)]
    (cond-> (land/log-history land-data (str (when existing-owner (str existing-owner " relinquishes claim on and ")) name " lays claim to a " (:name unit) " at " area))
      ;; remove from existing owner
      existing-owner
      (update-in [::land/economy :civ-name->source existing-owner] dec-or-dissoc unit)

      ;; add to new owner
      :always
      (update-in [::land/economy :civ-name->source name unit] (fnil inc 0)))))

(defn init-civ-economy [{::land/keys [area->units] :as land-data} {:fruit-economy.civ/keys [name] :as civ}]
  (let [;; stop recording sources on the node directly
        civ-node (make-civ-node civ [])]
    (-> land-data
      ;; Add the civ node to the graph
      (update-in [::land/economy :ubergraph] graph/add-node-with-attrs [name civ-node])

      ;; Mark down new claims, should really use assign-source->civ,
      ;;   but it doesn't work with the graph yet
      (as-> $
        (reduce
          (fn [land area]
            (if-let [unit (area->units area)]
              (-> (land/log-history land (str name " lays claim to a " (:name unit) " at " area))
                (update-in [::land/economy :civ-name->source name unit] (fnil inc 0))
                (update-in [::land/economy :ubergraph] graph/add-directed-edge name (:name unit) {:label (str "at " area)}))
              land))
          $
          (:fruit-economy.civ/territory civ))))))

(defn add-resources [{::land/keys [area->units] :as land-data}]
  (reduce
    (fn [land {:keys [name kind] :as resource}]
      (let [resource-node {:id name :ref resource
                           :kind (->> (get land/kind->category kind)
                                   clojure.core/name
                                   (str "exists-")
                                   keyword)}]
        (-> land
          (update-in [::land/economy :ubergraph] graph/add-node-with-attrs [name resource-node]))))
    land-data
    (distinct (vals area->units))))

(comment
  (require '[fruit-economy.civ :as civ])
  (do
    (swap! fruit-economy.core/*state assoc :world w :cell 200)
    nil)

  (def w
    (let [width 3 height 4
          world (-> (land/populate (land/gen-land (land/make-land "World" width height)) 100)
                  (civ/try-spawn-new-civs 10))]
      world))

  (let [land-data w
        {::land/keys [economy civ-name->civ area->units area->civ-name] :as land-data} land-data
        civ-name "Civ )+3"
        civ  (civ-name->civ civ-name)
        area [1 0]
        existing-owner (area->civ-name area)
        unit (area->units area)]
    (cond-> land-data
      ;; remove from existing owner
      existing-owner
      (update-in [::land/economy :civ-name->source existing-owner] dec-or-dissoc unit)

      ;; add to new owner
      :always
      (update-in [::land/economy :civ-name->source civ-name unit] (fnil inc 0))

      :always
      ::land/economy)
    ;nil
    #_(-> (reduce
            (fn [land area]
              (let [unit (area->units area)]
                (println area unit)
                (-> land
                  (update-in [::land/economy :civ-name->source civ-name unit] (fnil inc 0))
                  #_(update-in [::land/economy :area->source area] unit))))
            land-data
            (:fruit-economy.civ/territory civ))
        ::land/economy))

  (let [world #_w (:world @fruit-economy.core/*state)]
    ((juxt
       ::land/area->units
       (comp
         #(map (juxt :fruit-economy.civ/name :fruit-economy.civ/territory) %)
         vals ::land/civ-name->civ)
       ::land/economy)
     world))

  ,)

(defn ->svg [{:keys [ubergraph nodes edges] :as economy}]
  (cond-> economy

    ;; contains nodes or edges. so make ubergraph
    (or nodes edges)
    (graph/make)

    ;; contains ubergraph, so grab it
    :ubergraph
    (get :ubergraph)

    :always
    (graph/->svg)))

;; categories of resources
;; :bush :tree :flower :herb :shroom :nut :fruit
(def processes [:chop :boil :magic :heat :slice])

(defn apply-rules [graph rules]
  (let [rules' (shuffle rules)]
    (reduce
      (fn [g {:keys [match alter] :as _rule}]
        (let [result (reduce
                       (fn [v match-node]
                         (if-let [matches (graph/find-nodes g match-node)]
                           (conj v matches)
                           (reduced [])))
                       []
                       match)]
          (if (every? seq result)
            (apply alter g result)
            g)))
      graph
      rules')))

(comment
  (let [;; Extending the matching rules?
        ;;   https://www.metosin.fi/blog/malli-regex-schemas/
        rules [{:match [[{:kind :source}]
                        [{:kind :exists-fruit}]]
                :alter (fn [graph sources exists-fruits]
                         (let [source (rand-nth sources)
                               exists-fruit (rand-nth exists-fruits)]
                           (-> graph
                             (assoc-in [:attrs (:id exists-fruit) :kind] :unused-fruit)
                             (graph/add-directed-edge (:id source) (:id exists-fruit)))))}
               {:match [[{:kind :unused-fruit}]
                        [{:kind :exists-fruit-proc}]]
                :alter (fn [graph unused-fruits exists-fruit-procs]
                         (let [unused-fruit (rand-nth unused-fruits)
                               exists-fruit-proc (rand-nth exists-fruit-procs)
                               process (rand-nth processes)
                               process-node {:id (keyword (str (name (:id unused-fruit)) "-process")) :kind :process :tag process}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-fruit) :kind] :fruit)
                             (assoc-in [:attrs (:id exists-fruit-proc) :kind] :fruit-proc)
                             (graph/add-node-with-attrs [(:id process-node) process-node])
                             (graph/add-directed-edge (:id unused-fruit) (:id process-node))
                             (graph/add-directed-edge (:id process-node) (:id exists-fruit-proc)))))}
               {:match [[{:kind :unused-fruit}]
                        [{:kind :exists-fruit-proc}]
                        [{:kind :exists-fruit-good}]]
                :alter (fn [graph unused-fruits exists-fruit-procs exists-fruit-goods]
                         (let [unused-fruit (rand-nth unused-fruits)
                               exists-fruit-proc (rand-nth exists-fruit-procs)
                               exists-fruit-good (rand-nth exists-fruit-goods)
                               process (rand-nth processes)
                               process-node {:id (keyword (str (name (:id unused-fruit)) "-process")) :kind :process :tag process}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-fruit) :kind] :fruit)
                             (assoc-in [:attrs (:id exists-fruit-proc) :kind] :fruit-proc)
                             (assoc-in [:attrs (:id exists-fruit-good) :kind] :fruit-good)
                             (graph/add-node-with-attrs [(:id process-node) process-node])
                             (graph/add-directed-edge (:id unused-fruit) (:id process-node))
                             (graph/add-directed-edge (:id process-node) (:id exists-fruit-proc) {:label :=})
                             (graph/add-directed-edge (:id process-node) (:id exists-fruit-good) {:label :=}))))}]
        ;; still need to figure out how to populate this with goods,
        ;;   processes can come from some randomness and process,
        ;;   maybe just do the same with goods for now?
        g {:nodes [{:id :source :kind :source}
                   {:id :sink :kind :sink}
                   {:id "Civ R+23" :kind :civ}
                   {:id :apple :kind :exists-fruit}
                   {:id :banana :kind :exists-fruit}
                   {:id :cream :kind :exists-fruit-good}
                   {:id :chop :kind :exists-fruit-proc}],
           :edges [[:source :sink]]}
        g (graph/make g)]
    (nth (iterate #(apply-rules % rules) g) 3)))
