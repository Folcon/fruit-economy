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
   :label (into [:table {:border 0}
                 [:tr [:td {:border 1} (str name)]]]
            (map (fn [[k v]] [:tr [:td (:name k)] [:td {:border 1} v]]))
            sources)})

(defn assign-source->civ [{::land/keys [economy area->civ-name area->resources] :as land-data} {:fruit-economy.civ/keys [name] :as _civ} area]
  (let [existing-owner (area->civ-name area)
        resource (area->resources area)]
    (cond-> (land/log-history land-data (str (when existing-owner (str existing-owner " relinquishes claim on and ")) name " lays claim to a " (:name resource) " at " area))
      ;; remove from existing owner
      existing-owner
      (update-in [::land/economy :civ-name->source existing-owner] dec-or-dissoc resource)

      ;; add to new owner
      :always
      (update-in [::land/economy :civ-name->source name resource] (fnil inc 0)))))

(defn init-civ-economy [{::land/keys [area->resources] :as land-data} {:fruit-economy.civ/keys [name] :as civ}]
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
            (if-let [resource (area->resources area)]
              (-> (land/log-history land (str name " lays claim to a " (:name resource) " at " area))
                (update-in [::land/economy :civ-name->source name resource] (fnil inc 0))
                (update-in [::land/economy :ubergraph] graph/add-directed-edge name (:name resource) {:label (str "at " area)}))
              land))
          $
          (:fruit-economy.civ/territory civ))))))

(defn add-resources [{::land/keys [area->resources] :as land-data}]
  (reduce
    (fn [land {:keys [name kind] :as resource}]
      (let [node-kind (->> (get land/kind->category kind)
                        clojure.core/name
                        (str "exists-")
                        keyword)
            resource-node {:id name :ref resource
                           :kind node-kind
                           :label (str name "\n" node-kind)}]
        (-> land
          (update-in [::land/economy :ubergraph] graph/add-node-with-attrs [name resource-node]))))
    land-data
    (distinct (vals area->resources))))

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
        {::land/keys [economy civ-name->civ area->resources area->civ-name] :as land-data} land-data
        civ-name "Civ )+3"
        civ  (civ-name->civ civ-name)
        area [1 0]
        existing-owner (area->civ-name area)
        resource (area->resources area)]
    (cond-> land-data
      ;; remove from existing owner
      existing-owner
      (update-in [::land/economy :civ-name->source existing-owner] dec-or-dissoc resource)

      ;; add to new owner
      :always
      (update-in [::land/economy :civ-name->source civ-name resource] (fnil inc 0))

      :always
      ::land/economy)
    ;nil
    #_(-> (reduce
            (fn [land area]
              (let [resource (area->resources area)]
                (println area resource)
                (-> land
                  (update-in [::land/economy :civ-name->source civ-name resource] (fnil inc 0))
                  #_(update-in [::land/economy :area->source area] resource))))
            land-data
            (:fruit-economy.civ/territory civ))
        ::land/economy))

  (let [world #_w (:world @fruit-economy.core/*state)]
    ((juxt
       ::land/area->resources
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

(defn id+kind+tag->label [{:keys [id kind tag] :as _node}] (str id "<br />" kind (when tag (str " " tag))))

(defn generate-rules []
  (let [categories [:bush :tree :flower :herb :shroom :nut :fruit]]
    (into []
      (comp
        (map
          (fn [category-kw]
            (let [category-str (name category-kw)
                  exists-kw (keyword (str "exists-" category-str))
                  unused-kw (keyword (str "unused-" category-str))
                  exists-process-kw (keyword (str "exists-" category-str "-process"))
                  exists-processed-kw (keyword (str "exists-" category-str "-processed"))
                  processed-kw (keyword (str category-str "-processed"))
                  exists-good-kw (keyword (str "exists-" category-str "-good"))
                  good-kw (keyword (str category-str "-good"))]
              [;; innovation-token =>
               ;;   (exists-category-process|exists-category-good)
               {:match [[{:kind :innovation-token}]]
                :alter (fn [graph innovation-tokens]
                         (let [innovation-token (first innovation-tokens)
                               token-id (re-find #"[0-9]+" (:id innovation-token))
                               category-kw (rand-nth categories)
                               category-str (name category-kw)
                               innovation (rand-nth ["process" "good"])
                               innovation-str (str category-str "-" innovation)
                               exists-innovation-str (str "exists-" innovation-str)
                               exists-innovation-kw (keyword exists-innovation-str)
                               innovation-node-id (str innovation-str "-" token-id)]
                           (-> graph
                             ;; remove the old token, we might want to keep it in the future if they're created by civ's
                             ;;   and are linked to them, we should probably use numbers as id's so that we don't need to rename nodes
                             (graph/remove-node (:id innovation-token))
                             (graph/add-node-with-attrs [innovation-node-id {:id innovation-node-id :kind exists-innovation-kw :color "purple"}])
                             (graph/update-labels {:nodes [{:id innovation-node-id :label-fn id+kind+tag->label}]}))))}

               ;; source + exists-category =>
               ;;   unused-category +
               ;;   [source unused-category]
               {:match [[{:kind :source}]
                        [{:kind exists-kw}]]
                :alter (fn [graph sources exists-categories]
                         (let [source (rand-nth sources)
                               exists-category (rand-nth exists-categories)]
                           (-> graph
                             (assoc-in [:attrs (:id exists-category) :kind] unused-kw)
                             (graph/add-directed-edge (:id source) (:id exists-category))
                             (graph/update-labels {:nodes [{:id (:id exists-category) :label-fn id+kind+tag->label}]}))))}

               ;; unused-category + exists-category-process =>
               ;;   category + category-processed + processor +
               ;;   [category processor] [processor category-processed]
               {:match [[{:kind unused-kw}]
                        [{:kind exists-process-kw}]]
                :alter (fn [graph unused-categories exists-category-processes]
                         (let [unused-category (rand-nth unused-categories)
                               exists-category-process (rand-nth exists-category-processes)
                               processor (rand-nth processes)
                               processor-node {:id (keyword (str (name (:id unused-category)) "-processor")) :kind :process :tag processor}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-category) :kind] category-kw)
                             (assoc-in [:attrs (:id exists-category-process) :kind] processed-kw)
                             (graph/add-node-with-attrs [(:id processor-node) processor-node])
                             (graph/add-directed-edge (:id unused-category) (:id processor-node))
                             (graph/add-directed-edge (:id processor-node) (:id exists-category-process))
                             (graph/update-labels {:nodes [{:id (:id unused-category) :label-fn id+kind+tag->label}
                                                           {:id (:id exists-category-process) :label-fn id+kind+tag->label}
                                                           {:id (:id processor-node) :label-fn id+kind+tag->label}]}))))}

               ;; unused-category + exists-category-process + exists-category-good =>
               ;;   category + category-good + category-processed + processor +
               ;;   [category processor] [processor category-good] [processor category-processed]
               {:match [[{:kind unused-kw}]
                        [{:kind exists-processed-kw}]
                        [{:kind exists-good-kw}]]
                :alter (fn [graph unused-categories exists-category-processeds exists-category-goods]
                         (let [unused-category (rand-nth unused-categories)
                               exists-category-processed (rand-nth exists-category-processeds)
                               exists-category-good (rand-nth exists-category-goods)
                               processor (rand-nth processes)
                               processor-node {:id (keyword (str (name (:id unused-category)) "-process")) :kind :process :tag processor}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-category) :kind] category-kw)
                             (assoc-in [:attrs (:id exists-category-processed) :kind] processed-kw)
                             (assoc-in [:attrs (:id exists-category-good) :kind] good-kw)
                             (graph/add-node-with-attrs [(:id processor-node) processor-node])
                             (graph/add-directed-edge (:id unused-category) (:id processor-node))
                             (graph/add-directed-edge (:id processor-node) (:id exists-category-processed) {:label :=})
                             (graph/add-directed-edge (:id processor-node) (:id exists-category-good) {:label :=})
                             (graph/update-labels {:nodes [{:id (:id unused-category) :label-fn id+kind+tag->label}
                                                           {:id (:id exists-category-processed) :label-fn id+kind+tag->label}
                                                           {:id (:id exists-category-good) :label-fn id+kind+tag->label}
                                                           {:id (:id processor-node) :label-fn id+kind+tag->label}]}))))}])))
        cat)
      categories)))

(defn step-economy [economy-data]
  (let [rules (generate-rules)]
    (update economy-data :ubergraph apply-rules rules)))

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
