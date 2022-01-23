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
  (let []
    (reduce
      (fn [land area]
        (if-let [unit (area->units area)]
          (-> (land/log-history land (str name " lays claim to a " (:name unit) " at " area))
            (update-in [::land/economy :civ-name->source name unit] (fnil inc 0))
            (as-> $ (update-in $ [::land/economy :nodes] conj (make-civ-node civ (get-in $ [::land/economy :civ-name->source name]))))
            #_(update-in [::land/economy :area->source area] unit))
          land))
      land-data
      (:fruit-economy.civ/territory civ))))

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

(defn ->svg [{:keys [nodes edges] :as economy}]
  (-> (graph/make economy)
    (graph/->svg)))
