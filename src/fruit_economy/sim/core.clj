(ns fruit-economy.sim.core
  (:require [fruit-economy.land :as land]
            [fruit-economy.language :as lang]
            [fruit-economy.sim.coords :as coords]
            [fruit-economy.data.core :as data]))


;; The decision system, basically actions are broken down into decisions and choices, at the decision stage we're
;;   picking between a bunch of options, eliminating any ones we can because we know we can't follow through with it,
;;   because we don't meet a prerequisite.
;;   Once a decision is decided, we then need to make some sub-decisions which for now we'll call choices.
;;   These choices are for figuring out details, for example: if we decide to claim a territory,
;;   the choice would be which one?
(defn decide
  "Makes a high level decision, basically what will be done"
  [world-db {decider-id :db/id civ-name :fruit-economy.civ/name :keys [decisions name] :as peep}]
  ;; Eliminate options, for example are there valid territories to claim? If not we shouldn't choose it,
  ;;   there's scope in the future when we weight decisions based on the peep where if they can't choose their favourite
  ;;   decision it annoys them or makes them brood for a turn etc...
  {:decision (rand-nth decisions) :decider name :decider-id decider-id :fruit-economy.civ/name civ-name})

(defn choose
  "Decides the details of the decision given, these sub-decisions won't necessarily be made by the same peep who made
   the decision."
  [world-db {chooser-id :db/id civ-name :fruit-economy.civ/name :keys [name] :as peep} {decision-action :decision :as decision}]
  (let [territory (->> (data/civ-name->civ world-db civ-name '[{:civ/territory [:area]}])
                    :civ/territory
                    (mapv :area))
        land-data (data/land-data world-db)
        terrain (::land/terrain land-data)
        _ (println :territory territory)]
    (condp = decision-action
      :claim (let [candidates (coords/get-territory-borders land-data territory)
                   ;; remove invalid, locations TODO: can we turn this into a xf and pass optionally pass it into get-territory-borders?
                   _ (println :claim :pre-candidates candidates terrain)
                   candidates (into [] (remove (fn [[x y]] (= (get-in terrain [y x]) :ocean))) candidates)
                   _ (println :claim :candidates candidates)]
               ;; we get a sub-decision to attempt to claim target!
               (if (seq candidates)
                 (let [target (rand-nth candidates)]
                   (assoc decision :target target :chooser name :chooser-id chooser-id))
                 (assoc decision :decision :nonviable :initial-decision (:decision decision) :chooser name :chooser-id chooser-id)))
      :develop (let [candidates (vec territory)
                     _ (println :develop :candidates candidates)]
                 ;; we get a sub-decision to attempt to develop (reduce the wildness of the tile or make the tile more
                 ;;   hospitable to the civ, so maybe wild races make the tiles wilder?) target!
                 (if (seq candidates)
                   (let [target (rand-nth candidates)]
                     (assoc decision :target target :chooser name :chooser-id chooser-id))
                   (assoc decision :decision :nonviable :initial-decision (:decision decision) :chooser name :chooser-id chooser-id)))
      :gather (let [;; This one may need a precondition that the civ has a claimed resource?
                    resource-areas (into #{} (map :area) (data/land-resources world-db))
                    candidates (into [] (filter (partial contains? resource-areas)) territory)
                    _ (println :gather :candidates candidates :resource-areas resource-areas)]
                ;; we get a sub-decision to attempt to gather target resource!
                (if (seq candidates)
                  (let [target (rand-nth candidates)]
                    (assoc decision :target target :chooser name :chooser-id chooser-id))
                  (assoc decision :decision :nonviable :initial-decision (:decision decision) :chooser name :chooser-id chooser-id)))
      ;; no real sub-decision to be made here, we just have the pop grow
      ;;   we're adding the :chooser and :chooser-id, because they could choose to block in future
      :grow (assoc decision :chooser name :chooser-id chooser-id))))

;; TODO: Consider keeping these user messages as data and only converting them as a last step, this way agents can process them.
(defn decision-explanation [{:keys [chooser decider target decision initial-decision] civ-name :fruit-economy.civ/name}]
  (cond
    (and (= :nonviable decision) (= :gather initial-decision))
    (str "In " civ-name " " decider " wanted to " initial-decision ", but " (if (= chooser decider) "later realised it was inadvisable" (str chooser " realised it was not possible")) " as there are no resources.")

    (= :nonviable decision)
    (str "In " civ-name " " decider " wanted to " initial-decision " @ " target ", but " (if (= chooser decider) "later realised it was inadvisable." (str chooser " realised it was not possible.")))

    :else
    (str "In " civ-name " " decider " chose to " decision " @ " target)))

(defn process-decision
  "Take a decision and specify what needs to be done"
  [world-db {decision-action :decision :keys [target chooser-id] civ-name :fruit-economy.civ/name :as decision}]
  (let [{::land/keys [area->civ-name civ-name->civ] :as land-data} (data/land-data world-db)]
    (condp = decision-action
      :nonviable (data/log-history (decision-explanation decision))
      :claim (let [{new-civ-id :db/id :as new-civ} (data/civ-name->civ world-db civ-name [{:civ/territory '[*]}])
                   area->claim-ids (into {} (map (juxt :area :db/id)) (data/land-claims world-db))
                   existing-claim-id (get area->claim-ids target)]
               (println :target target :area->civ-name area->civ-name)
               (println :new-civ civ-name (pr-str new-civ))
               (into
                 (data/log-history (decision-explanation decision))
                 ;; Is there an existing claim?
                 (if existing-claim-id
                   (let [{old-civ-id :db/id :as old-civ} (data/land-claims->civ world-db existing-claim-id)]
                     (println :old-civ existing-claim-id (pr-str old-civ))
                     [;; Remove the area from the old civ
                      [:db/retract old-civ-id :civ/territory existing-claim-id]
                      ;; Mark down the area as belonging to the new civ
                      [:db/add new-civ-id :civ/territory existing-claim-id]
                      [:db/retract chooser-id :planned-decision]])
                   ;; Mark down the area in the new civ's territory
                   [{:db/id new-civ-id :civ/territory {:area target :development 0}}
                    [:db/retract chooser-id :planned-decision]])))
      :develop (let [{civ-id :db/id territory->development :fruit-economy.civ/territory->development} (data/civ-name->civ world-db civ-name [:fruit-economy.civ/territory->development])]
                 (into
                   (data/log-history (decision-explanation decision))
                   [{:fruit-economy.civ/territory->development (update territory->development target (fnil inc 0)) :db/id civ-id}
                    [:db/retract chooser-id :planned-decision]]))
      :gather (let [area->resources (into {} (map (juxt :area #(select-keys % [:name :kind :glyph])) (data/land-resources world-db)))
                    resource (get area->resources target)
                    {civ-id :db/id store :fruit-economy.civ/store} (data/civ-name->civ world-db civ-name [:fruit-economy.civ/store])]
                (into
                  (data/log-history (decision-explanation decision))
                  [{:fruit-economy.civ/store (update store resource (fnil inc 0)) :db/id civ-id}
                   [:db/retract chooser-id :planned-decision]]))
      :grow (let [{civ-id :db/id power :fruit-economy.civ/power} (data/civ-name->civ world-db civ-name [:fruit-economy.civ/power])]
              (into
                (data/log-history (decision-explanation decision))
                [{:fruit-economy.civ/power (inc power) :db/id civ-id}
                 [:db/retract chooser-id :planned-decision]])))))

(defn leader-tick
  "Make a decision for each peep"
  [world-db {id :db/id :as leader}]
  (let [{:civ/keys [peeps]} (data/peep->civ-peeps world-db id)]
    (println :peeps peeps)
    (reduce
      (fn [v peep]
        (conj v {:db/id (:db/id peep) :planned-decision (decide world-db leader)}))
      []
      peeps)))

(defn subordinate-tick
  "Take the planned decision given to the peep and generate the changes"
  [world-db {worker-id :db/id :keys [name planned-decision] :as peep}]
  (let [subordinate-choose (choose world-db peep planned-decision)
        ;; Just in case I want to split it further, it should be easy to add in a separate worker.
        worker-execute (assoc subordinate-choose :worker-name name :worker-id worker-id)]
    (process-decision world-db worker-execute)))

(comment
  (let [width 3 height 3
        top-left-x 0 top-left-y 0
        bottom-right-x (dec width) bottom-right-y (dec height)
        x 2 y 2 parent nil
        land-data (-> (land/make-land "World" width height)
                    (land/gen-land))
        {::land/keys [name terrain curr-civ-id civ-letters lang] :as land-data} land-data
        symbol (first civ-letters)
        civ-name (lang/make-word lang)
        biome (get-in terrain [y x])
        first-name (lang/make-word lang)
        last-name (lang/make-word lang)
        new-peep {:fruit-economy.civ/id curr-civ-id
                  :fruit-economy.civ/name civ-name
                  :name (str first-name " " last-name)
                  :first-name first-name
                  :last-name last-name
                  :kind :peep
                  :glyph "🧑"
                  :decisions [:claim :develop :gather :grow]}
        new-civ (civ/make-civ curr-civ-id civ-name symbol [x y] name biome parent [new-peep])

        ;; This is about having a leader decide what action to do next.
        decide-action (rand-nth (get new-peep :decisions))]
    (-> land-data
      (assoc-in [::land/area->civ-name [x y]] civ-name)
      (update ::land/civ-name->civ assoc civ-name new-civ)

      ;; Assume this happens much later after world gen, so we need to grab info out, we want a process decision kind of thing,
      ;;   ie what are the next decisions that need to be made now that the leader has made a choice,
      ;;   say we're claiming an adjacent territory, someone has to decide which adjacent territory, and this could be the leader,
      ;;   but doesn't *have* to be, maybe it's the head explorer for example.
      (as-> $
        (let [decision (assoc (decide $ new-peep) :decision :claim)
              decision (choose $ new-peep decision)]
          (process-decision $ decision))
        #_
        (condp = decide-action
          :claim (let [candidates (coords/get-territory-borders $ (get-in $ [::land/civ-name->civ civ-name :fruit-economy.civ/territory]))
                       ;; remove invalid, locations TODO: can we turn this into a xf and pass optionally pass it into get-territory-borders?
                       candidates (into [] (remove (fn [[x y]] (= (get-in $ [::land/civ-name->civ civ-name :fruit-economy.civ/terrain y x]) :ocean))) candidates)
                       target (rand-nth candidates)]
                   ;; we get a sub-decision to attempt to claim target!
                   ,)
          :develop (let [candidates (get-in $ [::land/civ-name->civ civ-name :fruit-economy.civ/territory])
                         target (rand-nth candidates)]
                     ;; we get a sub-decision to attempt to develop (reduce the wildness of the tile or make the tile more
                     ;;   hospitable to the civ, so maybe wild races make the tiles wilder?) target!
                     ,)
          :gather (let [;; This one may need a precondition that the civ has a claimed resource?
                        area->resources (get $ ::land/area->resources)
                        candidates (into [] (filter area->resources) (get-in $ [::land/civ-name->civ civ-name :fruit-economy.civ/territory]))
                        target (rand-nth candidates)]
                    ;; we get a sub-decision to attempt to gather target resource!
                    ,)
          :grow (-> $
                  (update-in [::land/civ-name->civ civ-name :fruit-economy.civ/power] inc)
                  ;; also, if our pop hits a threshold (fibonacci number) we gain a new worker!
                  ,)))

      ;; Let's start with a situation when no action is planned
      (subordinate-tick new-peep) ;; nothing should happen

      ;; Now let's do a leader tick, followed by a subordinate-tick
      (leader-tick new-peep) ;; all the civ peeps have a new :planned-decision
      (as-> $
        (let [peep (get-in $ [::land/civ-name->civ civ-name :fruit-economy.civ/peeps 0])]
          (subordinate-tick $ peep))) ;; this peep executes their decision

      (get-in [::land/civ-name->civ civ-name])))
  ,)
