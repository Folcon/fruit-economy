(ns fruit-economy.sim.core
  (:require [fruit-economy.land :as land]
            [fruit-economy.language :as lang]
            [fruit-economy.civ :as civ]
            [fruit-economy.sim.coords :as coords]))


;; The decision system, basically actions are broken down into decisions and choices, at the decision stage we're
;;   picking between a bunch of options, eliminating any ones we can because we know we can't follow through with it,
;;   because we don't meet a prerequisite.
;;   Once a decision is decided, we then need to make some sub-decisions which for now we'll call choices.
;;   These choices are for figuring out details, for example: if we decide to claim a territory,
;;   the choice would be which one?
(defn decide
  "Makes a high level decision, basically what will be done"
  [land-data {civ-name ::civ/name :keys [decisions name] :as peep}]
  ;; Eliminate options, for example are there valid territories to claim? If not we shouldn't choose it,
  ;;   there's scope in the future when we weight decisions based on the peep where if they can't choose their favourite
  ;;   decision it annoys them or makes them brood for a turn etc...
  {:decision (rand-nth decisions) :decider name ::civ/name civ-name})

(defn choose
  "Decides the details of the decision given, these sub-decisions won't necessarily be made by the same peep who made
   the decision."
  [{::land/keys [terrain] :as land-data} {civ-name ::civ/name :keys [name] :as peep} {decision-action :decision :as decision}]
  (let [territory (get-in land-data [::land/civ-name->civ civ-name ::civ/territory])
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
                   (assoc decision :target target :chooser name))
                 (assoc decision :decision :nonviable :initial-decision (:decision decision) :chooser name)))
      :develop (let [candidates (vec territory)
                     _ (println :develop :candidates candidates)]
                 ;; we get a sub-decision to attempt to develop (reduce the wildness of the tile or make the tile more
                 ;;   hospitable to the civ, so maybe wild races make the tiles wilder?) target!
                 (if (seq candidates)
                   (let [target (rand-nth candidates)]
                     (assoc decision :target target :chooser name))
                   (assoc decision :decision :nonviable :initial-decision (:decision decision) :chooser name)))
      :gather (let [;; This one may need a precondition that the civ has a claimed resource?
                    area->resources (get land-data ::land/area->resources)
                    candidates (into [] (filter area->resources) territory)
                    _ (println :gather :candidates candidates)]
                ;; we get a sub-decision to attempt to gather target resource!
                (if (seq candidates)
                  (let [target (rand-nth candidates)]
                    (assoc decision :target target :chooser name))
                  (assoc decision :decision :nonviable :initial-decision (:decision decision) :chooser name)))
      ;; no real sub-decision to be made here, we just have the pop grow
      :grow decision)))

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
        new-peep {::civ/id curr-civ-id
                  ::civ/name civ-name
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
        (condp = decide-action
          :claim (let [candidates (coords/get-territory-borders $ (get-in $ [::land/civ-name->civ civ-name ::civ/territory]))
                       ;; remove invalid, locations TODO: can we turn this into a xf and pass optionally pass it into get-territory-borders?
                       candidates (into [] (remove (fn [[x y]] (= (get-in $ [::land/civ-name->civ civ-name ::civ/terrain y x]) :ocean))) candidates)
                       target (rand-nth candidates)]
                   ;; we get a sub-decision to attempt to claim target!
                   ,)
          :develop (let [candidates (get-in $ [::land/civ-name->civ civ-name ::civ/territory])
                         target (rand-nth candidates)]
                     ;; we get a sub-decision to attempt to develop (reduce the wildness of the tile or make the tile more
                     ;;   hospitable to the civ, so maybe wild races make the tiles wilder?) target!
                     ,)
          :gather (let [;; This one may need a precondition that the civ has a claimed resource?
                        area->resources (get $ ::land/area->resources)
                        candidates (into [] (filter area->resources) (get-in $ [::land/civ-name->civ civ-name ::civ/territory]))
                        target (rand-nth candidates)]
                    ;; we get a sub-decision to attempt to gather target resource!
                    ,)
          :grow (-> $
                  (update-in [::land/civ-name->civ civ-name ::civ/power] inc)
                  ;; also, if our pop hits a threshold (fibonacci number) we gain a new worker!
                  ,)))
      (get ::land/civ-name->civ civ-name)))
  ,)
