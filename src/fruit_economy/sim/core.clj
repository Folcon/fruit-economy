(ns fruit-economy.sim.core
  (:require [fruit-economy.land :as land]
            [fruit-economy.language :as lang]
            [fruit-economy.civ :as civ]
            [fruit-economy.sim.coords :as coords]))


(comment
  (let [width 3 height 3
        top-left-x 0 top-left-y 0
        bottom-right-x (dec width) bottom-right-y (dec height)
        x 2 y 2 parent nil
        land-data (land/make-land "World" width height)
        {::land/keys [name terrain curr-civ-id civ-letters lang] :as land-data} land-data
        symbol (first civ-letters)
        civ-name (lang/make-word lang)
        biome (get-in terrain [y x])
        first-name (lang/make-word lang)
        last-name (lang/make-word lang)
        new-peep {:civ/id curr-civ-id
                  :civ/name civ-name
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
