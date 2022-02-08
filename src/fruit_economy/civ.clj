(ns fruit-economy.civ
  (:require [fruit-economy.rand :refer [roll]]
            [fruit-economy.colour :refer [colour]]
            [fruit-economy.language :refer [make-word]]
            [fruit-economy.land :as land :refer [log-history]]
            [fruit-economy.economy :as economy]
            [fruit-economy.data.core :as data]
            [fruit-economy.sim.core :as sim]))


(defn make-civ [id name symbol origin home-world-name terrain ancestor peeps]
  (let [[x y] origin]
    (merge
      {::id id
       ::name name
       ::symbol symbol
       ::tint (colour (rand-int 256) (rand-int 256) (rand-int 256))
       ::territory #{origin}
       ::territory->development {origin 0}
       :civ/territory [{:area origin :development 0}]
       ::home-world home-world-name
       ::home-biome terrain

       ;; peeps
       ::peeps peeps
       :civ/peeps peeps

       ;; stats
       ::instability -10  ;; low starting instability to ensure early expansion/survival
       ::power (reduce + (repeatedly 3 #(roll 10)))
       ::power-base 1
       ::tech-level 0
       ::dead false

       ;; diplomatic profile
       ::profile {:friendliness (roll 100)
                  :trustworthiness (roll 100)
                  :fearfulness (roll 100)
                  :reputation 0}

       ;; diplomatic relationships
       ::relationships {}
       ::age 0}
      (when ancestor
        {::ancestor ancestor}))))

(defn peep-on-tick [{id :db/id :keys [area decisions] :as peep} world-db]
  (let [[x y] area dir-x (rand-nth [-1 0 1]) dir-y (rand-nth [-1 0 1])
        x' (+ x dir-x) y' (+ y dir-y) area' [x' y']
        land-data (data/land-data world-db)
        target (get-in land-data [::land/terrain y' x'])]
    (into [(when (and target (not= target :ocean))
             {:db/id id :area area'})]
      (when (seq decisions)
        (sim/leader-tick world-db peep)))))

(defn spawn-civ [{::land/keys [name terrain curr-civ-id civ-letters lang] :as land-data} x y {:keys [parent]}]
  (if (seq civ-letters)
    (let [symbol (first civ-letters)
          civ-name (make-word lang)
          biome (get-in terrain [y x])
          new-peep {::id curr-civ-id
                    ::name civ-name
                    :name (str "Peep " (inc (rand-int 1000)))
                    :kind :peep
                    :glyph "🧑"
                    :area [x y]
                    :on-tick peep-on-tick
                    :decisions [:claim :develop :gather :grow]}
          new-civ (make-civ curr-civ-id civ-name symbol [x y] name biome parent [new-peep])]
      (-> land-data
        (log-history (str "Spawning new civ at " x " " y " on " biome))
        (assoc-in [::land/area->civ-name [x y]] civ-name)
        (update ::land/civ-name->civ assoc civ-name new-civ)
        (update :land/civs (fnil conj []) new-civ)
        (update ::land/civ-letters disj symbol)
        (update ::land/curr-civ-id inc)
        (economy/init-civ-economy new-civ)))
    (-> land-data
      (log-history (str "Tried to spawn new civ at " x " " y " ran out of letters")))))

(defn try-spawn-new-civs [{::land/keys [width height] :as land-data} n-attempts]
  (reduce
    (fn [land _n]
      (let [x (rand-int width)
            y (rand-int height)
            target (get-in land [::land/terrain y x])
            occupied? (get-in land [::land/area->civ-name [x y]])]
        (println :try-spawn-civ _n target :occupied? occupied?)
        (if (and (not= target :ocean)
              (not occupied?))
          (spawn-civ land x y {})
          land)))
    land-data
    (range n-attempts)))
