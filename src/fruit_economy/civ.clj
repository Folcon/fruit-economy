(ns fruit-economy.civ
  (:require [fruit-economy.rand :refer [roll]]
            [fruit-economy.colour :refer [colour]]
            [fruit-economy.land :as land :refer [log-history]]
            [fruit-economy.economy :as economy]))


(defn make-civ [name symbol origin home-world-name terrain ancestor]
  (let [[x y] origin]
    (merge
      {::name name
       ::symbol symbol
       ::tint (colour (rand-int 256) (rand-int 256) (rand-int 256))
       ::territory #{origin}
       ::home-world home-world-name
       ::home-biome terrain

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

(defn spawn-civ [{::land/keys [name terrain curr-civ-id civ-letters] :as land-data} x y {:keys [parent]}]
  (if (seq civ-letters)
    (let [symbol (first civ-letters)
          civ-name (str "Civ " symbol "+" curr-civ-id)
          biome (get-in terrain [y x])
          new-civ (make-civ civ-name symbol [x y] name biome parent)]
      (-> land-data
        (log-history (str "Spawning new civ at " x " " y " on " biome))
        (assoc-in [::land/area->civ-name [x y]] civ-name)
        (update ::land/civ-name->civ assoc civ-name new-civ)
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
            target (get-in land [::land/terrain y x])]
        (println _n target)
        (if (not= target :ocean)
          (spawn-civ land x y {})
          land)))
    land-data
    (range n-attempts)))
