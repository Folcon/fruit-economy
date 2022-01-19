(ns fruit-economy.civ-actions
  (:require [fruit-economy.land :as land :refer [log-history]]
            [fruit-economy.rand :as rand]
            [fruit-economy.utils :refer [clamp]]))


(defn expand-territory [{::land/keys [width height] :as land-data} {:fruit-economy.civ/keys [dead territory name] :as civ}]
  (println civ "\n  " dead (pr-str territory))
  (if (or (not dead) (seq territory))
    (reduce
      (fn [{::land/keys [terrain civ-name->civ area->civ-name] :as land} _n]
        (let [{:fruit-economy.civ/keys [tech-level home-biome] :as civ} (get civ-name->civ name)
              [attempt-x attempt-y] (rand-nth (vec territory))
              _ (println :attempt [attempt-x attempt-y] [width height] [(clamp (dec attempt-x) {:mx (dec height)}) (clamp (inc attempt-x) {:mx (dec height)})])
              ax (rand/rand-int-between
                   (clamp (dec attempt-x) {:mx (dec width)})
                   (clamp (inc attempt-x) {:mx (dec width)}))
              ay (rand/rand-int-between
                   (clamp (dec attempt-y) {:mx (dec height)})
                   (clamp (inc attempt-y) {:mx (dec height)}))
              attempt-biome (get-in terrain [ay ax])]
          (println :attempt-loc attempt-biome territory [ax ay] (contains? territory [ax ay]))
          ;; cannot expand into water
          (println :tech-level tech-level home-biome)
          (if-not (or (= attempt-biome :ocean) (contains? territory [ax ay]))
            (let [;; base 10% success chance, max +70% from technology
                  chance (cond-> (+ 10 (min 70 (* 5 tech-level)))
                           ;; bonus chance for expansion if attempting to expand into base biome
                           (= attempt-biome home-biome)
                           (+ 20)

                           ;; spreading into occupied squares is harder
                           (get area->civ-name [ax ay])
                           (- 5 (Math/ceil (/ (get-in civ-name->civ [(get area->civ-name [ax ay]) :fruit-economy.civ/tech-level]) 10)))

                           ;; mountains make it hard to spread
                           (= (get-in terrain [ay ax]) :mountain)
                           (- 5)

                           ;; grasslands easy to spread
                           (= (get-in terrain [ay ax]) :grassland)
                           (+ 5))
                  roll-result (rand/roll 100)
                  existing-claim (get area->civ-name [ax ay])]
              (println "<" :roll-result roll-result :chance chance)
              ;; lower rolls are better
              (if (< roll-result chance)
                (cond-> (log-history land (str "Expanding " name " at " [ax ay] (when existing-claim (str " replacing " existing-claim))))
                  ;; overwrite existing territorial claim
                  existing-claim
                  (update-in [::land/civ-name->civ (get area->civ-name [ax ay]) :fruit-economy.civ/territory] disj [ax ay])

                  :always
                  (assoc-in [::land/area->civ-name [ax ay]] name)

                  :always
                  (update-in [::land/civ-name->civ name :fruit-economy.civ/territory] conj [ax ay]))
                land))
            land)))
      land-data
      (range (rand/rand-int-between 2 5)))
    land-data))