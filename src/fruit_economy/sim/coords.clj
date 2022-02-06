(ns fruit-economy.sim.coords
  (:require [fruit-economy.land :as land]))


;; TODO: Improve get-territory-borders by having neighbours only generating coords in bounds.
(defn neighbours
  "Generates the neighbours of a cell on a square grid where movement is orthogonal and diagonal"
  ([[x y]] (neighbours x y))
  ([x y]
   (for [+-x (range -1 2 1)
         +-y (range -1 2 1)
         :when (not (and (= +-x 0) (= +-y 0)))]
     [(+ x +-x) (+ y +-y)])))

(defn get-territory-borders
  "Given a set of coords, generates the coords that are in bounds that are adjacent to the territory but don't contain it"
  [{::land/keys [width height] :as _land-data} territory]
  (let [dec-width (dec width)
        dec-height (dec height)]
    (into [] (comp (map neighbours) cat (distinct) (remove (fn [[x y]] (or ((complement <=) 0 x dec-width) ((complement <=) 0 y dec-height) (contains? territory [x y]))))) territory)))

(comment
  (let [width 3 height 3
        territory
        #{[0 0] [1 1] [2 2] [2 1]}
        #_#{[2 2] [1 2] [2 1] [-1 1] [1 -1] [4 2] [3 4] [4 3] [4 4]}
        width 3 height 3
        territory
        #{[1 1] [0 0]}]
    (into #{} (comp (map neighbours) cat (remove (fn [[x y]] (or ((complement <=) 0 x (dec width)) ((complement <=) 0 y (dec height)) (contains? territory [x y]))))) territory))

  (neighbours 1 0))
