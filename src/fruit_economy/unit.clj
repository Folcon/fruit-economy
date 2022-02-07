(ns fruit-economy.unit
  (:require [fruit-economy.data.core :as data]))


(defn unit-on-tick [{:keys [loc] :as unit} world-db]
  (let [[x y] loc dir-x (rand-nth [-1 0 1]) dir-y (rand-nth [-1 0 1])
        x' (+ x dir-x) y' (+ y dir-y) loc' [x' y']
        land-data (data/land-data world-db)
        target (get-in land-data [:fruit-economy.land/terrain y' x'])]
    (if (and target (not= target :ocean))
      [loc' (assoc unit :loc loc')]
      [loc unit])))

(defn spawn-units [{:fruit-economy.land/keys [width height] :as land-data} n]
  (reduce
    (fn [land attempt]
      (let [x (rand-int width)
            y (rand-int height)
            target (get-in land [:fruit-economy.land/terrain y x])]
        (println attempt target)
        (cond
          ;; we've hit how many we wanted, so stop
          (= n attempt)
          (reduced land)

          (not= target :ocean)
          (let [[sub-kind kind glyph] (rand-nth
                                        [[:rabbit :wildlife "🐇"]
                                         [:deer :wildlife "🦌"]
                                         [:dragon :monster "🐉"]
                                         [:spider :monster "🕷️"]])]
            (println sub-kind kind glyph)
            (assoc-in land [:fruit-economy.land/area->units [x y]] {:name (str (name target) "-" (name sub-kind)) :kind kind :glyph glyph :loc [x y] :on-tick unit-on-tick}))

          :else
          land)))
    land-data
    ;; basically try three times for each
    (range (* n 3))))
