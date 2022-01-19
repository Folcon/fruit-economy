(ns fruit-economy.land
  (:require [fruit-economy.gen-land :refer [make-temp-noise-map make-elev-noise-map process-noise-map]]
            [fruit-economy.colour :refer [colour]]))


;; TODO: Spec land, civ, manor and validate
(defn make-land
  ([name width height] (make-land name width height {}))
  ([name width height {:keys [temp-mod elev-mod temp-noise elev-noise base-biome] :or {temp-mod 0.1 elev-mod 0 base-biome :ocean}}]
   (let [temp-noise (or temp-noise (make-temp-noise-map width height))
         elev-noise (or elev-noise (make-elev-noise-map width height))]
     {::name name
      ::temp-noise temp-noise ::elev-noise elev-noise
      ::temp (process-noise-map temp-noise temp-mod)
      ::elev (process-noise-map elev-noise elev-mod)
      ::base-biome base-biome
      ::terrain (vec (repeat height (vec (repeat width base-biome))))
      ::area->civ-name {}
      ::area->manor {}
      ::area->ruin {}
      ::width width ::height height ::sea-level (rand-nth (range 0.001 0.009 0.001))
      ::curr-civ-id 0
      ::civ-name->civ {}
      ::civ-missives []
      ::civ-letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-_=+[{]}\\|;:,<.>/?"})))

(defn gen-land [{::keys [width height sea-level] :as land-data}]
  (reduce
    (fn [land y]
      (reduce
        (fn [{::keys [temp elev] :as land} x]
          (let [local-temp (get-in temp [y x])
                local-elev (get-in elev [y x])]
            (cond
              (and (>= local-elev sea-level) (< local-elev 0.1))
              (assoc-in land [::terrain y x] :beach)

              (and (>= local-elev 0.4) (< local-temp -0.2))
              (assoc-in land [::terrain y x] :snow-mountain)

              (>= local-elev 0.4)
              (assoc-in land [::terrain y x] :mountain)

              ;; temperate region
              (> local-elev sea-level)
              (cond
                (< local-temp -0.2)
                (assoc-in land [::terrain y x] :snow)

                (< local-temp 0)
                (assoc-in land [::terrain y x] :tundra)

                (< local-temp 0.1)
                (assoc-in land [::terrain y x] :grassland)

                (< local-temp 0.2)
                (assoc-in land [::terrain y x] :forest)

                (< local-temp 0.3)
                (assoc-in land [::terrain y x] :jungle)

                :else
                (assoc-in land [::terrain y x] :desert))

              :else
              land)))
        land
        (range width)))
    land-data
    (range height)))

(defn render-tile-colour [terrain]
  (condp = terrain
    :beach (colour 230 236 172) #_(rand-nth [(colour 117 139 171) (colour 230 236 172)])
    :desert (colour 246 244 118)
    :forest (colour 64 88 37)
    :grassland (colour 78 138 53)
    :jungle (colour 66 108 40)
    :mountain (colour 73 66 52)
    :snow-mountain (colour 86 82 73)
    :ocean (rand-nth [(colour 87 119 197) (colour 87 102 153) (colour 87 102 153)])
    :snow (colour 247 246 247)
    :tundra (colour 153 153 155)
    (colour 0 0 0)))

(defn render-tile-str [terrain]
  (condp = terrain
    :beach "b"
    :desert "d"
    :forest "f"
    :grassland "g"
    :jungle "j"
    :mountain "m"
    :snow-mountain "M"
    :ocean "~"
    :snow "s"
    :tundra "t"
    (str terrain)))

(defn render-manor-colour [{::keys [level] :as _manor}]
  (condp > level
    50 (colour 78 138 55)
    30 (colour 230 236 170)
    10 (colour 0 255 255)
    (colour 87 117 150)))
