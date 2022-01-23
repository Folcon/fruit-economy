(ns fruit-economy.land
  (:require [fruit-economy.gen-land :refer [make-temp-noise-map make-elev-noise-map process-noise-map]]
            [fruit-economy.colour :refer [colour]]
            [fruit-economy.language :refer [make-lang]]
            [fruit-economy.graph :refer [make]]))


(def allowed-civ-letters (into #{} (map str) "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-_=+[{]}\\|;:,<.>/?"))

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
      ::area->units {}
      ::area->civ-name {}
      ::area->manor {}
      ::area->ruin {}
      ::width width ::height height ::sea-level (rand-nth (range 0.001 0.009 0.001))
      ::curr-civ-id 0
      ::civ-name->civ (sorted-map)
      ::civ-missives []
      ::civ-letters allowed-civ-letters
      ::history []
      ::lang (make-lang)
      ::economy (let [nodes [{:id :source :color "green"
                              :label [:P {:BORDER 1} "supply"]}
                             {:id :sink :color "red"
                              :label [:P {:BORDER 1} "demand"]}]
                      edges [[:source :sink]]]
                  {:ubergraph (make {:nodes nodes :edges edges})
                   :nodes nodes
                   :edges edges})})))

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

(def kind->name {:growing-plant "🌱" :dying-plant "🥀"
                 :bush "🌳" :tree-1 "🌴" :tree-2 "🌲" :tree-3 "🌵" :tree-4 "🌾" :tree-5 "🎋" :tree-6 "🎍" :magic-tree "🎄"
                 :flower-1 "🌸" :flower-2 "💮" :flower-3 "🏵️" :flower-4 "🌺" :flower-5 "🌻" :flower-6 "🌼" :flower-7 "🌷"
                 :herb-1 "🌿" :herb-2 "☘️" :herb-3 "🍀" :herb-4 "🍁" :shroom "🍄" :nut-1 "🌰" :nut-2 "🥥"
                 :fruit-1 "🍇" :fruit-2 "🍈" :fruit-3 "🍉" :fruit-4 "🍊" :fruit-5 "🍋" :fruit-6 "🍌" :fruit-7 "🍍" :fruit-8 "🥭"
                 :fruit-9 "🍎" :fruit-10 "🍏" :fruit-11 "🍐" :fruit-12 "🍑" :fruit-13 "🍒" :fruit-14 "🍓" :fruit-15 "🥝" :fruit-16 "🍅"})

(def kind->category {:bush :bush :tree-1 :tree :tree-2 :tree :tree-3 :tree :tree-4 :tree :tree-5 :tree :tree-6 :tree :magic-tree :tree
                     :flower-1 :flower :flower-2 :flower :flower-3 :flower :flower-4 :flower :flower-5 :flower :flower-6 :flower :flower-7 :flower
                     :herb-1 :herb :herb-2 :herb :herb-3 :herb :herb-4 :herb :nut-1 :nut :nut-2 :nut :shroom :shroom
                     :fruit-1 :fruit :fruit-2 :fruit :fruit-3 :fruit :fruit-4 :fruit :fruit-5 :fruit :fruit-6 :fruit :fruit-7 :fruit :fruit-8 :fruit
                     :fruit-9 :fruit :fruit-10 :fruit :fruit-11 :fruit :fruit-12 :fruit :fruit-13 :fruit :fruit-14 :fruit :fruit-15 :fruit :fruit-16 :fruit})

(defn populate [{::keys [width height] :as land-data} n]
  (reduce
    (fn [land attempt]
      (let [x (rand-int width)
            y (rand-int height)
            target (get-in land [::terrain y x])]
        (println attempt target)
        (cond
          ;; we've hit how many we wanted, so stop
          (= n attempt)
          (reduced land)

          (not= target :ocean)
          (let [kind (rand-nth (into [] (remove #{:growing-plant :dying-plant}) (keys kind->name)))]
            (assoc-in land [::area->units [x y]] {:name (str (name target) "-" (name kind)) :kind kind :glyph (kind->name kind)}))

          :else
          land)))
    land-data
    ;; basically try three times for each
    (range (* n 3))))

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

(defn log-history [land-data message]
  (do
    (println message)
    (update land-data ::history conj message)))
