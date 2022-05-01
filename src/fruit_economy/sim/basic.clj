(ns fruit-economy.sim.basic
  (:require [clojure.walk :as walk]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [datascript.core :as d]
            [fruit-economy.gen-land :refer [make-temp-noise-map make-elev-noise-map process-noise-map]]
            [fruit-economy.land :refer [render-tile-colour]]
            [fruit-economy.colour :refer [colour colour-noise]]
            [fruit-economy.data.core :refer [lookup-avet]])
  (:import [io.github.humbleui.skija Paint]))


(defn rewrite
  "Tries to match rule against db
  Returns tx-data or empty list if rule does not match"
  [{:keys [when then args]} db]
  (let [syms (for [row then el row :when (symbol? el)] el)
        results (apply d/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args))]
    (for [match results tx then]
      (let [swaps (zipmap syms match)
            f (fn [x] (if (coll? x) x (get swaps x x)))]
        (walk/postwalk f tx)))))

(defn infer
  "Returns a new db with all inferred facts and a list of tx-data.
  Stops when no more rules apply or after 100 iterations"
  ([db rules] (infer db rules 100))
  ([db rules max-iter]
   (loop [db db max-iter max-iter]
     (let [tx-data (for [rule rules tx (rewrite rule db)] tx)]
       (cond
         (empty? tx-data) db
         (zero? max-iter) db
         :else (recur (d/db-with db tx-data) (dec max-iter)))))))


(defn coord+entity->entities-coll [coord+entity]
  (reduce
    (fn [m [coord entity]]
      (conj m (assoc entity :place [:coord coord])))
    []
    coord+entity))

(defn make-bug []
  {:glyph "🐞" :wealth 0 :vision (inc (rand-int 4)) :hunger (inc (rand-int 4)) :max-age (+ (rand-int 5) 20)})

(defn resource-fn [local-elev sea-level x] (if (>= local-elev sea-level) (int (* (/ (+ x 1) 2) 5)) 0))

(defn decide-biome [local-temp local-elev sea-level]
  (cond
    (and (>= local-elev sea-level) (< local-elev 0.1)) :beach
    (and (>= local-elev 0.4) (< local-temp -0.2)) :snow-mountain
    (>= local-elev 0.4) :mountain
    ;; temperate region
    (> local-elev sea-level)
    (cond
      (< local-temp -0.2) :snow
      (< local-temp 0) :tundra
      (< local-temp 0.1) :grassland
      (< local-temp 0.2) :forest
      (< local-temp 0.3) :jungle
      :else :desert)
    :else :ocean))

(defn gen-bug-world [size n-peeps]
  (let [temp-noise (make-temp-noise-map size size)
        elev-noise (make-elev-noise-map size size)
        temp-mod 0.1 elev-mod 0
        temp (process-noise-map temp-noise temp-mod)
        elev (process-noise-map elev-noise elev-mod)
        sea-level (rand-nth (range 0.001 0.009 0.001))]
    (into
      (vec
        (for [x (range size)
              y (range size)]
          (let [local-temp (get-in temp [y x]) local-elev (get-in elev [y x])
                food (resource-fn local-elev sea-level local-temp)
                rock (resource-fn local-elev sea-level local-elev)]
            {:init-food food :food food :rock rock :coord [x y] :temp local-temp :elev local-elev :biome (decide-biome local-temp local-elev sea-level)})))
      (coord+entity->entities-coll
        (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)]))))))


(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(defn reset-world []
  (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                          :coord {:db/unique :db.unique/identity}})
    (gen-bug-world bug-world-size bug-count)))

(def *world (atom {:world-db (reset-world) :map-view :default-view}))

(defn coord-q [db coord]
  (-> (lookup-avet db :coord coord)
    first
    d/touch))

(defn units-q
  "coord can be nil or [x y]"
  [db coord]
  (->> (when coord
         [:coord coord])
    (lookup-avet db :place)
    (mapv :db/id)
    (d/pull-many db '[* {:place [:coord]}])
    (mapv #(update % :place :coord))))


(defn sees [coord vision]
  (let [[x y] coord]
    (into []
      cat
      [(for [x (range (- x vision) (inc (+ x vision)))
             :when (not= [x y] coord)]
         [x y])
       (for [y (range (- y vision) (inc (+ y vision)))
             :when (not= [x y] coord)]
         [x y])])))

(defn best-food [db coord sees]
  (let [get-food (fn [coord]
                   (-> (lookup-avet db :coord coord [:food])
                     (first)
                     (get :food 0)))]
    (reduce
      (fn [[coord food] target]
        (let [target-food (get-food target)]
          (if (> target-food food)
            [target target-food]
            [coord food])
          (if (> target-food food)
            [target target-food]
            [coord food])))
      [coord (get-food coord)]
      sees)))

(defn hunt [db coord vision]
  (let [[best-food-coord _food] (best-food db coord (sees coord vision))]
    best-food-coord))

(def hunt-rule
  (-> '{:when [[?e :place ?ce]
               [?e :vision ?vision]
               [?ce :coord ?coord]
               [?ce :food ?food]
               [(hunt $ ?coord ?vision) ?target]
               [(not= ?target ?coord)]
               [?te :coord ?target]]
        :then [[:db/add ?e :place ?te]]}
    (merge {:args {'hunt hunt}})))

(defn gathered [food] (min food (+ (quot food 2) 2)))

(def try-eat-rule
  (-> '{:when [[?e :place ?ce]
               [?e :wealth ?wealth]
               [?e :hunger ?hunger]
               [?ce :coord ?coord]
               [?ce :food ?food]
               [(gathered ?food) ?gather]
               [(- ?food ?gather) ?rem-food]
               [(+ ?wealth ?gather) ?gain-wealth]
               [(- ?gain-wealth ?hunger) ?rem-wealth]]
        :then [[:db/add ?e :wealth ?rem-wealth]
               [:db/add ?ce :food ?rem-food]]}
    (merge {:args {'gathered gathered}})))

(def remove-starving-rule
  '{:when [[?e :wealth ?wealth]
           [(< ?wealth 0)]]
    :then [[:db/retractEntity ?e]]})

(defn grow-food [food init-food] (min (inc food) init-food))

(def grow-food-rule
  (-> '{:when [[?e :food ?food]
               [?e :init-food ?init-food]
               [(< ?food ?init-food)]
               [(grow-food ?food ?init-food) ?gain-food]]
        :then [[:db/add ?e :food ?gain-food]]}
    (merge {:args {'grow-food grow-food}})))

(def rules
  [hunt-rule
   try-eat-rule
   remove-starving-rule
   grow-food-rule])

(def map-ui-view
  (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black cell tick lrtb map-font emoji-font]} ctx
                   {:keys [world-db map-view]} @*world]
    (let [[left right top bottom] lrtb
          terrain-tint (condp = map-view
                         :temp-view (fn [tile] (colour (colour-noise (get tile :temp)) 0 0))
                         :elev-view (fn [tile] (colour 0 (colour-noise (get tile :elev)) 0))
                         :climate-view (fn [tile] (colour (colour-noise (get tile :temp)) (colour-noise (get tile :elev)) 0))
                         :forage-view (fn [tile] (colour 0 (* (get tile :food 0) 40) 0))
                         :mine-view (fn [tile] (colour 0 (* (get tile :rock 0) 40) 0))
                         (fn [tile] (render-tile-colour (get tile :biome))))
          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            coord-x x
                            coord-y y
                            coord [coord-x coord-y]

                            tile (coord-q world-db coord)

                            things (units-q world-db coord)
                            size (count things)
                            {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))]
                        (when thing
                          (println :wealth thing (get thing :wealth)))
                        (cond
                          thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                          :else ["" (terrain-tint tile) map-font font-offset-x font-offset-y])))]
      (ui/column
        (interpose (ui/gap 0 0)
          (for [y-idx (range top bottom)]
            (ui/row
              (interpose (ui/gap 0 0)
                (for [x-idx (range left right)]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [[glyph tile-colour font] (unit-data x-idx y-idx)]
                        (ui/fill (if hovered?
                                   (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                   (paint/fill tile-colour))
                          (ui/width cell
                            (ui/halign 0.5
                              (ui/height cell
                                (ui/valign 0.5
                                  (ui/label glyph font fill-white))))))))))))))))))

(def ui-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-green fill-yellow fill-dark-gray]} ctx
                   {:keys [world-db map-view]} @*world]
    (ui/column
      map-ui-view
      (ui/row
        (ui/clickable
          #(reset! *world {:world-db (reset-world)})
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-yellow fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "RESET!" font-small fill-white))))))
        (ui/clickable
          #(swap! *world update :world-db infer rules 1)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-yellow fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "Apply Rules" font-small fill-white)))))))
      (ui/row
        (ui/clickable
          #(swap! *world assoc :map-view :default-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :default-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🗺️" font-small fill-white))))))
        (ui/clickable
          #(swap! *world assoc :map-view :temp-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :temp-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌡" font-small fill-white))))))
        (ui/clickable
          #(swap! *world assoc :map-view :elev-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :elev-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "📏" font-small fill-white))))))
        (ui/clickable
          #(swap! *world assoc :map-view :climate-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :climate-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌍" font-small fill-white))))))
        (ui/clickable
          #(swap! *world assoc :map-view :forage-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :forage-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🚜" font-small fill-white))))))
        (ui/clickable
          #(swap! *world assoc :map-view :mine-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :mine-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "⛏️" font-small fill-white))))))))))

