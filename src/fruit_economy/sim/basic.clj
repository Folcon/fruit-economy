(ns fruit-economy.sim.basic
  (:require [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [datascript.core :as d]
            [fruit-economy.colour :refer [colour]])
  (:import [io.github.humbleui.skija Paint]))


(defn loc+entity->entities-coll [loc+entity]
  (reduce
    (fn [m [[loc entity] idx]]
      (conj m (assoc entity :id idx :pos [:loc loc])))
    []
    (map vector loc+entity (range))))

(defn make-bug []
  {:glyph "🐞" :wealth 0 :vision (inc (rand-int 4)) :hunger (inc (rand-int 4))})

(defn gen-bug-world [size n-peeps]
  (into
    (vec
      (for [x (range size)
            y (range size)]
        (let [food #_(inc (rand-int 4)) (- 5 (int (Math/sqrt (rand-int 32))))]
          {:init-food food :food food :max-age (+ (rand-int 5) 20) :loc [x y]})))
    (loc+entity->entities-coll
      (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)])))))


(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(defn reset-world []
  (d/db-with (d/empty-db {:pos {:db/valueType :db.type/ref}
                          :loc {:db/unique :db.unique/identity}})
    (gen-bug-world bug-world-size bug-count)))

(def *world (atom {:world-db (reset-world)}))

(defn loc-q [db loc]
  (d/q '[:find (pull ?e [*]) .
         :in $ ?loc
         :where
         [?e :loc ?loc]
         [?e :food]]
    db
    loc))

(defn units-q [db loc]
  (d/q '[:find [(pull ?e [*])]
         :in $ ?loc
         :where
         [?le :loc ?loc]
         [?e :pos ?le]]
    db
    loc))

(def map-ui-view
  (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black cell tick lrtb map-font emoji-font]} ctx
                   {:keys [world-db]} @*world]
    (let [[left right top bottom] lrtb
          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x
                            loc-y y
                            loc [loc-x loc-y]

                            tile (loc-q world-db loc)

                            things (units-q world-db loc)
                            size (count things)
                            {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))]
                        (when thing
                          (println :wealth thing (get thing :wealth)))
                        (cond
                          thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                          :else ["" (colour 0 (* (get tile :food 0) 40) 0) map-font font-offset-x font-offset-y])))]
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
  (ui/dynamic ctx [{:keys [font-small fill-white fill-green fill-dark-gray]} ctx]
    (ui/column
      map-ui-view
      (ui/row
        (ui/clickable
          #(reset! *world {:world-db (reset-world)})
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-green fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "RESET!" font-small fill-white))))))))))