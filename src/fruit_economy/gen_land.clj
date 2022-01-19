(ns fruit-economy.gen-land
  (:require [fruit-economy.noise :refer [coord->noise simplex-noise-fn]]))


(comment
  (let [n 0
        octaves [2 4 8] lacunarity [1.25 2 4]]
    (coord->noise simplex-noise-fn [10 10] {:seed 1 :octave (nth octaves n) :lacunarity (nth lacunarity n) :persistence 0.5 :scale [1 1] :normalise? false})))

(comment
  (let [n 0
        octaves [2 4 8] lacunarity [1 1.25 2 4]]
    (coord->noise simplex-noise-fn [10 10] {:seed 1 :octave (nth octaves n) :lacunarity (nth lacunarity n) :persistence 0.5 :scale [1 1] :normalise? true})))


;; TODO: Turn these into record based protocol like thing,
;;   ie you have a noise-map, and call [x y] into it and it does the computation with the values it has
(defn simplex-noise-map [width height {:keys [seed octave scale low high] :or {scale 1 low 0 high 1}}]
  (let [norm (fn [v size] (/ (float v) size))]
    (into []
      (for [y (range height)]
        (into []
          (for [x (range width)]
            (coord->noise simplex-noise-fn [(norm x width) (norm y height)]
              {:seed seed :octave octave :lacunarity 1
               :persistence 0.5 :scale [scale scale]
               :normalise? true :low -1 :high 1})))))))

(comment
  (simplex-noise-map 2 3 {:seed 1 :octave 1}))

(defn grey-noise-map
  "greyscale map"
  [width height {:keys [seed octaves]}]
  (let [norm (fn [v size] (/ (float v) size))]
    (into []
      (for [y (range height)]
        (into []
          (for [x (range width)]
            (int (* 256 (coord->noise simplex-noise-fn [(norm x width) (norm y height)]
                          {:seed seed :octave octaves :lacunarity 1
                           :persistence 0.5 :scale [0.007 0.007]
                           :normalise? true :low 0 :high 1})))))))))

(defn make-noise-map [width height seed-size octaves]
  (let [[o0 o1 o2] octaves]
    {:width width :height height
     :noise-coll
     [(simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o0})
      (simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o1})
      (simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o2})]}))

(defn make-temp-noise-map [width height]
  (make-noise-map width height 10000 [2 4 88]))

(comment
  (make-temp-noise-map 2 3))

(defn make-elev-noise-map [width height]
  (make-noise-map width height 10000 [4 10 20]))

(defn messy-noise [noise-coll [x y]]
  (let [[n0 n1 n2] noise-coll]
    (+
      (get-in n0 [y x])
      (* (get-in n1 [y x]) 0.5)
      (* (get-in n2 [y x]) 0.25))))

(defn process-noise-map [{:keys [width height noise-coll] :as _noise-map} val-mod]
  (into []
    (map (fn [row]
           (mapv (fn [coord]
                   (+ (messy-noise noise-coll coord) val-mod))
             row)))
    (for [y (range height)]
      (into []
        (for [x (range width)]
          [x y])))))
