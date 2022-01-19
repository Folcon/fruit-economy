(ns fruit-economy.noise
  (:import [org.kdotjpg.noise OpenSimplex2]))


(defn simplex-noise-fn [seed x y]
  (OpenSimplex2/noise2 seed x y))

;; https://cmaher.github.io/posts/working-with-simplex-noise/
(defn coord->noise [noise-fn [x y] {:keys [seed octave lacunarity persistence scale offset low high normalise?] :or {scale [1 1] offset [0 0]}}]
  (let [[scale-x scale-y] scale
        [offset-x offset-y] offset
        offset-x-fn (if (zero? offset-x) identity (partial + offset-x)) offset-y-fn (if (zero? offset-y) identity (partial + offset-y))
        [result result-amplitude]
        (loop [result 0
               result-amplitude 0
               frequency 1
               amplitude 1
               idx 0]
          (if (< idx octave)
            (recur
              (+ result (* (noise-fn seed (* frequency (offset-x-fn x) scale-x) (* frequency (offset-y-fn y) scale-y)) amplitude))
              (+ result-amplitude amplitude)
              (* amplitude persistence)
              (* frequency lacunarity)
              (inc idx))
            [result result-amplitude]))]
    (cond-> result
      normalise?
      (as-> $ (/ $ result-amplitude))

      (and low high)
      (as-> $ (+ (/ (* $ (- high low)) 2) (/ (+ high low) 2))))))


(comment
  (let [n 0
        octaves [2 4 8] lacunarity [1.25 2 4]]
    (coord->noise simplex-noise-fn [10 10] {:seed 1 :octave (nth octaves n) :lacunarity (nth lacunarity n) :persistence 0.5 :scale [1 1] :normalise? false})))
