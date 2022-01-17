(ns fruit-economy.noise
  (:require [clisk.core :refer [show image]]
            [clisk.patterns :as patterns]
            [clisk.functions :as functions]
            [clisk.live :as clisk]
            [clisk.util :refer [red-from-argb green-from-argb blue-from-argb alpha-from-argb]]))



(defn make-perlin-noise [{:keys [seed octaves lacunarity gain scale] :as opts}]
  (if seed
    (patterns/seed-perlin-noise! seed)
    (patterns/seed-perlin-noise!))
  (image
    (patterns/make-multi-fractal
      patterns/perlin-noise
      opts)
    opts))

(defn make-terrain [{:keys [seeds seed size octaves lacunarity as-img? display-colour? display-gradient?] :or {seed 10 seeds [10 10 10] octaves [2 4 8] lacunarity [1.25 2 4] size 512}}]
  (let [opts {:size size :seed seed}
        img-fn (if as-img? (fn [f] (image f :size size)) identity)
        display-colour-fn (if display-colour? functions/vector-offsets identity)
        display-gradient-fn (if display-gradient? functions/gradient identity)
        ;; TODO: Make noise properly care about seed
        ;seed (hash seeds)
        [seed-1 seed-2 seed-3] seeds
        [octave-1 octave-2 octave-3] octaves
        [lacunarity-1 lacunarity-2 lacunarity-3] lacunarity
        _ (patterns/seed-perlin-noise! seed-1)
        a (img-fn
            (patterns/make-multi-fractal
              (display-colour-fn patterns/perlin-noise)
              (assoc opts :octaves octave-1)))
        _ (patterns/seed-perlin-noise! seed-2)
        b (img-fn
            (patterns/make-multi-fractal
              (display-colour-fn patterns/perlin-noise)
              (assoc opts :octaves octave-2)))
        _ (patterns/seed-perlin-noise! seed-3)
        c (img-fn
            (patterns/make-multi-fractal
              (display-colour-fn patterns/perlin-noise)
              (assoc opts :octaves octave-3)))]
    (display-gradient-fn (functions/compose (functions/v* a lacunarity-1) (functions/v* lacunarity-2 b) (functions/v* lacunarity-3 c)))))

(comment
  (show (functions/height-normal 0.25 (make-terrain {:seed 10 :seeds [10 10 10] :display-colour? true :display-gradient? false :as-img? false})))

  (show (make-terrain {:seed 10 :seeds [10 10 10] :display-colour? true :display-gradient? false :as-img? false}) :size 512)

  (def a (patterns/make-multi-fractal (make-perlin-noise {:size 512})
           {:octaves 1 :lacunarity 0.5}))

  (def b (patterns/make-multi-fractal (make-perlin-noise {:size 512})
           {:octaves 1 :lacunarity 0.25}))

  (show b :size 512)

  (let [size 512
        opts {:size size :seed 10}
        a (patterns/make-multi-fractal
            (functions/vector-offsets patterns/perlin-noise)
            (assoc opts :octaves 2))
        b (patterns/make-multi-fractal
            (functions/vector-offsets patterns/perlin-noise)
            (assoc opts :octaves 4))
        c (patterns/make-multi-fractal
            (functions/vector-offsets patterns/perlin-noise)
            (assoc opts :octaves 8))]
    (show (functions/gradient (functions/compose (functions/v* a 1.25) (functions/v* 2 b) (functions/v* 4 c))) #_(functions/compose a (functions/v* 2 b) (functions/v* 4 c)) :size size)
    #_(show
        (functions/compose
          a
          (patterns/make-multi-fractal b {:octaves 2 :lacunarity 1 :gain 0.5 :scale 2})
          (patterns/make-multi-fractal c {:octaves 1 :lacunarity 1 :gain 0.5 :scale 8}))
        :size size))

  (show (patterns/make-multi-fractal (make-perlin-noise {:size 512})
          {:octaves 1 :lacunarity 0.25}) :size 512))

(defn image->rgba [img x y]
  (let [colour (.getRGB img x y)]
    (mapv (fn [f] (f colour)) [red-from-argb green-from-argb blue-from-argb alpha-from-argb])))

(defn image->rgb [img x y]
  (let [colour (.getRGB img x y)]
    (mapv (fn [f] (f colour)) [red-from-argb green-from-argb blue-from-argb])))

(defn image->number [img x y]
  (let [colour (.getRGB img x y)]
    (reduce (fn [v f] (+ v (f colour))) 0 [red-from-argb green-from-argb blue-from-argb])))


(defn scale [img-or-fn n]
  (functions/scale n img-or-fn))

(defn v* [img-or-fn n]
  (functions/v* img-or-fn n))

(defn compose
  ([f g]
   (image (functions/compose f g)))
  ([f g & more]
   (image (functions/compose f (apply functions/compose g more)))))

(defn gradient [img-or-fn]
  (functions/gradient img-or-fn))

(defn render
  ([img-or-fn]
   (show img-or-fn))
  ([img-or-fn size]
   (show img-or-fn :size size :width size :height size)))


(comment
  (show (functions/seamless 0.25 (compose patterns/plasma patterns/vsnoise)) :size 512))


(defn messy-noise
  "nicely scales the noise using the standard notion of lacunarity"
  [[x y] noise-coll]
  (let [[n0 n1 n2] noise-coll]
    (+
      (image->number n0 x y)
      (* 0.5 (image->number n1 x y))
      (* 0.25 (image->number n2 x y)))))

(defn messy-noise-2
  "nicely scales the noise using the standard notion of lacunarity"
  [[n0 n1 n2]]
  (compose
     n0
     (patterns/make-multi-fractal n1 {:octaves 1 :lacunarity 1 :gain 1 :scale 0.5})
     (patterns/make-multi-fractal n2 {:octaves 1 :lacunarity 1 :gain 1 :scale 0.25})))


(comment
  (def noise-1 (make-perlin-noise {:seed (inc (rand-int 10000)) :octaves 2}))
  (def noise-2 (make-perlin-noise {:seed 20 :octaves 2}))

  (def seed (reduce + (map (comp int char) "Fruit")))
  (patterns/seed-perlin-noise! seed)
  (patterns/seed-simplex-noise! seed)

  (def noise-1 (make-perlin-noise {:seed 10 :octaves 2}))
  (def noise-2 (make-perlin-noise {:seed 20 :octaves 2}))

  (show noise-1)
  (show noise-2)

  (functions/evaluate (functions/texture-map noise-1))

  (mapv (fn [f] (f (.getRGB noise-1 100 0))) [red-from-argb green-from-argb blue-from-argb alpha-from-argb])

  (show [functions/y functions/y 0])

  (clisk.node/node [functions/x functions/y 0]))
