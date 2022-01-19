(ns fruit-economy.gen-land
  (:require [fruit-economy.noise :refer [make-terrain make-perlin-noise messy-noise image->number scale v* compose gradient render]]))


(def width 5 #_50)
(def height 2 #_20)

(def temp-noise
  [(make-perlin-noise {:seed (inc (rand-int 10000)) :octaves 2})
   (make-perlin-noise {:seed (inc (rand-int 10000)) :octaves 4})
   (make-perlin-noise {:seed (inc (rand-int 10000)) :octaves 88})])

(def messy-temp-noise
  (compose
    (nth temp-noise 0)
    (v* (nth temp-noise 1) 0.5)
    (v* (nth temp-noise 2) 0.25)))

(render (scale (first temp-noise) 4))

(render (apply compose temp-noise))
(render messy-temp-noise 512)
(render (fruit-economy.noise/messy-noise-2 temp-noise))

(render (fruit-economy.noise/gradient messy-temp-noise))

(dotimes [_ 10]
  (messy-noise [2 5] temp-noise))

(messy-noise [2 5] temp-noise)
(image->number (apply compose temp-noise) 2 5)
(image->number messy-temp-noise 2 5)
(image->number (fruit-economy.noise/messy-noise-2 temp-noise) 2 5)

(for [w (range width)
      h (range height)]
  [w h])

(comment
  (let [temp-noise
        [(make-perlin-noise {:seed 1 :octaves 2})
         (make-perlin-noise {:seed 2 :octaves 4})
         (make-perlin-noise {:seed 3 :octaves 88})]
        [a b c] temp-noise]
    (render (v* a 1.25) #_(apply compose [(v* a 1.25) (v* b 2) (v* c 4)]))))

