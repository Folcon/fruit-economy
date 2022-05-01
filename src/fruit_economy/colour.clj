(ns fruit-economy.colour)


(defn colour
  "an unchecked-int or r g b should be between 0 and 255"
  ([uint] (unchecked-int uint))
  ([r g b]
   (unchecked-int
     (bit-or
       (unchecked-int 0xFF000000)
       (bit-shift-left r 16)
       (bit-shift-left g 8)
       (bit-shift-left b 0)))))

(defn colour-noise
  "turns noise into an r, g, or b value for use with colour"
  [noise]
  (if noise (int (quot (* (+ noise 1) 255) 2)) 0))
