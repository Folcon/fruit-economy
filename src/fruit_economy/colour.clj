(ns fruit-economy.colour)


(defn colour
  "r g b should be between 0 and 255"
  [r g b]
  (unchecked-int
    (bit-or
      (unchecked-int 0xFF000000)
      (bit-shift-left r 16)
      (bit-shift-left g 8)
      (bit-shift-left b 0))))
