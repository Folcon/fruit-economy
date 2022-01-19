(ns fruit-economy.rand)


(defn rand-between
  "Returns a random floating point number between 0 (inclusive) and
   n (default 1) (exclusive).
   If two parameters are provided, returns a random floating point number between aand b."
  {:added "1.0"    :static true}
  ([] (. Math (random)))
  ([n] (* n (rand-between)))
  ([a b] (+ a (rand-between (- b a)))))

(defn rand-int-between
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  If two parameters are provided, returns a random integer between a and b."
  {:added "1.0"    :static true}
  ([n] (int (rand-between n)))
  ([a b] (int (rand-between a b))))

(defn roll [size]
  (inc (rand-int size)))

(defn roll-1 [size]
  (rand-int-between 1 size))

(comment
  ;; Simple benchmarking to start, looks like stick to roll
  (time
    (dotimes [_ 100000000]
      (roll 100)))
  #_#_=> "Elapsed time: 1961.923092 msecs"

  (time
    (dotimes [_ 100000000]
      (roll-1 100)))
  #_#_=> "Elapsed time: 3949.292498 msecs")
