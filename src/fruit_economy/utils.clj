(ns fruit-economy.utils
  (:require [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream StringWriter]))


(defn resource-file->byte-array [path]
  (with-open [in (io/input-stream (io/resource path))
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn clamp [val {:keys [mn mx] :or {mn 0 mx 1}}]
  (max mn (min val mx)))

(defmacro suppress-print [& body]
  `(let []
     (binding [*out* (new StringWriter)]
       ~@body)))

(defn weighted-rand [m]
  "{:W 1 :U 10 :B 6 :R 10 :G 12}"
  (if (= (count m) 1)
    (ffirst m)
    (let [w (reductions #(+ % %2) (vals m))
          r (rand-int (last w))]
      (nth (keys m) (count (take-while #(<= % r) w))))))
