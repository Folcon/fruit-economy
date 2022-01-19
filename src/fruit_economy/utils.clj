(ns fruit-economy.utils
  (:require [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream]))


(defn resource-file->byte-array [path]
  (with-open [in (io/input-stream (io/resource path))
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn clamp [val {:keys [mn mx] :or {mn 0 mx 1}}]
  (max mn (min val mx)))
