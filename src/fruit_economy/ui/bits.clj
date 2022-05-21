(ns fruit-economy.ui.bits
  (:require [io.github.humbleui.ui :as ui]))


(def padding 4)

(declare show-map-ui)

(defn show-val-ui [v font fill lead-col?]
  (cond
    (instance? clojure.lang.Atom v) (show-val-ui @v font fill lead-col?)
    (vector? v) ((if lead-col? ui/column ui/row)
                 (interpose (ui/gap 2 padding) (mapv #(show-val-ui % font fill lead-col?) v)))
    (map? v) (show-map-ui v font fill (not lead-col?))
    :else (ui/label (pr-str v) {:font font :paint fill})))

(defn show-map-ui
  ([m font fill] (show-map-ui m font fill true))
  ([m font fill lead-col?]
   ((if lead-col? ui/column ui/row)
    (for [[k v] m]
      (ui/padding 5
        ((if lead-col? ui/row ui/column)
         (ui/label (str k) {:font font :paint fill})
         (ui/gap 10 5)
         (show-val-ui v font fill lead-col?)))))))
