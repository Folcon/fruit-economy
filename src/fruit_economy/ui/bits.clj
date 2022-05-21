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

(defn camera->viewport [camera zoom content-width content-height]
  (let [w content-width h content-height
        vp 0.1 wv (* w vp zoom) hv (* h vp zoom)
        cell (int (* 10 zoom))
        width (quot content-width (* cell 2)) height (quot content-height (* cell 2)) #_(int (quot zoom 1.25))
        half-x (quot width 2) half-y (quot height 2)]
    {:w w :h h :wv wv :hv hv :cell cell :width width :height height :half-x half-x :half-y half-y :size [width height] :center [half-x half-y :+ camera] :lrtb [(- (first camera) half-x) (+ (first camera) half-x) (- (second camera) half-y) (+ (second camera) half-y)]}))

(defn nested-limit
  ([coll limit] (nested-limit coll limit nil))
  ([coll limit elide-str]
   (let [cond-add-elide (fn [v] (if elide-str (conj v elide-str) v))]
     (reduce
       (fn [[rem v] item]
         (let [size (count item)
               rem' (- rem size)]
           (cond
             (< rem size) (reduced (conj v (cond-add-elide (into [] (take rem) item))))
             (> rem' 0) [rem' (conj v item)]
             (zero? rem') (reduced (conj v item)))))
       [limit []]
       coll))))

(comment
  ;; Can turn to tests later...
  (let [;; 3 => [[1 2] [3]]
        ;; 4 => [[1 2] [3 4]]
        coll [[1 2] [3 4] [5 6]]
        coll' [[1 2] [3 4 5 6 7 8 9 10]]]
    (and
      (= [[1 2] [3]]
        (nested-limit coll 3))
      (= [[1 2] [3 4]]
        (nested-limit coll 4))
      (= [[1 2] [3 4]]
        (nested-limit coll' 4)))))
