(ns fruit-economy.screen-ui
  (:require [clojure.string :as str]
            [io.github.humbleui.ui :as ui]
            [fruit-economy.data.core :as data]
            [fruit-economy.sim.basic :as basic]
            [fruit-economy.humble-ui :as custom-ui]))


(def *chart (atom {:index 0}))

(defn display-chart [width height ys]
  (ui/dynamic ctx [width width height height ys ys
                   {:keys [index]} @*chart]
    (let [page-attrs {:xmlns "http://www.w3.org/2000/svg"
                      :version "1.2"
                      :width width
                      :height height
                      :view-box (str/join " " (map str [0 0 width height]))}
          mx (apply max ys)
          limit-fn (fn [val]
                     (reduce
                       (fn [val [limit div]]
                         (if (zero? (quot val limit))
                           val
                           (reduced div)))
                       val
                       [[1000 100] [100 10] [10 nil]]))
          apply-limit-fn (fn [val limit] (if limit (float (/ val limit)) val))
          ;;limit (limit-fn mx)
          scale-fn (comp int #(Math/ceil %) (partial * (- height 26)) #(apply-limit-fn % mx))
          ;;_ (println :limit (mapv scale-fn xs))
          coll [0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361]
          size (count ys)
          max-entries (min size #_500 1080)
          data (take max-entries (cycle coll))
          ;; Have chart be able to zoom in and out... with swapping to doc-2 chart when zoomed in and doc-1 zoomed out.
          #_#_ ;; use real data
          ys (mapv (partial + 20) data)
          window 22
          step 1 #_10 ;; effectively chart zoom
          x-offset 4
          xs (range x-offset (* (+ (inc size) x-offset) step) step)
          _ (println :ys ys :xs xs :h (mapv scale-fn ys) :-h (mapv (comp (partial - height) scale-fn) ys))
          doc-1 [:dali/page page-attrs
                 [:rect {:stroke :none :fill :orchid} [(+ x-offset index) 0] [window 100]]
                 [:line {:stroke {:paint :red :width 4}} [0 0] [0 100]]
                 [:polyline {:fill :none :stroke :black} (map #(vector %1 %2) xs (mapv (comp (partial - height) scale-fn) ys))]]
          ys' (if (seq ys)
                (subvec ys index (min (+ window index) max-entries))
                ys)
          doc-2 [:dali/page page-attrs
                 (when (seq ys')
                   [:dali/stack
                    {:position [10 10] :direction :right :anchor :bottom-left :gap 2}
                    (map (fn [h]
                           [:dali/stack
                            {:direction :up :gap 6}
                            [:rect {:stroke :none :fill :darkorchid} :_ [20 (scale-fn h)]]
                            [:text {:text-family "Verdana" :font-size 12} (str h)]])
                      ys')])]]
      (ui/dynamic ctx [{:keys [fill-dark-gray fill-light-gray]} ctx
                       doc-1 doc-1
                       doc-2 doc-2]
        (ui/column
          (ui/fill fill-dark-gray
            (ui/row
              (ui/fill fill-light-gray
                (ui/height height
                  (ui/width width
                    (custom-ui/svg doc-1))))
              (ui/gap 2 0)
              (ui/fill fill-light-gray
                (ui/height height
                  (ui/width width
                    (custom-ui/svg doc-2))))
              (ui/column
                (ui/button
                  #(swap! *chart update :index (fn [x] (max (- x 7) 0)))
                  {:border-radius 0}
                  (ui/label "< 7x"))
                (ui/button
                  #(swap! *chart update :index (fn [x] (max (dec x) 0)))
                  {:border-radius 0}
                  (ui/label "<="))
                #_#_
                (ui/button
                  #(println "SS")
                  {:border-radius 0}
                  (ui/label "+"))
                (ui/button
                  #(println "SS")
                  {:border-radius 0}
                  (ui/label "-"))
                (ui/button
                  #(swap! *chart update :index (fn [x] (min (inc x) max-entries)))
                  {:border-radius 0}
                  (ui/label "=>"))
                (ui/button
                  #(swap! *chart update :index (fn [x] (min (+ x 7) max-entries)))
                  {:border-radius 0}
                  (ui/label "7x >"))))))))))

(def price-chart-ui
  (ui/dynamic ctx [{:keys [price price-history fill-green]} ctx]
    (ui/padding 20
      (ui/column
        (ui/label (str "Current Price:            " price " g"))
        (ui/gap 0 5)
        (if-not (seq price-history)
          (ui/gap 0 0)
          (display-chart 500 100 price-history))))))
