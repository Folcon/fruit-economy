(ns fruit-economy.screen-ui
  (:require [clojure.string :as str]
            [io.github.humbleui.ui :as ui]
            [fruit-economy.data.core :as data]
            [fruit-economy.sim.basic :as basic]
            [fruit-economy.humble-ui :as custom-ui]))


;; TODO: Create a UI element + add button when nil to select player
(def player-stats-ui
  (ui/dynamic ctx [{:keys [font-small fill-black world-db player-eid]} ctx]
    (if player-eid
      (let [player (data/entity world-db player-eid)]
        (ui/row
          (ui/padding 10 10
            (ui/label (str "Nation " (get-in player [:governs :settlement/name])) {:font font-small :paint fill-black}))
          (ui/padding 10 10
            (ui/label (str "Money $" (:money player)) {:font font-small :paint fill-black}))))
      (ui/gap 0 0))))

(def top-bar-ui
  (ui/dynamic ctx [{:keys [font-small fill-white fill-black fill-yellow day]} ctx]
    (ui/column
      [:stretch 1
       (ui/padding 0 0 0 10
         (ui/fill fill-yellow
           (ui/row
             (ui/padding 10 10
               (ui/label (str "Day " day) {:font font-small :paint fill-black}))
             player-stats-ui
             [:stretch 1 nil]
             (ui/fill fill-white
               (ui/clickable
                 #(reset! basic/*world (basic/reset-world))
                 (ui/padding 10 10
                   (ui/label "↻ Restart" {:font font-small :paint fill-black})))))))])))

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
          max-entries #_500 1080
          data (take max-entries (cycle coll))
          ;; Have chart be able to zoom in and out... with swapping to doc-2 chart when zoomed in and doc-1 zoomed out.
          ys (mapv (partial + 20) data)
          size (count ys)
          window 22
          step 1 #_10 ;; effectively chart zoom
          x-offset 4
          xs (range x-offset (* (+ (inc size) x-offset) step) step)
          _ (println :ys ys :xs xs :h (mapv scale-fn ys) :-h (mapv (comp (partial - height) scale-fn) ys))
          doc-1 [:dali/page page-attrs
                 [:rect {:stroke :none :fill :orchid} [(+ x-offset index) 0] [window 120]]
                 [:line {:stroke {:paint :red :width 4}} [0 0] [0 100]]
                 [:polyline {:fill :none :stroke :black} (map #(vector %1 %2) xs (mapv (comp (partial - height) scale-fn) ys))]]
          ys' (subvec ys index (+ window index))
          doc-2 [:dali/page page-attrs
                 [:dali/stack
                  {:position [10 10] :direction :right :anchor :bottom-left :gap 2}
                  (map (fn [h]
                         [:dali/stack
                          {:direction :up :gap 6}
                          [:rect {:stroke :none :fill :darkorchid} :_ [20 (scale-fn h)]]
                          [:text {:text-family "Verdana" :font-size 12} (str h)]])
                    ys')]]]
      (ui/dynamic ctx [{:keys [fill-dark-gray fill-light-gray]} ctx
                       doc-1 doc-1
                       doc-2 doc-2]
        (ui/column
          (ui/row
            (ui/fill fill-dark-gray
              (ui/height
                height
                (ui/width
                  width
                  (custom-ui/svg doc-1))))
            (ui/fill fill-light-gray
              (ui/height
                height
                (ui/width
                  width
                  (custom-ui/svg doc-2))))
            (ui/column
              [:stretch 1
               (ui/button
                 #(swap! *chart update :index (fn [x] (max (dec x) 0)))
                 {:border-radius 0}
                 (ui/label "<="))]
              (ui/gap 0 10)
              #_#_
              (ui/button
                #(println "SS")
                {:border-radius 0}
                (ui/label "+"))
              (ui/button
                #(println "SS")
                {:border-radius 0}
                (ui/label "-"))
              [:stretch 1
               (ui/button
                 #(swap! *chart update :index (fn [x] (min (inc x) max-entries)))
                 {:border-radius 0}
                 (ui/label "=>"))])))))))

(def price-chart-ui
  (ui/dynamic ctx [{:keys [price price-history fill-green]} ctx]
    (ui/padding 20
      (ui/column
        (ui/label (str "Current Price:            " price " g"))
        (ui/fill fill-green
          (if-not (seq price-history)
            (ui/label (pr-str price-history))
            (display-chart 500 100 (mapv #(* % %) (range 20)) #_price-history)))))))
