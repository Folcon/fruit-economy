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

(defn display-chart [width height ys]
  (ui/dynamic ctx [width width height height ys ys]
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
          ys (mapv (partial + 20) [0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361])
          size (count ys)
          step 10
          xs (range 0 (* (inc size) step) step)
          _ (println :ys ys :xs xs :h (mapv scale-fn ys) :-h (mapv (comp (partial - height) scale-fn) ys))
          doc-1 [:dali/page page-attrs
                 [:line {:stroke {:paint :red :width 4}} [0 0] [0 100]]
                 [:polyline {:fill :none :stroke :black} (map #(vector %1 %2) xs (mapv (comp (partial - height) scale-fn) ys))]
                 #_[:dali/stack
                    {:position [10 10] :direction :right :anchor :bottom-left :gap 2}
                    #_[:path
                       {:id :path :fill :none :stroke :black}
                       :M [110 80]
                       :C [140 10] [165 10] [195 80]
                       :C [225 150] [250 150] [280 80]]
                    [:polyline {:fill :none :stroke :black} (map #(vector %1 %2) xs (cycle [620 700]))]]]
          step 10
          init-x 100
          xs (range init-x (+ (* (inc size) step) init-x) step)
          doc-2 [:dali/page page-attrs
                 [:dali/stack
                  {:position [10 10] :direction :right :anchor :bottom-left :gap 2}
                  (map (fn [h]
                         [:dali/stack
                          {:direction :up :gap 6}
                          [:rect {:stroke :none :fill :darkorchid} :_ [20 (scale-fn h)]]
                          [:text {:text-family "Verdana" :font-size 12} (str h)]])
                    xs)]]]
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
                  (custom-ui/svg doc-2))))))))))

(def price-chart-ui
  (ui/dynamic ctx [{:keys [price price-history fill-green]} ctx]
    (ui/padding 20
      (ui/column
        (ui/label (str "Current Price:            " price " g"))
        (ui/fill fill-green
          (if-not (seq price-history)
            (ui/label (pr-str price-history))
            (display-chart 500 100 (mapv #(* % %) (range 20)) #_price-history)))))))
