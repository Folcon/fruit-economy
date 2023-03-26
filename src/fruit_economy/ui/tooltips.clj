(ns fruit-economy.ui.tooltips
  (:require [io.github.humbleui.paint :as paint]
            [io.github.humbleui.ui :as ui]

            [io.github.humbleui.core :as core]
            [io.github.humbleui.protocols :as protocols]
            [io.github.humbleui.ui.align :as align]
            [io.github.humbleui.ui.dynamic :as dynamic]
            [io.github.humbleui.ui.hoverable :as hoverable]
            [io.github.humbleui.ui.gap :as gap]))



(def *tooltips (atom {}))

(core/deftype+ RelativeRect [relative opts]
  :extends core/AWrapper
  protocols/IComponent
  (-draw [_ ctx rect ^Canvas canvas]
    (let [{:keys [left up anchor shackle]
           :or {left 0 up 0
                anchor :top-left shackle :top-right}} opts
          child-size    (core/measure child ctx (core/ipoint (:width rect) (:height rect)))
          child-rect    (core/irect-xywh (:x rect) (:y rect) (:width child-size) (:height child-size))
          rel-cs        (core/measure relative ctx (core/ipoint 0 0))
          rel-cs-width  (:width rel-cs)
          rel-cs-height (:height rel-cs)
          rel-rect      (condp = [anchor shackle]
                          [:top-left :top-left]         (core/irect-xywh (- (:x child-rect) left) (- (:y child-rect) up) rel-cs-width rel-cs-height)
                          [:top-right :top-left]        (core/irect-xywh (- (:x child-rect) rel-cs-width left) (- (:y child-rect) up) rel-cs-width rel-cs-height)
                          [:bottom-right :top-left]     (core/irect-xywh (- (:x child-rect) rel-cs-width left) (- (:y child-rect) rel-cs-height up) rel-cs-width rel-cs-height)
                          [:bottom-left :top-left]      (core/irect-xywh (- (:x child-rect) left) (- (:y child-rect) rel-cs-height up) rel-cs-width rel-cs-height)
                          [:top-left :top-right]        (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) left)) (- (:y child-rect) up) rel-cs-width rel-cs-height)
                          [:top-right :top-right]       (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) rel-cs-width left)) (- (:y child-rect) up) rel-cs-width rel-cs-height)
                          [:bottom-left :top-right]     (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) left)) (- (:y child-rect) rel-cs-height up) rel-cs-width rel-cs-height)
                          [:bottom-right :top-right]    (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) rel-cs-width left)) (- (:y child-rect) rel-cs-height up) rel-cs-width rel-cs-height)
                          [:top-left :bottom-right]     (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) left)) (+ (:y child-rect) (- (:height child-rect) up)) rel-cs-width rel-cs-height)
                          [:top-right :bottom-right]    (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) rel-cs-width left)) (+ (:y child-rect) (- (:height child-rect) up)) rel-cs-width rel-cs-height)
                          [:bottom-right :bottom-right] (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) rel-cs-width left)) (+ (:y child-rect) (- (:height child-rect) rel-cs-height up)) rel-cs-width rel-cs-height)
                          [:bottom-left :bottom-right]  (core/irect-xywh (+ (:x child-rect) (- (:width child-rect) left)) (+ (:y child-rect) (- (:height child-rect) rel-cs-height up)) rel-cs-width rel-cs-height)
                          [:top-left :bottom-left]      (core/irect-xywh (- (:x child-rect) left) (+ (:y child-rect) (- (:height child-rect) up)) rel-cs-width rel-cs-height)
                          [:top-right :bottom-left]     (core/irect-xywh (- (:x child-rect) rel-cs-width left) (+ (:y child-rect) (- (:height child-rect) up)) rel-cs-width rel-cs-height)
                          [:bottom-left :bottom-left]   (core/irect-xywh (- (:x child-rect) left) (+ (:y child-rect) (- (:height child-rect) rel-cs-height up)) rel-cs-width rel-cs-height)
                          [:bottom-right :bottom-left]  (core/irect-xywh (- (:x child-rect) rel-cs-width left) (+ (:y child-rect) (- (:height child-rect) rel-cs-height up)) rel-cs-width rel-cs-height))]
      (core/draw-child child ctx child-rect canvas)
      (if-let [id (:id opts)]
        (swap! *tooltips assoc id #(core/draw-child relative ctx rel-rect canvas))
        (core/draw-child relative ctx rel-rect canvas)))))


(defn relative-rect
  ([relative child]
   (relative-rect {} relative child))
  ([opts relative child]
   (map->RelativeRect
     {:relative relative
      :opts     opts
      :child    child})))

(defn tooltip
  ([tip child] (tooltip {} tip child))
  ([opts tip child]
   (align/valign 0
     (align/halign 0
       (hoverable/hoverable
         (dynamic/dynamic ctx [{:hui/keys [active? hovered?]} ctx]
           (let [tip' (cond
                        active?  tip
                        hovered? tip
                        :else    (gap/gap 0 0))]
             (relative-rect opts tip' child))))))))



(def ui-view
  (ui/stack
    (ui/row
      (ui/center
        (ui/row
          (ui/stack
            (ui/center
              (tooltip {:id :a}
                (ui/center
                  (ui/rect (paint/fill 0x8033CC33)
                    (ui/padding 100
                      (ui/label "TOOLTIP!!"))))
                (ui/center
                  (ui/rect (paint/fill 0x803333CC)
                    (ui/gap 300 100))))))
          (ui/center
            (ui/rect (paint/fill 0xFFCCCCCC)
              (ui/gap 200 200))))))
    (ui/dynamic _ [tooltips @*tooltips]
      (do
        (doseq [[_id tooltip-fn] tooltips]
          (tooltip-fn))
        (ui/gap 0 0)))))
