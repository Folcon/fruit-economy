(ns fruit-economy.humble-ui
  (:require [clojure.stacktrace :as stacktrace]
            [io.github.humbleui.protocols :refer [IComponent -measure -draw -event]]
            [fruit-economy.utils :refer [resource-file->byte-array]])
  (:import [io.github.humbleui.skija Canvas Data ClipMode]
           [io.github.humbleui.types IPoint Point Rect]
           [io.github.humbleui.skija.svg SVGDOM SVGLengthContext SVGLengthType]))

;; Should be in io.github.humbleui.ui
(def *broken (atom false))

(defrecord UICanvas [width height on-paint on-event]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs canvas]
    (when on-paint
      (let [canvas ^Canvas canvas
            {:keys [width height]} cs
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (.clipRect canvas rect ClipMode/INTERSECT true)
          (try
            (when-not @*broken
              (on-paint canvas width height))
            (catch Exception e
              (reset! *broken true)
              (stacktrace/print-stack-trace (stacktrace/root-cause e))))
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]
    (when on-event
      (try
        (when-not @*broken
          (on-event event))
        (catch Exception e
          (reset! *broken true)
          (stacktrace/print-stack-trace (stacktrace/root-cause e)))))))

(defn ui-canvas
  "(ui-canvas 400 300 {:on-paint #'on-paint-impl
                       :on-event #'on-event-impl})"
  [width height {:keys [on-paint on-event]}]
  (UICanvas. width height on-paint on-event))

(defrecord SVGCanvas [width height svg-path svg-str on-event]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs canvas]
    (when (or svg-path svg-str)
      (let [^Canvas canvas canvas
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)
            bounds (Point. (- width 50) (- height 50))
            lc (SVGLengthContext. bounds)]
        (try
          (.clipRect canvas rect ClipMode/INTERSECT true)
          (try
            (when-not @*broken
              (let [data-byte-array (cond
                                      svg-path (resource-file->byte-array svg-path)
                                      svg-str (.getBytes svg-str))
                    data (Data/makeFromBytes data-byte-array)
                    svg-dom (SVGDOM. data)
                    root (.getRoot svg-dom)
                    svg-width (.resolve lc (.getWidth root) SVGLengthType/HORIZONTAL)
                    svg-height (.resolve lc (.getHeight root) SVGLengthType/VERTICAL)
                    _ (.setContainerSize svg-dom bounds)
                    scale (Math/min (/ (.getX bounds) svg-width) (/ (.getY bounds) svg-height))
                    {:keys [svg-x svg-y svg-z paint]} ctx
                    svg-x' (+ (/ (- width (* svg-width scale)) 2) svg-x)
                    svg-y' (+ (/ (- height (* svg-height scale)) 2) svg-y)
                    svg-z' (+ scale svg-z)]
                (.translate canvas svg-x' svg-y')
                (.drawRect canvas (Rect/makeWH (* svg-width scale) (* svg-height scale)) paint)
                (.scale canvas svg-z' svg-z')
                (.render svg-dom canvas)))
            (catch Exception e
              (reset! *broken true)
              (stacktrace/print-stack-trace (stacktrace/root-cause e))))
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]
    (when on-event
      (try
        (when-not @*broken
          (on-event event))
        (catch Exception e
          (reset! *broken true)
          (stacktrace/print-stack-trace (stacktrace/root-cause e)))))))

(defn svg-canvas
  "(svg-canvas 400 300 {:svg-path \"<path in resource>\"})"
  [width height {:keys [svg-path svg-str on-event]}]
  (SVGCanvas. width height svg-path svg-str on-event))
;; END Should be in io.github.humbleui.ui
