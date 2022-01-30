(ns fruit-economy.humble-ui
  (:require [clojure.stacktrace :as stacktrace]
            [io.github.humbleui.ui :refer [IComponent]]
            [fruit-economy.utils :refer [resource-file->byte-array]])
  (:import [io.github.humbleui.skija Canvas Data ClipMode]
           [io.github.humbleui.types IPoint Rect]
           [io.github.humbleui.skija.svg SVGDOM]))

;; Should be in io.github.humbleui.ui
(def *broken (atom false))

(defrecord UICanvas [width height on-paint on-event]
  IComponent
  (-layout [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx canvas]
    (when on-paint
      (let [canvas ^Canvas canvas
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
  (-layout [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx canvas]
    (when (or svg-path svg-str)
      (let [canvas ^Canvas canvas
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (.clipRect canvas rect ClipMode/INTERSECT true)
          (try
            (when-not @*broken
              (let [data-byte-array (cond
                                      svg-path (resource-file->byte-array svg-path)
                                      svg-str (.getBytes svg-str))
                    data (Data/makeFromBytes data-byte-array)
                    svg-dom (SVGDOM. data)]
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
