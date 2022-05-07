(ns fruit-economy.humble-ui
  (:require [clojure.stacktrace :as stacktrace]
            [io.github.humbleui.protocols :refer [IComponent -measure -draw -event]]
            [io.github.humbleui.ui :as ui]
            [dali.io :refer [render-svg-string]]
            [dali.layout.stack])
  (:import [io.github.humbleui.skija Canvas Data ClipMode Surface]
           [io.github.humbleui.types IPoint IRect Rect]
           [io.github.humbleui.skija.svg SVGDOM]))

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

(defn svg-canvas
  ([svg-str] (svg-canvas svg-str nil))
  ([svg-str opts]
   (let [dom (with-open [data (Data/makeFromBytes (.getBytes svg-str))]
               (SVGDOM. data))
         scaling (ui/svg-opts->scaling opts)]
     (ui/->SVG dom scaling nil))))

(defn svg [doc]
  (svg-canvas (render-svg-string doc)))
;; END Should be in io.github.humbleui.ui

(def render-buffer (atom {}))

(defn make-named-buffer [name requested-width requested-height]
  (let [{:keys [width height]} (get @render-buffer name)]
    (when-not (and (= width requested-width) (= height requested-height))
      (let [surface (Surface/makeRasterN32Premul requested-width requested-height)
            canvas (.getCanvas surface)]
        (swap! render-buffer assoc name {:width requested-width
                                         :height requested-height
                                         :surface surface
                                         :canvas canvas
                                         :image nil})))))

(defn get-named-canvas [name]
  (get-in @render-buffer [name :canvas]))

(defn update-named-buffer [name]
  (swap! render-buffer assoc-in [name :image] (.makeImageSnapshot (get-in @render-buffer [name :surface]))))

(defn draw-named [name canvas]
  (when-let [image (get-in @render-buffer [name :image])]
    (.drawImage canvas image 0 0)))

(comment
  (do
    (require '[clojure.java.io :as io])
    (import '[io.github.humbleui.skija Surface Paint]))

  ;; Testing
  (let [svg-str "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"620\" height=\"472\"><text font-family=\"DejaVu Sans\" stroke-width=\"4\" x=\"310\" y=\"174.47\" font-size=\"180\">TEST</text></svg>"
        data-bytes (.getBytes svg-str)
        data (Data/makeFromBytes data-bytes)
        svg-dom (SVGDOM. data)
        front-buffer (Surface/makeRasterN32Premul 640 360)
        back-buffer (Surface/makeRasterN32Premul 640 360)
        front-canvas (.getCanvas front-buffer)
        back-canvas (.getCanvas back-buffer)

        render-svg? true]
    ;; clear canvas
    (doto back-canvas
      (.clear (unchecked-int 0xFFFFFFFF)))

    (if render-svg?
      ;; render svg
      (.render svg-dom back-canvas)

      ;; draw stuff
      (.drawCircle back-canvas 320 217 16 (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))))

    ;; copy across
    (.drawImage front-canvas (.makeImageSnapshot back-buffer) 0 0)

    (io/copy
      (-> front-buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png"))))

;; Should be in io.github.humbleui.ui
(defn fragment [& children]
  (list children))

(def <> #'fragment)

