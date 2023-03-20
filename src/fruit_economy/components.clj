(ns fruit-economy.components
  (:require [io.github.humbleui.paint :as paint]
            [io.github.humbleui.ui :as ui])
  (:import [io.github.humbleui.skija Canvas Paint PaintMode]))


(defn atom-checkbox [*checked text]
  (ui/clickable
    #(swap! *checked not)
    (ui/dynamic ctx [checked @*checked
                     {:keys [font-ui fill-text leading scale]} ctx]
      (let [border (paint/stroke (unchecked-int 0xFF000000) (* 1 scale))]
        (ui/row
          (ui/rect border
            (if checked
              (ui/padding 1 1
                (ui/rect (paint/fill fill-text)
                  (ui/gap (- leading 2) (- leading 2))))
              (ui/gap leading leading)))
          (ui/gap 5 0)
          (ui/label text {:font font-ui :paint fill-text}))))))
