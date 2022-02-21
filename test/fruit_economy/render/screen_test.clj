(ns fruit-economy.render.screen-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [fruit-economy.db.core :as db]
            [fruit-economy.core :as core])
  (:import [io.github.humbleui.skija Surface Canvas Color4f FontMgr FontStyle Typeface Font Paint])
  (:import [io.github.humbleui.types IPoint IRect Rect]))


(def world
  (let [width  20
        height 25]
    (core/init-world "World" width height)))

(def world-db
  (let [db (d/empty-db {:area {:db/index true}
                        :land/history {:db/valueType :db.type/ref
                                       :db/cardinality :db.cardinality/many}
                        :land/resources {:db/valueType :db.type/ref
                                         :db/cardinality :db.cardinality/many}
                        :land/units {:db/valueType :db.type/ref
                                     :db/cardinality :db.cardinality/many}
                        :land/civs {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}
                        :civ/territory {:db/valueType :db.type/ref
                                        :db/cardinality :db.cardinality/many}
                        :civ/peeps {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}})

        world-db (db/db-bulk-insert db [world])]
    world-db))

(comment
  (let [zoom 1.0
        init-cell 20
        cell init-cell

        ;; size of the window
        window-width 640 window-height 480
        window-rect (Rect/makeXYWH 0 0 window-width window-height)

        ;; size of the window's content
        content-width 640 content-height 420
        content-rect (Rect/makeXYWH 0 0 content-width content-height)

        ;; the size of the canvas within the window's content
        viewport-width 600  viewport-height 400
        viewport-rect (Rect/makeXYWH 0 0 viewport-width viewport-height)

        world-width (quot viewport-width cell) world-height (quot viewport-height cell)


        [min-width min-height] [600 400]

        buffer (Surface/makeRasterN32Premul window-width window-height)
        canvas (.getCanvas buffer)]
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (with-open [w-fill (doto (Paint.) (.setColor (unchecked-int 0xFFCC3333)))
                c-fill (doto (Paint.) (.setColor (unchecked-int 0xFF33CC33)))
                v-fill (doto (Paint.) (.setColor (unchecked-int 0xFF3333CC)))]
      (.drawRect canvas window-rect w-fill)
      (.drawRect canvas content-rect c-fill)
      (.drawRect canvas viewport-rect v-fill))

    (doseq [x (range world-width)
            y (range world-height)
            :let [;; pixel-x and pixel-y
                  px-x (* x cell) px-y (* y cell)]]
      (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFFCC33CC)))]
        (.drawRect canvas (Rect/makeXYWH px-x px-y cell cell) fill)))

    (io/copy
      (-> buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png"))))
