(ns fruit-economy.render.screen-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [fruit-economy.db.core :as db]
            [fruit-economy.land :as land]
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
        viewport-width 604  viewport-height 404
        viewport-rect (Rect/makeXYWH 0 0 viewport-width viewport-height)

        viewport-offset 2

        world-width (quot viewport-width cell) world-height (quot viewport-height cell)
        half-ww (quot world-width 2) half-wh (quot world-height 2)


        [min-width min-height] [600 400]

        tile-color (unchecked-int 0xFFCC33CC)

        world-db world-db
        terrain (db/q '[:find ?v . :where [?e :fruit-economy.land/terrain ?v]] world-db)

        buffer (Surface/makeRasterN32Premul window-width window-height)
        canvas (.getCanvas buffer)
        [camera-x camera-y] [10 12]]
    (println (pr-str terrain))
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
                  px-x (+ (* x cell) viewport-offset) px-y (+ (* y cell) viewport-offset)

                  loc-x (+ (int (- x half-ww)) camera-x)
                  loc-y (+ (int (- y half-wh)) camera-y)
                  loc [loc-x loc-y]
                  path [loc-y loc-x]
                  biome (get-in terrain path)

                  tile-colour (land/render-tile-colour biome)]]
      (with-open [fill (doto (Paint.) (.setColor tile-colour))]
        (.drawRect canvas (Rect/makeXYWH px-x px-y cell cell) fill)))

    (io/copy
      (-> buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png"))))
