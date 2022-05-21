(ns fruit-economy.state)


(defonce ^:dynamic *canvas-width* 2400)
(defonce ^:dynamic *canvas-height* 1200)


(defonce *menu (atom nil))

(defonce *world (atom nil))

(defonce *state (atom nil))

(def *selected-ui-view (atom nil))

(defonce *floating (atom false))
