(ns shape-game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def width 500)
(def height 500)
(def rect-width (/ width 4))
(def rect-height (/ height 10))
(def rect-y (- height rect-height 10))
(def rect-x-init (/ (- width rect-width) 2))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  {:rect-x 0})

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 240)
  (q/rect rect-x-init rect-y rect-width rect-height))

(q/defsketch shape-game
  :title "Shape game"
  :size [width height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
