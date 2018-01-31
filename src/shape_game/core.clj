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
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:rect-x rect-x-init})

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 240)
  (q/rect (:rect-x state) rect-y rect-width rect-height))

(q/defsketch shape-game
  :title "Shape game"
  :size [width height]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode]
  :key-pressed (fn [{:keys [rect-x] :as state} { :keys [key key-code] }]
                 (case key
                   (:right) {:rect-x (+ (:rect-x state) 4)}
                   (:left) {:rect-x (- (:rect-x state) 4)}
                   state)))
