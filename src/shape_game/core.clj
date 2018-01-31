(ns shape-game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def width 800)
(def height 600)

(def rect-width (/ width 4))
(def rect-height (/ height 20))
(def rect-x-init (/ (- width rect-width) 2))
(def rect-y-init (- height rect-height 10))

(def ellipse-wh (/ rect-width 10))
(def ellipse-x-init (+ rect-x-init (/ rect-width 2)))
(def ellipse-y-init (- rect-y-init (/ ellipse-wh 2)) )

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:rect-x rect-x-init
   :rect-y rect-y-init
   :ellipse-x ellipse-x-init
   :ellipse-y ellipse-y-init})

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 240)
  (q/ellipse (:ellipse-x state) (:ellipse-y state) ellipse-wh ellipse-wh)
  (q/rect (:rect-x state) (:rect-y state) rect-width rect-height))

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
                   (:right) (update state :rect-x (partial + 4)) 
                   (:left) (update state :rect-x (fn [x] (- x 4)))
                   state)))
