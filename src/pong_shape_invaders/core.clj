(ns pong-shape-invaders.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]))

(def width 800)
(def height 600)
(def background-color 0)
(def fps 60)

(def rect-width (/ width 4))
(def rect-height (/ height 20))
(def rect-x-init (/ (- width rect-width) 2))
(def rect-y-init (- height rect-height 10))
(def rect-x-step 6)
(def rect-x-speed-init 0)
(def rect-dir :none)

(def ellipse-wh (/ rect-width 10))
(def ellipse-r (/ ellipse-wh 2))
(def ellipse-x-init (+ rect-x-init (/ rect-width 2)))
(def ellipse-y-init (- rect-y-init (/ ellipse-wh 2)))
(def ellipse-y-step 10)
(def ellipse-x-speed-init 0)
(def ellipse-diagonal-step 2)

(def enemy-diameter 30)

(defn generate-enemies-shape-state-in-y [y]
  (->>
   (range enemy-diameter
          (* 26 enemy-diameter)
          (* 2 enemy-diameter))
   (map (fn [x] {:x x :y y :dir (rand-int 2)}))))

(defn generate-enemies-shape-state []
  (concat
   (generate-enemies-shape-state-in-y 40)
   (generate-enemies-shape-state-in-y 120)
   (generate-enemies-shape-state-in-y 200)))

(def init-state
  (let [enemies-shape-state (generate-enemies-shape-state)]
    {:rect-x rect-x-init
     :rect-x-speed rect-x-speed-init
     :rect-y rect-y-init
     :rect-dir rect-dir
     :ellipse-x ellipse-x-init
     :ellipse-x-speed ellipse-x-speed-init
     :ellipse-sign-x +
     :ellipse-sign-y -
     :ellipse-y ellipse-y-init
     :enemies-shape-state enemies-shape-state
     :score 0
     :enemies-total (count enemies-shape-state)}))

(defn setup []
  (q/frame-rate fps)
  init-state)

(defn is-ellipse-hit-rect? [state]
  (< (:rect-x state)
     (:ellipse-x state)
     (+ (:rect-x state) rect-width)))

(defn move-ellipse-x-diagonal [state]
  (update state
          :ellipse-x
          (fn [x]
            ((:ellipse-sign-x state)
             x
             (:ellipse-x-speed state)))))

(defn update-ellipse-state [state]
  (if (> (:ellipse-y state) width)
    init-state 
    (->
     (cond
       (= (:ellipse-y state) (/ ellipse-wh 2))
       (->
        state
        (update :rect-x-speed (fn [_] ellipse-diagonal-step))
        (update :ellipse-sign-y (fn [_] +)))
       (and (is-ellipse-hit-rect? state)
            (= (:ellipse-y state) (+ (- rect-y-init rect-height) ellipse-wh))) 
       (->
        state
        (update :ellipse-x-speed (fn [_] (:rect-x-speed state)))
        (update :ellipse-sign-y (fn [_] -))
        (update :ellipse-sign-x (fn [y] (if (= :left (:rect-dir state)) + -))))
       (= (:ellipse-x state) (- width ellipse-wh))
       (->
        state
        (update :ellipse-sign-x (fn [_] -))
        move-ellipse-x-diagonal)
       (= (:ellipse-x state) ellipse-wh) 
       (->
        state
        (update :ellipse-sign-x (fn [_] +))
        move-ellipse-x-diagonal)
       :else
       (move-ellipse-x-diagonal state))
     (update :ellipse-y (fn [y] ((:ellipse-sign-y state) y ellipse-y-step))))))

(defn update-rect-state [state]
  (cond
    (>= (:rect-x state) (- width rect-width))
    (update state :rect-x (fn [x] (- x rect-x-step)))
    (<= (:rect-x state) 0)
    (update state :rect-x (fn [x] (+ x rect-x-step)))
    :else
    state))

(defn is-ellipse-and-enemy-point-collide?
  [state enemy-state ellipse-point-keyword enemy-point-keyword]
  (> (count
      (set/intersection
       (set
        (range
         (- (ellipse-point-keyword state) ellipse-r)
         (+ 1 (+ (ellipse-point-keyword state) ellipse-r))))
       (set
        (range
         (enemy-point-keyword enemy-state)
         (+ 1 (+ (enemy-point-keyword enemy-state) enemy-diameter))))))
     0))

(defn update-enemies-state [state]
  (let [enemies-shape-state (:enemies-shape-state state)
        total-enemies (count enemies-shape-state)
        enemies-state-alive
        (->>
         (remove
          (fn [enemy-state]
            (and
             (is-ellipse-and-enemy-point-collide? state enemy-state :ellipse-y :y)
             (is-ellipse-and-enemy-point-collide? state enemy-state :ellipse-x :x)))
          enemies-shape-state)
         (map (fn [enemy-state]
                (let [x (:x enemy-state)
                      dir (:dir enemy-state)
                      y (:y enemy-state)
                      rand-step (rand-int 4)
                      new-enemy-state
                      (if (= 0 (:dir enemy-state))
                        {:x (- x rand-step)
                         :y y
                         :dir 1}
                        {:x (+ x rand-step)
                         :y y
                         :dir 0})]
                  (cond
                    (>= (+ (:x new-enemy-state) enemy-diameter) width)
                    (update new-enemy-state :x (fn [_] (- width enemy-diameter)))
                    (<= (:x new-enemy-state) 0)
                    (update new-enemy-state :x (fn [_] 0))
                    :else
                    new-enemy-state)))))
        new-total-enemies (count enemies-state-alive)
        delta-enemies (- total-enemies new-total-enemies)]    
    (if (= 0 delta-enemies)
      (update state :enemies-shape-state (fn [_] enemies-state-alive))
      (->
       (update state :enemies-shape-state (fn [_] enemies-state-alive))
       (update :score
               #(+ % (* (- total-enemies new-total-enemies) 10)))
       (update :enemies-total #(- % delta-enemies))))))

(defn update-state [state]
  (->
   state
   update-ellipse-state
   update-rect-state
   update-enemies-state))

(defn draw-state [state]
  (q/background background-color)
  (q/fill 131 131 131)
  (q/rect (:rect-x state) (:rect-y state) rect-width rect-height)
  (q/fill 0 248 255)
  (q/ellipse (:ellipse-x state) (:ellipse-y state) ellipse-wh ellipse-wh)
  (q/text-size 20)
  (q/fill 255)
  (q/text (str "Score : " (:score state)) 20 20)
  (q/text (str "Enemies Total : " (:enemies-total state)) (- width 200) 20)
  (doseq [p (:enemies-shape-state state)]
    (q/fill (rand-int 256) 120 (rand-int 256))
    (q/rect (:x p) (:y p) enemy-diameter enemy-diameter)))

(q/defsketch shape-game
  :title "Shape game"
  :size [width height]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode]
  :key-pressed (fn [{:keys [rect-x] :as state} { :keys [key key-code] }]
                 (->
                  (case key
                   (:right)
                   (update state :rect-x (partial + rect-x-step))
                   (:left)
                   (update state :rect-x (fn [x] (- x rect-x-step)))
                   state)
                  (update :rect-dir (fn [_] key)))))