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
(def rect-x-step 6)
(def rect-x-init (/ (- width rect-width) 2))
(def rect-y-init (- height rect-height 10))

(def ellipse-diameter (/ rect-width 10))
(def ellipse-radius (/ ellipse-diameter 2))
(def ellipse-y-step 10)
(def ellipse-diagonal-step 2)

(def enemy-diameter 30)

(defn generate-enemies-shape-state-in-y [y]
  (->>
   (range enemy-diameter
          (* 26 enemy-diameter)
          (* 2 enemy-diameter))
   (map (fn [x] {:x x :y y :dir (rand-int 2)}))))

(defn generate-enemies-shape-state [level]
  (case level
    0 (generate-enemies-shape-state-in-y 40)
    1 (concat
       (generate-enemies-shape-state-in-y 40)
       (generate-enemies-shape-state-in-y 120)) 
    (concat
     (generate-enemies-shape-state-in-y 40)
     (generate-enemies-shape-state-in-y 120)
     (generate-enemies-shape-state-in-y 200))))

(defn init-state [level]
  (let [enemies-shape-state (generate-enemies-shape-state level)]
    {:rect {:x rect-x-init
            :y rect-y-init
            :dir :none
            :x-speed 0}
     :ellipse {:x (+ rect-x-init (/ rect-width 2))
               :y (- rect-y-init (/ ellipse-diameter 2))
               :x-speed 0
               :x-sign +
               :y-sign -}
     :enemies enemies-shape-state
     :score 0
     :enemies-total (count enemies-shape-state)
     :is-paused? false
     :level level}))

(defn setup []
  (q/frame-rate fps)
  (init-state 0))

(defn is-ellipse-hit-rect? [state]
  (< (get-in state [:rect :x])
     (get-in state [:ellipse :x])
     (+ (get-in state [:rect :x]) rect-width)))

(defn move-ellipse-x-diagonal [state]
  (update-in state
             [:ellipse :x] 
             (fn [x]
               ((get-in state [:ellipse :x-sign])
                x
                (get-in state [:ellipse :x-speed])))))

(defn update-ellipse-state [state]
  (if (> (get-in state [:ellipse :y]) width)
    (init-state (:level state)) 
    (->
     (cond
       (<= (get-in state [:ellipse :y]) (/ ellipse-diameter 2)) 
       (->
        state
        (update-in [:rect :x-speed] (fn [_] ellipse-diagonal-step))
        (update-in [:ellipse :y-sign] (fn [_] +)))
       (and (is-ellipse-hit-rect? state)
            (= (get-in state [:ellipse :y])
               (+ (- rect-y-init rect-height) ellipse-diameter)))
       (->
        state
        (update-in [:ellipse :x-speed]
                   (fn [_] (get-in state [:rect :x-speed])))
        (update-in [:ellipse :y-sign] (fn [_] -))
        (update-in
         [:ellipse :x-sign]
         (fn [y] (if (= :left (get-in state [:rect :dir])) + -))))
       (>= (get-in state [:ellipse :x]) (- width ellipse-diameter))
       (->
        state
        (update-in [:ellipse :x-sign] (fn [_] -))
        move-ellipse-x-diagonal)
       (= (get-in state [:ellipse :x] state) ellipse-diameter) 
       (->
        state
        (update-in [:ellipse :x-sign] (fn [_] +))
        move-ellipse-x-diagonal)
       :else
       (move-ellipse-x-diagonal state))
     (update-in
      [:ellipse :y]
      (fn [y]
        ((get-in state [:ellipse :y-sign]) y ellipse-y-step))))))

(defn update-rect-state [state]
  (cond
    (>= (get-in state [:rect :x]) (- width rect-width))
    (update-in state [:rect :x] (fn [x] (- x rect-x-step)))
    (<= (get-in state [:rect :x]) 0)
    (update-in state [:rect :x] (fn [x] (+ x rect-x-step)))
    :else
    state))

(defn is-ellipse-and-enemy-point-collide?
  [state enemy-state point]
  (> (count
      (set/intersection
       (set
        (range
         (- (get-in state [:ellipse point]) ellipse-radius)
         (+ 1 (+ (get-in state [:ellipse point]) ellipse-radius))))
       (set
        (range
         (point enemy-state)
         (+ 1 (+ (point enemy-state) enemy-diameter))))))
     0))

(defn update-enemies-state [state]
  (let [enemies-shape-state (:enemies state)
        total-enemies (count enemies-shape-state)
        enemies-state-alive
        (->>
         (remove
          (fn [enemy-state]
            (and
             (is-ellipse-and-enemy-point-collide?
              state
              enemy-state
              :y)
             (is-ellipse-and-enemy-point-collide?
              state
              enemy-state
              :x)))
          enemies-shape-state)
         (map (fn [enemy-state]
                (if (= (:level state) 2)
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
                      (update new-enemy-state
                              :x
                              (fn [_] (- width enemy-diameter)))
                      (<= (:x new-enemy-state) 0)
                      (update new-enemy-state :x (fn [_] 0))
                      :else
                      new-enemy-state))
                  enemy-state))))
        new-total-enemies (count enemies-state-alive)
        delta-enemies (- total-enemies new-total-enemies)]    
    (if (= 0 delta-enemies)
      (update state :enemies (fn [_] enemies-state-alive))
      (->
       (update state :enemies (fn [_] enemies-state-alive))
       (update :score
               #(+ % (* (- total-enemies new-total-enemies) 10)))
       (update :enemies-total #(- % delta-enemies))))))

(defn update-level [state]  
  (if (= 0 (:enemies-total state))
    (case (:level state)
      2 (update state :level :end)
      (init-state (inc (:level state))))
    state))

(defn update-state [state]
  (if (or (= (:level state) :end)
          (:is-paused? state)) 
    state
    (->
     state
     update-ellipse-state
     update-rect-state
     update-enemies-state
     update-level)))

(defn draw-state [state] 
  (q/background background-color)

  (if (= (:level state) :end)
    (do
      (q/text-size 100)
      (q/text "THE END" (/ height 2) (/ width 2)))
    (do
      (q/fill 131 131 131)
      (q/rect (get-in state [:rect :x])
              (get-in state [:rect :y])
              rect-width
              rect-height)
      (q/fill 0 248 255)
      (q/ellipse (get-in state [:ellipse :x])
                 (get-in state [:ellipse :y])
                 ellipse-diameter
                 ellipse-diameter)
      (q/text-size 20)
      (q/fill 255)
      (q/text (str "Score : " (:score state)) 20 20)
      (q/text (str "Level : " (:level state)) (- (/ width 2) 20) 20)
      (q/text
       (str "Enemies Total : " (:enemies-total state)) (- width 200) 20)
      (when (:is-paused? state)
        (q/text-size 60)
        (q/text "PAUSED" (/ height 2) (/ width 2)))
      (doseq [p (:enemies state)]
        (q/fill (rand-int 256) 120 (rand-int 256))
        (q/rect (:x p) (:y p) enemy-diameter enemy-diameter)))))

(defn -main
  [& args]
  (q/defsketch pong-shape-invaders
    :title "Pong shape invaders"
    :size [width height]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]
    :key-pressed (fn [{:keys [rect-x] :as state} { :keys [key key-code] }]
                   (->
                    (case key
                      (:right)
                      (update-in state
                                 [:rect :x]
                                 (partial + rect-x-step))
                      (:left)
                      (update-in state
                                 [:rect :x]
                                 (fn [x] (- x rect-x-step)))
                      (:up)
                      (update state :is-paused? (fn [x] (not x)))
                      state)
                    (update-in [:rect :dir] (fn [_] key))))))
