(ns pong-shape-invaders.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]
            [clojure.java.io :as io])
  (:import (javafx.scene.media Media
                               MediaPlayer)
           (javafx.embed.swing JFXPanel)
           (java.util Calendar)))

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

(defn play-sound [sound]
  (let [_ (JFXPanel.)
        data-file (io/resource sound)
        media (Media. (.toString data-file))
        media-player (MediaPlayer. media)]
    (.play media-player)
    media-player))

(defn get-long-now []
  (.get (Calendar/getInstance) Calendar/MILLISECOND))

(defn generate-enemies-shape-state-in-y [y]
  (->>
   (range enemy-diameter
          (* 24 enemy-diameter)
          (* 4 enemy-diameter))
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

(defn init-state [level
                  life
                  last-level-score
                  score
                  tms
                  & {:keys [game-status sound] :or {game-status :run}}]
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
     :last-level-score last-level-score
     :score score
     :enemies-total (count enemies-shape-state)
     :level level
     :game-status game-status
     :life life
     :tms tms
     :sound sound
     :total-enemies-hitted 0}))

(defn setup []
  (q/frame-rate fps)
  (init-state 0 3 0 0 (get-long-now)))

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
    (let [life (:life state)
          current-tms (get-long-now)
          sound "ellipse_dead"]
      ;; (play-sound "ellipse_dead.mp3")
      (if (= life 1)
        (init-state 0
                    3
                    0
                    (:score state)
                    (get-long-now)
                    :game-status :game-over
                    :sound sound)
        (init-state (:level state)
                    (dec (:life state))
                    (:last-level-score state)
                    (:last-level-score state)
                    (- current-tms (:tms state))
                    :sound sound)))
    (->
     (cond
       (<= (get-in state [:ellipse :y]) (/ ellipse-diameter 2)) 
       (->
        state
        (update-in [:rect :x-speed]
                   (fn [_] ellipse-diagonal-step))
        (update-in [:ellipse :y-sign] (fn [_] +))
        (update :sound (fn [_] nil)))
       (and (is-ellipse-hit-rect? state)
            (= (get-in state [:ellipse :y])
               (+ (- rect-y-init rect-height) ellipse-diameter)))
       ;; (play-sound "ellipse_hit_rectangle.mp3")
       (->
        state
        (update-in [:ellipse :x-speed]
                   (fn [_] (get-in state [:rect :x-speed])))
        (update-in [:ellipse :y-sign] (fn [_] -))
        (update-in
         [:ellipse :x-sign]
         (fn [y] (if (= :left (get-in state [:rect :dir])) + -)))
        (update :sound (fn [_] "ellipse_hit_rectangle")))       
       (>= (get-in state [:ellipse :x]) (- width ellipse-diameter))
       ;; (play-sound "ellipse_hit_boundary.mp3")
       (->
        state
        (update-in [:ellipse :x-sign] (fn [_] -))
        move-ellipse-x-diagonal
        (update :sound (fn [_] "ellipse_hit_boundary")))
       (= (get-in state [:ellipse :x] state) ellipse-diameter) 
       ;; (play-sound "ellipse_hit_boundary.mp3")
       (->
        state
        (update-in [:ellipse :x-sign] (fn [_] +))
        move-ellipse-x-diagonal
        (update :sound (fn [_] "ellipse_hit_boundary")))      
       :else
       (->
        (move-ellipse-x-diagonal state)
        (update :sound (fn [_] nil))))
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
             (is-ellipse-and-enemy-point-collide? state enemy-state :y)
             (is-ellipse-and-enemy-point-collide? state enemy-state :x)))
          enemies-shape-state)
         (map (fn [enemy-state]
                (if (>= (:level state) 1)
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
        delta-enemies (- total-enemies new-total-enemies)
        new-state (if (= 0 delta-enemies)
                    (update state :enemies (fn [_] enemies-state-alive))
                    (->
                     (update state :enemies (fn [_] enemies-state-alive))
                     (update :score
                             #(+ % (* (- total-enemies new-total-enemies) 10)))
                     (update :enemies-total #(- % delta-enemies))))]
    (-> new-state
        (update :total-enemies-hitted (fn [_] delta-enemies)))))

(defn update-level [state]  
  (if (= 0 (:enemies-total state))
    (case (:level state)
      2 (update state :game-status (fn [_] :end))
      (init-state (inc (:level state))
                  (:life state)
                  (:score state)
                  (:score state)
                  (- (get-long-now) (:tms state))))
    state))

(defn update-tms [state]
  (update state :tms #(- (get-long-now) %)))

(defn update-state [state]
  (let [game-status (:game-status state)]
    (if (= game-status :run)
      (->
       state
       update-tms
       update-ellipse-state
       update-rect-state
       update-enemies-state
       update-level)
      state)))

(defn draw-when-game-run! [state]
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
  (q/text (str "Level : " (:level state)) 180 20)
  (q/text (str "Life : " (:life state)) 300 20)
  (q/text
   (str "Enemies Total : " (:enemies-total state)) (- width 200) 20)
  (when (= (:game-status state) :pause)
    (q/text-size 60)
    (q/text "PAUSED" (/ height 2) (/ width 2)))
  (doseq [p (:enemies state)]
    (q/fill (rand-int 256) 120 (rand-int 256))
    (q/rect (:x p) (:y p) enemy-diameter enemy-diameter))
  (dotimes [_ (:total-enemies-hitted state)]
    (play-sound "ellipse_hit_enemy.mp3"))
  (when-let [sound (:sound state)]
    (play-sound (str sound ".mp3"))))

(defn draw-when-game-over! [state]
  (q/fill 255)
  (q/text-size 34)
  (q/text "GAME OVER" 100 100)
  (q/text (str "TOTAL SCORE : " (:score state)) 100 150)
  (q/text (str "TIME : " (:tms state) " ms") 100 200)
  (q/text "PRESS ↑ (UPARROW) BUTTON TO RESTART" 40 300))

(defn draw-when-game-end! [state]
  (q/fill 255)
  (q/text-size 40)
  (q/text "THE END, CONGRATULATION !" 100 100)
  (q/text (str "TOTAL SCORE : " (:score state)) 100 300)
  (q/text (str "TIME : " (:tms state) " ms") 100 500))

(defn draw-state! [state] 
  (q/background background-color)
  (let [game-status (:game-status state)]
    (cond 
      (= game-status :game-over) 
      (draw-when-game-over! state)
      (= game-status :end) 
      (draw-when-game-end! state)
      :else
      (draw-when-game-run! state))))

(defn -main
  [& args]
  (q/defsketch pong-shape-invaders
    :title "Pong shape invaders"
    :size [width height]
    :setup setup
    :update update-state
    :draw draw-state!
    :features [:keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]
    :key-pressed (fn [{:keys [rect-x game-status] :as state} {:keys [key key-code]}]
                   (case game-status
                     :game-over
                     (if (= key :up)
                       (init-state 0 3 0 0 (get-long-now))
                       state)
                     :pause
                     (if (= key :up)
                       (update state
                             :game-status
                             (fn [_] :run))
                       state)
                     :run
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
                       (update state
                               :game-status
                               (fn [_] :pause))
                       state)
                      (update-in [:rect :dir] (fn [_] key)))
                     state))))


