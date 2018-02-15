(ns pong-shape-invaders.util
  (:require [clojure.java.io :as io])
  (:import (javafx.scene.media Media
                               MediaPlayer)
           (javafx.embed.swing JFXPanel)
           (java.util Calendar)))

(defn play-sound! [sound]
  (let [_ (JFXPanel.)
        data-file (io/resource sound)
        media (Media. (.toString data-file))
        media-player (MediaPlayer. media)]
    (.play media-player)
    media-player))

(defn get-long-now []
  (.get (Calendar/getInstance) Calendar/MILLISECOND))

(defn stop-media-player! [media-players]
  (doseq [x media-players]
    (.stop x)))


