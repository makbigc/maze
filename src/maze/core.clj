(ns maze.core
  (:require [quil.core :as q]
            [maze.generator :as gen]
            [maze.solver :as solver]))

(def my-maze (gen/create-maze 15 15 0 0))

(def my-path (solver/dfs-solver my-maze 0 0 14 14))

(defn draw []
  (q/background 255)
  (solver/draw-path my-path)
  (gen/draw-maze my-maze))

(q/defsketch maze
  :size [300 300]
  :draw draw)
