(ns maze.solver
  (:require [quil.core :as q]
            [maze.generator :as gen]))

(def cell-size 20)

(def dir {1 [0 -1] 2 [0 1] 4 [1 0] 8 [-1 0]})

;; -----------------------------------------------------------------------------------
;; Helper Function
;; -----------------------------------------------------------------------------------

(defn add-points
  [& pts]
  (vec (apply map + pts)))

(def select-vals (comp vec vals select-keys))

(defn get-neighbour
  ([maze x y]
  (let [key       (get-in maze [y x])
        match-key (map #(bit-and % key) (keys dir))
        dir       (select-vals dir match-key)]
    (vec (map #(add-points [x y] %) dir))))
  ([maze [x y]]
   (get-neighbour maze x y)))

(defn visit?
  [pt visited]
  (some #(= % pt) visited))

;; -----------------------------------------------------------------------------------
;; Solver
;; -----------------------------------------------------------------------------------

;; 1) set cpt as (x1, y1), put cpt into the stack and mark cpt as visited 
;; 2) while cpt != end point (x2 y2)
;;       3) get the neighbours of cpt
;;       4) pick the unvisited point from the neighbour
;;            5) visit that point, mark it as visited, put it into stack and set cpt as that point
;;            6) if no unvisited neighbour (cpt is a dead end), remove cpt from the stack.
;;               Peek the stack and set that element as cpt

(defn dfs-solver
  [maze x1 y1 x2 y2]
  """
  x1, y1: starting point
  x2, y2: end point

  Return: a thread from (x1, y1) to (x2, y2)
  """
  (loop [cpt [x1 y1]
         thread [cpt]
         visited [cpt]]
    (if (= cpt [x2 y2])
      thread
      (let [neighbours (get-neighbour maze cpt)
            not-visited (filter (complement #(visit? % visited)) neighbours)]
        (if (not-empty not-visited)
          (let [npt (rand-nth not-visited)]
            (recur npt (conj thread npt) (conj visited npt)))
          (let [rolled-thread (pop thread)] ;; At dead end, so step back
            (recur (peek rolled-thread) rolled-thread visited)))))))

;; -----------------------------------------------------------------------------------
;; Draw
;; -----------------------------------------------------------------------------------

(defn draw-step
  ([x y]
   (let [semi-red (q/color 255 0 0 120)]
     (q/fill semi-red)
     (q/rect (* cell-size x) (* cell-size y) cell-size cell-size)))
  ([[x y]]
   (draw-step x y)))

(defn draw-path
  [path]
  (q/no-stroke)
  (doseq [step path]
    (draw-step step))
  (q/stroke 0))
