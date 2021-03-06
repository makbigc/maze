(ns maze.generator
  (:require [clojure.pprint :refer (cl-format)]
            [quil.core :as q]))

(def board {:width 15 :height 15})

(def corners #{[0 0] [0 (board :height)] [(board :width) 0] [(board :width) (board :height)]})

(def cell-size 20)

;; The maze implementation is taken from:
;; http://weblog.jamisbuck.org/2010/12/27/maze-generation-recursive-backtracking
;; The maze is a w by h board in which each location is given by a 4-bit binary.

(def dir {1 :N 2 :S 4 :E 8 :W})

(defn init-maze
  [w h]
  (into [] (repeat h (into [] (repeat w 0)))))

(defn get-neighbour
  [pt width height]
  (let [[x y] pt]
    (->> [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
         (filter #(>= (first %) 0))
         (filter #(< (first %) width))
         (filter #(>= (second %) 0))
         (filter #(< (second %) height)))))

;; just another version of get-neighbour

(defn get-neighbour2
  [x y width height]
  (for [[nx ny] [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        :when (and (>= nx 0) (< nx width) (>= ny 0) (< ny height))]
    [nx ny]))

;; Each cell has four walls. To make a passage from one cell to another,
;; one has to modify the binaries in both cells.
;; For instance, remove the wall between the cell adjacent horizontally.
;;  __   __       __   __
;; |  | |  | --> |       |
;; |__| |__| --> |__   __|
;; 0000 0000     0010 0001

(defn carve-maze
  [maze pt1 pt2]
  """
  Remove the wall between the cells at pt1 and pt2.
  Assume pt1 and pt2 are adjacent. Futhermore, it is either that
  i) pt1 is above pt2 (dx=0, dy=-1) or
  ii) pt1 is to the left of pt2 (dx=-1, dy=0)
  """
  (let [[dx dy] (vec (map - pt1 pt2))]
    (if (some pos? [dx dy]) ;; The assumption doesn't hold
      (carve-maze maze pt2 pt1) ;; flip the points
      ;; The following is the else clause
      ;; fn update-in takes the element from a nested vec
      ;; by y-coordinate first and then by x-coordinate
      (let [fpt1 (reverse pt1)
            fpt2 (reverse pt2)]
        (cond
          ;; Assumption i) holds: pt1 is above pt2
          (zero? dx) (update-in (update-in maze fpt1 bit-or 2r0010)
                                fpt2 bit-or 2r0001)
          ;; Assumption ii) holds: pt1 is to the left of pt2
          (zero? dy) (update-in (update-in maze fpt1 bit-or 2r0100)
                                fpt2 bit-or 2r1000))))))

(defn visit?
  [pt visited]
  (some #(= % pt) visited))

(defn dfs-maze-gen
  [width height x y]
  """
  Generate a maze by depth-first-search algorithm starting at
  the point (x, y)
  """
  (loop [maze (init-maze width height)
         stack [[x y]] ;; vec is used to preserve the sequence
         visited [[x y]]]
    (if (empty? stack)
      maze
      (let [v            (peek stack)
            neighbours   (get-neighbour v width height)
            not-visited  (filter (complement #(visit? % visited)) neighbours)]
        (if (not-empty not-visited)
          (let [cpt (rand-nth not-visited)] ;; pick a random point from the neighbours
            (recur (carve-maze maze v cpt) (conj stack cpt) (conj visited cpt)))
          (recur maze (pop stack) visited))))))

;; -----------------------------------------------------------------------------------
;; Kruskal's Algo
;; -----------------------------------------------------------------------------------

(def cells (for [x (range (board :width))
                y (range (board :height))]
             [x y]))

(def vertical-edges
(->> (for [x (range (board :width))
           y (range (board :height))]
       [x y])
     (partition (board :height))
     (map #(partition 2 1 %))
     (apply concat))
  )

(def horizontal-edges
(->> (for [x (range (board :width))
           y (range (board :height))]
       [y x])
     (partition (board :width))
     (map #(partition 2 1 %))
     (apply concat))
  )

(defn init-edges
  []
  (concat horizontal-edges vertical-edges))

(defn init-parent
  []
  (zipmap cells cells))

(defn init-rank
  []
  (zipmap cells
          (take (* (board :width) (board :height)) (repeat 0))))

(defn find-root
  [parent pos]
  (if (= (parent pos) pos)
    pos
    (find-root parent (parent pos))))

(defn update-parent
  [parent rank pos1 pos2]
  (let [rank1 (rank pos1)
        rank2 (rank pos2)]
    (if (> rank1 rank2)
      (assoc parent pos2 pos1)
      (assoc parent pos1 pos2))))

(defn update-rank
  [rank pos1 pos2]
  (let [rank1 (rank pos1)
        rank2 (rank pos2)]
    (if (= rank1 rank2)
      (update rank pos2 inc)
      rank)))

(defn kruskal-maze-gen
  [width height]
  (loop [maze   (init-maze width height)
         edges  (shuffle (init-edges))
         parent (init-parent)
         rank   (init-rank)]
    (if (empty? edges)
      maze
      (let [[pt1 pt2] (peek edges)
            root1 (find-root parent pt1)
            root2 (find-root parent pt2)]
        (if (not= root1 root2) ; pt1 and pt2 aren't connected
          (recur (carve-maze maze pt1 pt2)
                 (pop edges)
                 (update-parent parent rank root1 root2)
                 (update-rank rank root1 root2))
          (recur maze
                 (pop edges)
                 parent
                 rank))))))

;; -----------------------------------------------------------------------------------
;; Prim's Algo
;; -----------------------------------------------------------------------------------

(defn get-new-edges
  [pt width height visited]
  (let [neighbour (get-neighbour pt width height)]
    (for [new-neighbour (filter (complement #(visit? % visited)) neighbour)]
      (list pt new-neighbour))))

(defn get-new-vertex
  [edge visited]
  (let [[pt1 pt2] edge
        not-visit? (complement #(visit? % visited))]
    (cond
      (not-visit? pt1) pt1
      (not-visit? pt2) pt2
      :else nil)))

(defn prim-maze-gen
  [maze width height]
  (let [start-pt [(rand-int width) (rand-int height)]]
    (loop [maze            (init-maze width height)
           visited         [start-pt]
           connected-edges (get-new-edges start-pt width height visited)]
      (if (= (count visited) (* width height))
        maze
        (let [edge       (rand-nth connected-edges)
              new-vertex (get-new-vertex edge)
              [pt1 pt2]  edge]
          (if (nil? new-vertex)
            (recur maze (remove #(= % edge) connected-edges) visited)
            (let [new-edges (get-new-edges new-vertex width height visited)]
              (recur (carve-maze maze pt1 pt2)
                     (into (remove #(= % edge) connected-edges) new-edges)
                     (into visited new-vertex)))))))))





;; -----------------------------------------------------------------------------------
;; Draw
;; -----------------------------------------------------------------------------------

(defn bin->walls
  [bin]
  """
  bin: 4-bit binary whose each digit shows which side of the cell is carved

  return: a list of dir keywords corresponding to the erected walls
          or nil if all four walls are removed
  """
  (let [walls (bit-and-not 2r1111 bin)] ;; the product binary shows which is the wall
    (vals (select-keys dir (map #(bit-and % walls) (keys dir))))))

(defn multi-by-cell-size
  [pt]
  (vec (map #(* cell-size %) pt)))

(defn draw-square-side
  [x y dir]
  """
  Draw one side of the square given by dir.
  dir is one of the dir keywords :N, :E, :S and :E.
  """
  (let [left-up      [x y]
        left-bottom  [x (inc y)]
        right-up     [(inc x) y]
        right-bottom [(inc x) (inc y)]]
    (let [sides {:N [left-up right-up]
                 :W [left-up left-bottom]
                 :E [right-up right-bottom]
                 :S [left-bottom right-bottom]}]
      (apply q/line (vec (map multi-by-cell-size (sides dir)))))))

(defn draw-square
  [x y bin]
  """
  Draw square at the point (x, y) without sides given by the 4-bit binary bin.
  """
  (let [f (partial draw-square-side x y)]
    (doseq [walls (bin->walls bin)]
      (f walls))))
    
(defn draw-maze
  [maze]
  (q/stroke-weight 2)
  (let [width (count (get maze 0))
        height (count maze)]
    (doseq [x (range width) ;; for loop doesn't work
            y (range height)]
      (draw-square x y (get-in maze [y x]))))
  (q/stroke-weight 1))
