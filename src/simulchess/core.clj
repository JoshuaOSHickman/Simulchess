(ns simulchess.core
  (:use lamina.core))

(def size 8)
(def num-players 2)
(def empty-piece {:type :empty :owner nil})
(def empty-board (vec (for [x (range size)] (vec (for [y (range size)] empty-piece)))))

(defn place-piece [board [x y] piece] ; TOSO check if piece is already there?
  (assoc-in board [x y] piece))

(defn at-place [board [x y]]
  (let [column (get board x)] (get column y)))

;; rules for moving:
;; empty is destroyed always
;; moving piece says alive unless both are moving
;; if both are moving, then the piece dies

(defn move-resolver [board move-channels]
  ;; make the channels passed in wrapped with something that filters for open moves.
  (let [moves (map wait-for-message move-channels); get one move from each
        items (map (fn [move]
                     {:move move
                      :item (at-place board (:start move))})
                   moves)
        moves-by-end (vals (group-by (comp :end :move) items))
        lifted-board (reduce (fn [board move]
                               (place-piece board (:start move)
                                            empty-piece))
                             board moves)]
    (reduce (fn [board move-set]
              (let [dest (:end (:move (first move-set)))]
                (if (> (count move-set) 1)
                  (place-piece board dest empty-piece) ; all ends will be the same -- kill them.
                  (let [move (first move-set)
                        type (:type (:item move))
                        [xd yd] dest
                        moved-piece (assoc (:item move) :moved? true)]
                    (if (and (= type :pawn) (or (= 0 xd) (= (dec size) xd)))
                      (place-piece board dest (assoc moved-piece :type (:promote move)))
                      (place-piece board dest moved-piece))))))
            lifted-board moves-by-end)))

;; this way I can seperate logic out for each type of piece
(defmulti open-moves ; must return set of [x y]
  (fn [board position] (:type (at-place board position))))

(defn within-board [[x y]] (and (< -1 x size) (< -1 y size)))

(defmethod open-moves :empty [board position] #{})

(defmethod open-moves :king [board position]
  ;; todo: castling
  (let [owner (:owner (at-place board position))
        [x y] position]
    (->> (for [xc (range (dec x) (+ 2 x))
               yc (range (dec y) (+ 2 y))
               :when (not (and (= xc x) (= yc y)))]
           [xc yc])
         (filter (fn [place] (not= owner (:owner (at-place board place)))))
         (filter within-board)
         (set))))

(defmethod open-moves :pawn [board position]
  ;; todo: pawn promotion
  (let [piece (at-place board position)
        owner (:owner piece)
        direction (if (zero? owner) 1 -1)
        [x y] position]
    (into (if (= :empty (:type (at-place board [x (+ y direction)])))
            (if (and (= :empty (:type (at-place board [x (+ y (* 2 direction))])))
                     (not (:moved? piece)))
              #{[x (+ y direction)] [x (+ y (* 2 direction))]}
              #{[x (+ y direction)]})
            #{})
          (filter (fn [place]
                    (and (within-board place)
                         (let [piece (at-place board place)
                               target-owner (:owner piece)]
                           (and (not (= :empty (:type piece)))
                                (not (= owner target-owner))))))
                  (map #(vector (+ x %) (+ y direction)) [1 -1])))))

(defn board-path [position change board player]
  ;; For rooks, queens, bishops
  (let [next-pos (change position)
        piece (at-place board next-pos)
        type (:type piece)
        owner (:owner piece)]
    (cond (= type :empty) (conj (board-path next-pos change board player) next-pos)
          (= owner player) #{}
          :else #{position})))

;; this code stinks too. put it in board path?
(defmethod open-moves :rook [board position]
  (let [[x y] position
        player (:owner (at-place board position))
        [upx dwnx upy downy] (map (fn [[dx dy]]
                                    (fn [[x y]] [(+ x dx) (+ y dy)]))
                                  [[1 0] [-1 0] [0 1] [0 -1]])]
    (-> (reduce into (map #(board-path position % board player)
                          [upx dwnx upy downy]))
        (disj position)))) ;; should probably rewrite this whole block to make this a non-issue.
;;; actually, I don't know why that was showing up in the first place....

(defmethod open-moves :bishop [board position]
  (let [[x y] position
        player (:owner (at-place board position))
        [ne nw sw se] (map (fn [[dx dy]]
                             (fn [[x y]] [(+ x dx) (+ y dy)]))
                           [[1 1] [-1 1] [-1 -1] [1 -1]])]
    (-> (reduce into (map #(board-path position % board player)
                          [ne nw sw se]))
        (disj position))))

(defmethod open-moves :queen [board position]
  (let [[x y] position
        player (:owner (at-place board position))
        [ne n nw w sw s se e] (map (fn [[dx dy]]
                             (fn [[x y]] [(+ x dx) (+ y dy)]))
                           [[1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] [1 0]])]
    (-> (reduce into (map #(board-path position % board player)
                          [ne nw sw se]))
        (disj position))))

(defmethod open-moves :knight [board position]
  (let [[x y] position
        owner (:owner (at-place board position))]
    (->> [[1 2] [2 1] [-1 2] [-2 1] [1 -2] [2 -1] [-1 -2] [-2 -1]]
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [place] (not= owner (:owner (at-place board place)))))
         (filter within-board)
         (set))))

(defn endgame? [game]
  (let [board (:board game)
        kings (filter #(= (:type %) :king) (apply concat board))]
    (cond (= 0 (count kings)) :tie
          (= 1 (count kings)) (:owner (first kings))
          :else false)))

(defn game
  ([board] (game board move-resolver open-moves endgame?))
  ([board mr om e?] {:board board :move-resolver mr :open-moves om :endgame? e?}))

(defn play-turn [game inputs]
  (let [errors (for [x (range num-players)] (channel))
        moves (for [x (range num-players)] (channel))
        valid? (fn [move] (contains? ((:open-moves game) (:board game) (:start move)) (:end move)))
        owner-at (fn [move] (:owner (at-place (:board game) move)))]
    (dorun
     (map (fn [input player moves error]
            (receive input (fn [move]
                             (if (and (= player (owner-at (:start move)))
                                      (valid? move))
                               (enqueue moves move)
                               (enqueue error move)))))
          inputs (range num-players) moves errors))
    {:board (future ((:move-resolver game) (:board game) moves))
     :errors errors}))

(defn std-board []
  (let [p2row (dec size)
        p1row 0]
    (reduce (fn [board place] (place board))
            empty-board
            (map (fn [player row place type]
                   (fn [board]
                     (place-piece board [place row] {:type type :owner player :moved? false})))
                 (concat (repeat (* 2 size) 0) (repeat (* 2 size) 1))
                 (concat (repeat size 0) (repeat size 1) (repeat size (- size 2)) (repeat size (dec size)))
                 (cycle (range size))
                 (let [row [:rook :knight :bishop :queen :king :bishop :knight :rook]
                       pawns (repeat 8 :pawn)]
                   (concat row pawns pawns row))))))

;; Command line specific input

(defn erase-screen [] (println "\u001B[1J"))

(defn blueify [s] (str "\u001B[34;1m" s "\u001B[39;0m")) ; set to a bold (;1m) blue (the 4 in 34)
(defn redify [s] (str "\u001B[31;1m" s "\u001B[39;0m")) ; and refert to a light (;0m) default (39 means terminal default.

(def piece-letters {:empty " " :king "K" :rook "R" :bishop "B"
                    :queen "Q" :knight "N" :pawn "P"})

(defn render-space [piece]
  ((if (or (nil? (:owner piece))
          (zero? (:owner piece)))
     blueify redify)
   (str " " (piece-letters (:type piece)) " ")))

(defn print-board [board]
  (let [rows (reverse (apply map vector board))
        lines (map #(apply str %2" |" %1); add beginning seperator and row number
                   (map (fn [row] (map #(str (render-space %) "|") row)) rows)
                   (iterate dec (dec size)))
        ;; * 4 because representations are 3 long, seperators are 1, inc for extra seperator
        ;; two spaces to account for row numbers.
        linebreak (apply str "\n  " (concat (for [i (range (inc (* 4 size)))] "-") ["\n"]))
        column-legend (apply str " " (for [c (range 0 size)] (str "   " c)))]
    (apply str column-legend (concat (interleave (cycle [linebreak]) lines) [linebreak]))))

(defn cl-prompt [s];; add the verification, transformation function, filter out things that throw or return null into a loop that tells them they did it all wrong.
  (print (str s ": "))
  (flush)
  (read-line))

(defn cl-move-read [game player]
  ;;;  (erase-screen) ;; bring this back
  (let [board (:board game)]
    (print "Your pieces")
    (prn (filter #(= player (:owner %))
                 (for [x (range size) y (range size)]
                   (merge (at-place board [x y]) {:place [x y]}))))
    (println (print-board board))
    (let [x (Integer/parseInt (cl-prompt "X Coord"))
          y (Integer/parseInt (cl-prompt "Y Coord"))]
      (print "Possible moves:")
      (prn ((:open-moves game) board [x y]))
      (let [tox (Integer/parseInt (cl-prompt "To what X?"))
            toy (Integer/parseInt (cl-prompt "And what Y?"))]
        {:start [x y] :end [tox toy]}))))

(defn command-line-game
  ([] (command-line-game (game (std-board))))
  ([game]
     (let [moves (for [player [0 1]] (channel (cl-move-read game player)))
           next (play-turn game moves)]
       (dorun (map (fn [moves errors player-num]
                     (receive errors (fn [illegal-move]
                                       (println "player" player-num "made an illegal move. Remake the move")
                                       (enqueue moves (cl-move-read game (dec player-num))))))
                   moves (:errors next) (iterate inc 1)))
       (let [next-turn (assoc game :board @(:board next))]
         (if (not ((:endgame? next-turn) next-turn))
           (recur next-turn)
           ((:endgame? next-turn) next-turn))))))

(defn -main [& args]
  (command-line-game))