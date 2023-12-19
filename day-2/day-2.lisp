;;;; day-2.lisp

(in-package #:day-2)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun alist-v (key alist &optional (default 0))
  "Return value from alist at key or a default value."
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        default)))

(defun parse-turn-string (turn-string)
  "Parse a string like this
   `1 blue, 2 green`
   into a data structure like this
   `((:green . 2) (:blue . 1))`"
  (flet ((split-and-flip (cube-count-string)
           (let ((parts (uiop:split-string cube-count-string :separator '(#\ ))))
             (cons
              (read-from-string (format nil ":~a" (cadr parts)))
              (parse-integer (car parts))))))
    (let ((draw-color-strings (mapcar
                               (lambda (s) (string-trim " " s))
                               (uiop:split-string turn-string :separator '(#\,)))))
      (mapcar #'split-and-flip draw-color-strings))))

(defun parse-line (line)
  "Convert a line like this
   `Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue`
   into a data structure like this
   `(2 (((:green . 2) (:blue . 1))
       ((:red . 1) (:green . 3) (:blue . 4))
       ((:green . 1) (:blue . 1))))`"
  (let* ((id-and-turns (uiop:split-string line :separator '(#\: #\;)))
         (id (parse-integer (second (uiop:split-string (car id-and-turns)))))
         (turn-strings (rest id-and-turns))
         (turns (loop :for turn-string :in turn-strings
                      :for turn-count = 1 :then (1+ turn-count)
                      :collect (parse-turn-string turn-string))))
    (list id turns)))

(defun parse-input (input)
  "Parse input into solution-friendly format."
  (mapcar #'parse-line input))

(defun cube-sets-from-draw-set (draw-set)
  "Return cube sets from given draw set."
  (second draw-set))

(defun possible-p (draw-set)
  "Return T if set is possible, else NIL."
  (let ((bag-contains '((:red . 12) (:green . 13) (:blue . 14)))
        (cube-sets (cube-sets-from-draw-set draw-set))
        (results ()))
    (dolist (cube-set cube-sets)
      (push (every (lambda (color)
                     (and (>= (alist-v color bag-contains) (alist-v color cube-set))))
                   '(:red :green :blue))
            results))
    (notany #'null results)))

(defun update-color (cube-set color value)
  "Set a new value of a color in cube set. Cube set is updated."
  (setf (cdr (assoc color cube-set)) value))

(defun max-cube-set (cube-set-1 &optional (cube-set-2 '((:red . 0) (:green . 0) (:blue . 0))))
  "Given two cube sets, return highest number of each colored cubes.
   A cube set is on the form `((:red . ?) (:green . ?) (:blue . ?))` where the question marks
   are integer counts of how many cubes there are of that color.
   Return another cube set representing the max value for each color."
  (let ((max-cube-set '((:red . 0) (:green . 0) (:blue . 0))))
    (dolist (color '(:red :green :blue))
      (update-color max-cube-set color (max (alist-v color cube-set-1) (alist-v color cube-set-2))))
    max-cube-set))

(defun cube-set-power (cube-set)
  "Return power of given cube set.
   Defalut value of 1 in getting color count, as it won't screw up multiplication."
  (reduce #'* (mapcar (lambda (color) (alist-v color cube-set 1)) '(:red :green :blue))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for draw-set :in input
        :when (possible-p draw-set) :sum (first draw-set)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (loop :for draw-set :in input
        :for cube-set = (reduce 'max-cube-set (cube-sets-from-draw-set draw-set))
        :summing (cube-set-power cube-set)))

(defun main (&optional (mode :full))
  "AoC 2023 day 2 solutions.
   Mode is one of
   :full - use full puzzle input
   :test - use test puzzle input"
  (let* ((path-input-full #P"./input-full")
         (path-input-test #P"./input-test")
         (path-input (if (equal mode :full)
                         path-input-full
                         path-input-test))
         (input (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 input))
    (format t "Part 2: ~a~%" (solve-part-2 input))))
