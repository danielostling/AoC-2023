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

   1 blue, 2 green

   into a data structure like this

   ((:green . 2) (:blue . 1))"
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
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue

   into a data structure like this

   (2 (((:green . 2) (:blue . 1))
       ((:red . 1) (:green . 3) (:blue . 4))
       ((:green . 1) (:blue . 1))))"
  (let* ((id-and-turns (uiop:split-string line :separator '(#\: #\;)))
         (id (parse-integer (second (uiop:split-string (car id-and-turns)))))
         (turn-strings (rest id-and-turns))
         (turns (loop for turn-string in turn-strings
                      for turn-count = 1 then (1+ turn-count)
                      collect (parse-turn-string turn-string))))
    (list id turns)))

(defun parse-input (input)
  "Parse input into solution-friendly format."
  (mapcar #'parse-line input))

(defun possible-p (draw-set)
  "Return T if set is possible, else NIL."
  (let* ((bag-contains '((:red . 12) (:green . 13) (:blue . 14)))
         (cube-counts (first (second draw-set)))
         (result (every (lambda (color)
                          (and (>= (alist-v color bag-contains) (alist-v color cube-counts))))
                        '(:red :green :blue))))

    (format t "draw-set ~a => ~a~%" draw-set result)
    result
    ))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop for draw-set in input
        when (possible-p draw-set) sum (first draw-set)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

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
