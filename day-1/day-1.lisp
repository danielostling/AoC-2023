;;;; day-1.lisp

(in-package #:day-1)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (input)
  "Parse input into solution-friendly format."
  input)

(defun alist-v (key alist)
  "Return value for key in alist or nil if key is missing."
  (cdr (assoc key alist :test 'equal)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let* ((just-numbers (loop for line in input
                             collect (loop for c across line
                                           when (digit-char-p c)
                                             collect (digit-char-p c))))
         (number-pairs (loop for number-list in just-numbers
                             when number-list
                               collect (list
                                        (first number-list)
                                        (first (last number-list))))))
    (loop for number-pair in number-pairs
          sum (+ (* 10 (nth 0 number-pair)) (nth 1 number-pair)))))


(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (let* ((number-map '(("one" . 1) ("two" . 2) ("three" . 3)
                       ("four" . 4) ("five" . 5) ("six" . 6)
                       ("seven" . 7) ("eight" . 8) ("nine" . 9)))))
  )

(defun main (&optional (mode :full))
  "AoC 2023 day 1 solutions.
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
