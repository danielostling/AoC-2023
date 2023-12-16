;;;; day-1.lisp

(in-package #:day-1)

(defun read-input (path)
  "Read and return puzzle input."
  (declare (ignorable path))
  )

(defun parse-input (input)
  "Parse input into solution-friendly format."
  (declare (ignorable input))
  )

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (declare (ignorable input))
  )

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (declare (ignorable input))
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
