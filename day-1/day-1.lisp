;;;; day-1.lisp

(in-package #:day-1)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (input)
  "Parse input into solution-friendly format."
  input)

(defun replace-number-string (line numbers)
  "Replace spelled-out numbers for digit in given string per numbers alist."
  (let ((new-line (copy-seq line)))
    (dolist (pair numbers)
      (setf new-line (cl-ppcre:regex-replace-all (car pair) new-line (cdr pair))))
    new-line))

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
  (let ((numbers '(("one" . "o1e") ("two" . "t2o") ("three" . "t3e")
                   ("four" . "f4r") ("five" . "f5e") ("six" . "s6x")
                   ("seven" . "s7n") ("eight" . "e8t") ("nine" . "n9e"))))
    (solve-part-1 (loop for line in input
                        collecting (replace-number-string line numbers)))))

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
