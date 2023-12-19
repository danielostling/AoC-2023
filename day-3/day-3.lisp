;;;; day-3.lisp

(in-package #:day-3)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun alist-v (key alist &optional (default 0))
  "Return value from alist at key or a default value."
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        default)))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
   Return a schema array with a dot (.) border added."
  (let* ((rows (+ (length lines) 2))
         (cols (+ (length (first lines)) 2))
         (new-lines ()))
    ;; Pad a margin of dots around all lines to simplify later steps.
    (push (make-string cols :initial-element #\.) new-lines)
    (dolist (line (reverse lines))
      (push (concatenate 'string "." line ".") new-lines))
    (push (make-string cols :initial-element #\.) new-lines)
    (make-array
     `(,rows ,cols)
     :initial-contents (mapcar (lambda (line) (coerce line 'list)) new-lines))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  
  )

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
