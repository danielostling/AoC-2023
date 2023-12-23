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
   Return a schematic array with a dot (.) border added."
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

(defun 2d-subarray (row-1 col-1 row-2 col-2 arr)
  "Return a copied portion of given 2d array arr delimited by
       top-left point: row-1, col-1
   bottom-right point: row-2, col-2

   Note: There is no bounds checking. Rows and columns outside arr will cause exceptions."
  (let* ((new-rows (1+ (- row-2 row-1)))
         (new-cols (1+ (- col-2 col-1)))
         (new-arr (make-array `(,new-rows ,new-cols)
                              :element-type 'character)))
    (loop :for row :from row-1 :to row-2
          :do (loop :for col :from col-1 :to col-2
                    :do (setf
                         (aref new-arr (- row row-1) (- col col-1))
                         (aref arr row col))))
    new-arr))

(defun frame-dot-p (arr)
  "Given a submatrix arr, check if the outer 'frame' is all dots."
  (flet ((dot-p (val)
           "t if val is a dot (.), else nil."
           (char= val #\.)))
    (let* ((rows (array-dimension arr 0))
           (cols (array-dimension arr 1)))
      (and
       (every #'dot-p (loop :for col :from 0 :below cols :collect (aref arr 0 col)))
       (every #'dot-p (loop :for col :from 0 :below cols :collect (aref arr (1- rows) col)))
       (every #'dot-p (loop :for row :from 0 :below rows :collect (aref arr row 0)))
       (every #'dot-p (loop :for row :from 0 :below rows :collect (aref arr row (1- cols))))))))

(defun machine-parts (row schematic)
  "Return all non-machine parts in given row in given schematic."
  (let* ((cols-in-schematic (array-dimension schematic 1))
         (row-vector (make-array cols-in-schematic
                                 :displaced-to schematic
                                 :displaced-index-offset (* row cols-in-schematic)))
         (row-as-string (coerce row-vector 'string))
         (scan-start 0)
         (part-numbers ()))
    (loop named parts
          :do (multiple-value-bind (hit-start hit-stop)
                  (cl-ppcre:scan "[0-9]+" row-as-string :start scan-start)
                (when (null hit-start)
                  (return-from parts part-numbers))
                (setf scan-start hit-stop)
                (unless (frame-dot-p (2d-subarray (1- row) (1- hit-start) (1+ row) hit-stop schematic))
                  (push (parse-integer (subseq row-as-string hit-start hit-stop)) part-numbers))))))

(defun solve-part-1 (schematic)
  "Solve part 1 of puzzle."
  (let ((rows (array-dimension schematic 0)))
    (reduce #'+ (loop :for row :from 1 :below (1- rows)
                      :when (machine-parts row schematic) :nconcing it))))

(defun solve-part-2 (schematic)
  "Solve part 2 of puzzle."
  1)

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
         (schematic (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 schematic))
    (format t "Part 2: ~a~%" (solve-part-2 schematic))))
