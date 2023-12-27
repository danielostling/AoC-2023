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

(defun extend-hash-value-list (hash key value)
  "Add value to list referencef by key in hash.
   If key is missing, initiate a new list with value as element.
   Modifies given hash. NB There must be a better way to do this."
  (let ((current-value (gethash key hash ())))
    (setf (gethash key hash) (push value current-value))))

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

(defun 2d-subarray (arr top-left bottom-right)
  "Return a copied portion of given 2d array arr delimited by
       top-left point: row-1, col-1
   bottom-right point: row-2, col-2

   Note: There is no bounds checking. Rows and columns outside arr will cause exceptions."
  (let* ((row-1 (first top-left))
         (row-2 (first bottom-right))
         (col-1 (second top-left))
         (col-2 (second bottom-right))
         (new-rows (1+ (- row-2 row-1)))
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

(defun find-row-machine-parts (row schematic)
  "Return machine parts in given row in given schematic.
   Returned value is a list of machine parts and bounding frame coordinates, where each element is
   (machine-part-number top-left bottom-right)
   where top-left and bottom-right are (row col) coordinates."
  (let* ((cols-in-schematic (array-dimension schematic 1))
         (row-vector (make-array cols-in-schematic
                                 :displaced-to schematic
                                 :displaced-index-offset (* row cols-in-schematic)))
         (row-as-string (coerce row-vector 'string))
         (scan-start 0)
         (bounding-box ())
         (bounding-box-coordinates ())
         (part-numbers ()))
    (loop named parts
          :do (multiple-value-bind (hit-start hit-stop)
                  (cl-ppcre:scan "[0-9]+" row-as-string :start scan-start)
                (when (null hit-start)
                  (return-from parts part-numbers))
                (setf scan-start hit-stop)
                (setf bounding-box-coordinates (list
                                                (list (1- row) (1- hit-start))
                                                (list (1+ row) hit-stop)))
                (setf bounding-box (2d-subarray schematic
                                                (first bounding-box-coordinates)
                                                (second bounding-box-coordinates)))
                (unless (frame-dot-p bounding-box)
                  (push (list (parse-integer (subseq row-as-string hit-start hit-stop))
                              bounding-box-coordinates)
                        part-numbers))))))

(defun find-machine-parts (schematic)
  "Return machine parts in schematic as list of (machine-part-id (bounding-box-coordinates))."
    (let ((rows (array-dimension schematic 0)))
      (loop :for row :from 1 :below (1- rows)
            :when (find-row-machine-parts row schematic) :nconcing it)))

(defun find-gears (schematic)
  "Given a schematic, return list of (x y) coordinates for each gear."
  (let ((rows (array-dimension schematic 0))
        (cols (array-dimension schematic 1))
        (gears ()))
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :when (char= #\* (aref schematic row col))
                      :do (push (cons row col) gears)))
    gears))

(defun point-in-box-p (top-left bottom-right point)
  "Return T if point (row-p col-p) is inside box
  top-left (row-1 col-1) by bottom-right (row-2 col-2), else nil."
  (and (>= (car point) (first top-left))
       (<= (car point) (first bottom-right))
       (>= (cdr point) (second top-left))
       (<= (cdr point) (second bottom-right))))

(defun gear-ratio (parts)
  "Multiply given list of part numbers."
  (reduce #'* parts))

(defun solve-part-1 (schematic)
  "Solve part 1 of puzzle."
  (flet ((just-machine-parts (machine-parts)
           "Extract machine parts only from machine-parts data structure"
           (loop :for machine-part :in machine-parts :collect (car machine-part))))
    (loop :for machine-part :in (just-machine-parts (find-machine-parts schematic))
            :summing machine-part)))

(defun solve-part-2 (schematic)
  "Solve part 2 of puzzle."
  (let* ((gears (find-gears schematic))
         (machine-parts (find-machine-parts schematic))
         (connected-machine-parts (make-hash-table :test #'equal)))
    (loop :for gear :in gears
          :do (loop :for machine-part :in machine-parts
                    :when (point-in-box-p (caadr machine-part) (cadadr machine-part) gear)
                      :do (extend-hash-value-list connected-machine-parts gear (car machine-part))))
    (loop :for parts :being :the :hash-value :of connected-machine-parts
          :when (equal 2 (length parts))
            :sum (gear-ratio parts))))

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
