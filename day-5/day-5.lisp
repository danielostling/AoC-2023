;;;; day-5.lisp

(in-package #:day-5)

(defun alist-v (key alist &optional (default 0))
  "Return value from alist at key or a default value."
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        default)))

(defun string-to-int-list (number-string)
  "Split a string of integers into a list of numbers."
  (loop :for number
          :in (uiop:split-string (string-trim " " number-string) :separator '(#\Space))
        :collect (parse-integer number :junk-allowed t)))

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
   Input is a line of seeds and a set of conversion maps.

   Seeds
   -----
   Convert a line
   seeds: 79 14 55 13
   into alist entry
   (:seeds (79 14 55 13))

   Maps
   ----
   Maps start with a map name, ending with a colon. Next lines has
   three numbers; source range, destination range and range length.
   Next blank line delimits map. Following line is either next map or end of input.
   Map names in alist is converted from input map name.
   'humidity-to-location map:' becomes the symbol :humidity-to-location.

   NB: This is a complicated function. I'm quite sure it can be substantially simplified."
  (flet ((string-to-map-symbol (map-string)
           "Convert string 'something map:' to symbol `:someting`."
           (let* ((map-string-parts (uiop:split-string map-string :separator '(#\Space #\:)))
                  (map-symbol-part (first map-string-parts))
                  (symbol-string (concatenate 'string ":" map-symbol-part)))
             (read-from-string symbol-string)))
         (add-number-list-to-almanac (almanac alist-key numbers)
           "Add numbers to almanac key."
           (if (equal alist-key :seeds)
               (push (cons alist-key numbers) almanac)
               (if (null (assoc alist-key almanac))
                   (push (cons alist-key (list numbers)) almanac)
                   (setf (cdr (assoc alist-key almanac))
                         (push numbers (cdr (assoc alist-key almanac))))))
           almanac))
    (let ((almanac ())
          (current-key nil))
      (loop :for line :in lines
            :do (cond ((and (find #\: line) (> (count-if #'digit-char-p line) 0))  ;; seeds line
                       (progn
                         (setf current-key :seeds)
                         (setf almanac (add-number-list-to-almanac
                                        almanac
                                        current-key
                                        (string-to-int-list
                                         (second
                                          (uiop:split-string line :separator '(#\:))))))))
                      ((find #\: line)                                  ;; map line
                       (setf current-key (string-to-map-symbol line)))
                      ((> (count-if #'digit-char-p line) 0)             ;; seed numbers or ranges
                       (setf almanac
                             (add-number-list-to-almanac
                              almanac
                              current-key
                              (string-to-int-list line))))))
      almanac)))

(defun map-lookup (lookup-value mapping almanac)
  "Lookup value in almanac for given mapping.
   A mapping is three integers (drs srs rl);
   drs - destination range start
   srs - source range start
   rl - range length

   If value is in range srs -> srs+rl, return corresponding mapping from drs -> drs+rl, same index.
   If value is not in range srs -> srs+rl, return same value as given value."
  (let ((mapped-value (loop :for (drs srs rl) :in (alist-v mapping almanac)
                            :when (and (>= lookup-value srs) (< lookup-value (+ srs rl)))
                              :collect (+ drs (- lookup-value srs)))))
    (if mapped-value
        (first mapped-value)
        lookup-value)))

(defun trace-seed-through-mappings (seed map-order almanac)
  "Given a seed and an almanac, trace seed to location using the mappings."
  (loop :for mapping :in map-order
        :for lookup-value = seed then (map-lookup lookup-value mapping almanac)
        :finally (return lookup-value)))

(defun lowest-location-in-range (first-seed seed-range map-order almanac)
  "Given a seed range and an almanac, trace seeds to locations using the mappings.
   Return lowest location."
  (let ((final-seed (1- (+ first-seed seed-range))))
    (loop :for seed :from first-seed :to final-seed
          :minimize (trace-seed-through-mappings seed map-order almanac))))

(defun solve-part-1 (almanac)
  "Solve part 1 of puzzle."
  (let* ((seeds (alist-v :seeds almanac))
         (map-order '(:dummy :seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light
                      :light-to-temperature :temperature-to-humidity :humidity-to-location)))
    (loop :for seed :in seeds :minimizing (trace-seed-through-mappings seed map-order almanac))))

(defun solve-part-2 (almanac)
  "Solve part 2 of puzzle."
  (let* ((seed-ranges (alist-v :seeds almanac))
         (map-order '(:dummy :seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light
                      :light-to-temperature :temperature-to-humidity :humidity-to-location)))
    (loop :for (first-seed range-length) :on seed-ranges :by #'cddr
          :minimizing (lowest-location-in-range first-seed range-length map-order almanac))))

(defun main (&optional (mode :full))
  "AoC 2023 day 5 solutions.
   Mode is one of
   :full - use full puzzle input
   :test - use test puzzle input"
  (let* ((path-input-full #P"./input-full")
         (path-input-test #P"./input-test")
         (path-input (if (equal mode :full)
                         path-input-full
                         path-input-test))
         (almanac (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 almanac))
    (format t "Part 2: ~a~%" (solve-part-2 almanac))))
