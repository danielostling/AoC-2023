;;;; day-4.lisp

(in-package #:day-4)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun numbers-to-ints (number-string)
  "Convert string
   '83 86  6 31 17  9 48 53'
   to
   (83 86 6 31 17 9 48 53)"
  (loop :for number-list :in (uiop:split-string number-string)
        :for numbers = (loop :for number
                               :in (uiop:split-string number-list :separator '(#\ ))
                             :collect (parse-integer number :junk-allowed t))
        :nconcing numbers))

(defun split-cards-line (line)
  "Split a single input string into list of list of winning and held numbers."
  (let* ((number-strings (uiop:split-string line :separator '(#\: #\|)))
         (winning-numbers (numbers-to-ints (second number-strings)))
         (held-numbers (numbers-to-ints (third number-strings))))
    (list winning-numbers held-numbers)))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
   Convert a line like this
   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
   into a list of winning numbers and held numbers
   ((41 48 83 86 17) (83 86 6 31 17 9 48 53))
   Finally, do this for each card line in input."
  (loop :for line :in lines :collect (split-cards-line line)))

(defun make-wins-per-card-map (cards)
  "Map cards to respective list of won cards per card. First card in input is card 1.
   DAY-4> (make-wins-per-card-map *cards*)
   Card 1: (2 3 4 5)  <-- Card 1 has four winning numbers. This means a win of four following cards.
   Card 2: (3 4)      <-- Card 2 has two winning numbers. This means a win of two following cards.
   Card 3: (4 5)      <-- Card 3 has two winning numbers. This means a win of two following cards.
   Card 4: (5)        <-- Card 4 has one winning numbers. This means a win of one following card.
   Card 5: NIL        <-- Card 5 has no winning numbers. This means no following cards.
   Card 6: NIL        <-- Card 6 has no winning numbers. This means no following cards."
  (let ((wins-map (make-hash-table :test #'equal)))
    (loop :for card :in cards
          :for card-number :from 1
          :for winning-numbers-in-card = (length (intersection (first card) (second card)))
          :if (> winning-numbers-in-card 0)
            :do (setf (gethash card-number wins-map)
                      (loop :for idx
                              :from (1+ card-number)
                                :to (+ card-number winning-numbers-in-card)
                            :collect idx))
          :else :do (setf (gethash card-number wins-map) ()))
    wins-map))

(defun solve-part-1 (cards)
  "Solve part 1 of puzzle."
  (loop :for winning-numbers-in-card
          :being :the :hash-value :of (make-wins-per-card-map cards)
        :when (> (length winning-numbers-in-card) 0)
          :sum (ash 1 (1- (length winning-numbers-in-card)))))

(defun solve-part-2 (cards)
  "Solve part 2 of puzzle."
  (let* ((card-map (make-wins-per-card-map cards))
         (won-cards (loop :for won-cards-per-card :being :the :hash-value :of card-map
                          :when won-cards-per-card :collect won-cards-per-card))
         (temp-cards ())
         (list-remains t))
    (loop
      :while list-remains
      :do (progn
            (setf list-remains nil)
            (setf temp-cards nil)
            (loop :for card-list-or-item :in won-cards
                  :if (listp card-list-or-item)
                    :do (progn
                          (setf list-remains t)
                          (push (first card-list-or-item) temp-cards)
                          (when (gethash (first card-list-or-item) card-map)
                            (push (gethash (first card-list-or-item) card-map) temp-cards))
                          (when (rest card-list-or-item)
                            (push (rest card-list-or-item) temp-cards)))
                  :else :do (push card-list-or-item temp-cards))
            (setf won-cards (reverse (copy-list temp-cards)))))
    (+ (length cards) (length won-cards))))

(defun main (&optional (mode :full))
  "AoC 2023 day 4 solutions.
   Mode is one of
   :full - use full puzzle input
   :test - use test puzzle input"
  (let* ((path-input-full #P"./input-full")
         (path-input-test #P"./input-test")
         (path-input (if (equal mode :full)
                         path-input-full
                         path-input-test))
         (cards (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 cards))
    (format t "Part 2: ~a~%" (solve-part-2 cards))))
