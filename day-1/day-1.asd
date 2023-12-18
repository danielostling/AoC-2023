;;;; day-1.asd

(asdf:defsystem #:day-1
  :description "Solutions for AoC 2023 day 1 puzzles."
  :author "Your Name <your.name@example.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:file "day-1")))
