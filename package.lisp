;;;; package.lisp

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery :defmemo)
  ;; All exported functions are created and exported "in-code" based on the
  ;; *methods* table
)
