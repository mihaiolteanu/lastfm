;;;; package.lisp

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery :fare-memoization)
  (:export lastfm-get))
