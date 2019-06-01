;;;; package.lisp

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery :defmemo)
  (:export lastfm-get))
