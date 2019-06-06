;;;; package.lisp

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery :defmemo)  
  (:export authorize-user
           ;; All the other exported functions are created and exported
           ;; "in-code" based on the *methods* table
           ))
