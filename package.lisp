;;;; package.lisp

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery :defmemo :generators)
  (:import-from :alexandria :emptyp)
  (:import-from :alexandria :random-elt)
  (:import-from :alexandria :circular-list)
  (:export authorize-user
           ;; All the other exported functions are created and exported
           ;; "in-code" based on the *methods* table
           ))
