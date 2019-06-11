;;;; lastfm.asd

(asdf:defsystem :lastfm
  :description "Interface for the Last.fm API (https://www.last.fm/api/)"
  :author "Mihai Olteanu"
  :license "GPLv3"
  :version "0.1"
  :depends-on (:drakma
               :plump
               :lquery
               :defmemo
               :alexandria
               :trivial-open-browser
               :generators)
  :serial t
  :components ((:file "package") 
               (:file "lastfm"))
  :in-order-to ((test-op (test-op "lastfm/tests"))))
