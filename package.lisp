;;;; package.lisp

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery :defmemo :generators)
  (:import-from :alexandria :emptyp)
  (:import-from :alexandria :random-elt)
  (:import-from :alexandria :circular-list)
  (:import-from :uiop :file-exists-p)
  (:import-from :uiop :run-program)
  (:export generate-session-key
           ;; Aux functionality
           song-youtube-url
           random-artist-song
           random-similar-artist
           random-user-loved-song
           random-tag-song
           random-tag-artist
           ;; Generators
           artist-songs
           artist-album-songs
           tag-songs
           user-songs
           artist-similar-artists-songs
           tag-similar-artists-songs
           ;; All the other exported functions are created and exported
           ;; "in-code" based on the *methods* table
           ))
