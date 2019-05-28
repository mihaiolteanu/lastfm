;;;; lastfm.lisp
(in-package :lastfm)

(defun config (&key api-key shared-secret username mpvsocket)
  (defparameter *api-key* api-key)
  (defparameter *shared-secret* shared-secret)
  (defparameter *username* username))

(load #P"~/.config/.lastfm.lisp")

(defparameter *base-url* "http://ws.audioscrobbler.com/2.0/")
(defparameter *services*
  '((:artist.getinfo       (artist)        "bio summary")
    (:artist.getsimilar    (artist limit)  "artist name")
    (:artist.gettoptags    (artist)        "tag name")
    (:artist.gettopalbums  (artist limit)  "album > name")
    (:artist.gettoptracks  (artist limit)  "track > name")
    (:artist.search        (artist limit)  "artist name")
    (:album.getinfo        (artist album)  "track > name")
    (:tag.getinfo          (tag)           "summary")
    (:tag.gettoptracks     (tag limit)     "artist > name, track > name")
    (:tag.gettopartists    (tag limit)     "artist name")
    (:user.getlovedtracks  (user limit)    "artist > name, track > name")
    (:auth.gettoken        (api_sig)       "token"))
  "List of all the Web Services supported by the Last.Fm API (see
  https://www.last.fm/api):
- The first field of each service denotes the API method.
- The second field is a list of all the parameters supported by this method.
- The last field is a string used to extract the relevant information
  from the xml response received from last.fm for this method.")

(defun service-method (service) (first service))
(defun parameters (service) (second service))
(defun query-string (service) (third service))
(defun multi-query-p (service) (find #\, (query-string service)))

(defun request-url (service param-values)
  "Build and request a last.fm service"
  (http-request *base-url*
     :parameters
     `(("api_key" . ,*api-key*)
       ("method" .  ,(symbol-name (service-method service)))
       ;; Build alists by matching up the service's
       ;; parameters with the user supplied param-values.
       ,@(mapcar (lambda (m v)
                   (cons (string-downcase (symbol-name m)) v))
                 (parameters service)
                 param-values))))

(defun lastfm-get (what &rest param-values)
  (let ((service (find what *services* :key #'first)))
    (when service
      (let* ((request (request-url service param-values))
             ;; Tell plump to parse the request as an xml
             (*tag-dispatchers* *xml-tags*)
             (query (query-string service))
             (result ($ (inline (parse request))
                       query (text))))
        ;; For top tracks, for example, the result vector contains
        ;; the artist name in its first half and the song name in its second
        (if (multi-query-p service)
            (let ((len (length result)))
              (map 'list (lambda (p1 p2)
                           (list p1 p2))
                   (subseq result 0 (/ len 2))
                   (subseq result (/ len 2) len)))
            (map 'list #'identity result))))))

(memoize 'lastfm-get)

