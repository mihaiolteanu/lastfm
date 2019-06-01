;;;; lastfm.lisp
(in-package :lastfm)

(defun config (&key api-key shared-secret username (sk ""))
  (defparameter *api-key* api-key)
  (defparameter *shared-secret* shared-secret)
  (defparameter *username* username)
  (defparameter *sk* sk))

(load #P"~/.config/.lastfm.lisp")

(defparameter *base-url* "http://ws.audioscrobbler.com/2.0/")
(defparameter *methods*
  '((artist-getinfo      :no-auth  (artist)        "bio summary"                 )
    (artist-getsimilar   :no-auth  (artist limit)  "artist name"                 )
    (artist-gettoptags   :no-auth  (artist)        "tag name"                    )
    (artist-gettopalbums :no-auth  (artist limit)  "album > name"                )
    (artist-gettoptracks :no-auth  (artist limit)  "track > name"                )
    (artist-search       :no-auth  (artist limit)  "artist name"                 )
    (album-getinfo       :no-auth  (artist album)  "track > name"                )
    (tag-getinfo         :no-auth  (tag)           "summary"                     )
    (tag-gettoptracks    :no-auth  (tag limit)     "artist > name, track > name" )
    (tag-gettopartists   :no-auth  (tag limit)     "artist name"                 )
    (user-getlovedtracks :no-auth  (user limit)    "artist > name, track > name" )
    (track-love          :auth     (artist track)  "lfm"                         )
    (track-unlove        :auth     (artist track)  "lfm"                         )
    (auth-gettoken       :no-auth  (api_sig)       "token"                       )
    (auth-getsession     :no-auth  (token api_sig) "session key"                 )
    ))

(defmacro build-lastfm-functions ()
  `(progn
     ,@(mapcar (lambda (method)
                 `(defun ,(method-name method) (,@(method-parameters method))
                    (lfm-request ',(method-name method) ,@(method-parameters method))))
               *methods*)))

(defun method-name (method)
  (first method))

(defun method-name-string (method)
  "A method name string in the format requested by the Last.fm API parameters"
  (string-downcase
   (substitute #\. #\- (symbol-name (method-name method)))))

(defun parameter-string (param)
  (string-downcase (symbol-name param)))

(defun auth-needed-p (method)
  (eql (second method) :auth))

(defun method-parameters (method)
  (third method))

(defun find-method-entry (name)
  (find name *methods* :key #'method-name))

(defun query-string (method)
  (fourth method))

(defun multi-query-p (query)
  "CSS selectors with ',' allow retrieving multiple tags in the same request"
  (find #\, query))

(defun param-value-list (method param-values)
  "Build the parameter/value list according to the given method and the user
supplied values."
  ;; The api key and method is needed for all calls.
  (let ((result `(("api_key" . ,*api-key*)
                  ("method" . ,(method-name-string method))
                  ;; Pair the method parameters with the user supplied values
                  ,@(mapcar (lambda (p v)
                              (cons (parameter-string p) v))
                            (method-parameters method)
                            param-values))))
    ;; For methods that require authentication, the session key also needs to be
    ;; added as a parameter together with the signature. The signature is
    ;; obtained thus: The parameter list, including the session key needs to be
    ;; sorted and transformed into a string. The shared secret is then appended
    ;; to this string. The string is then signed and then the signature
    ;; (api_sig) is added to the list of parameters.
    (if (auth-needed-p method)
        (progn (push `("sk" . ,*sk*) result)
               (setf result (sort result #'string-lessp :key #'method-name))
               (push `("api_sig" . ,(sign (concatenate 'string
                                                       (param-value-list->string result)
                                                       *shared-secret*)))
                     (cdr (last result)))
               result)
        result)))

(defun param-value-list->string (list)
  "The signing procedure for authentication needs all the parameters and values
lumped together in one big string without equal or ampersand symbols between
them."
  (format nil "~{~{~a~a~}~}"
          (mapcar (lambda (p)
                    ;; The format procedure needs a list of lists.
                    (list (first p) (rest p)))
                  list)))

(defun request-method (method param-values)
  "Make the request through the Last.fm API"
  (http-request *base-url*
                :method :post
                :parameters
                (param-value-list method
                                  param-values)))

(defun parse-request-results (html query)
  (let* ((*tag-dispatchers* *xml-tags*) ;; Tell plump to parse the request as an xml
         (result ($ (inline (parse html)) query (text))))
    ;; For top tracks, for example, the result vector contains
    ;; the artist name in its first half and the song name in its second
    ;; In either case, return a list as a result and not a vector.
    (if (multi-query-p query)
        (let ((len (length result)))
          (map 'list (lambda (p1 p2)
                       (list p1 p2))
               (subseq result 0 (/ len 2))
               (subseq result (/ len 2) len)))
        (map 'list #'identity result))))

(defun lfm-request (method-name &rest param-values)
  (let ((method (find-method-entry method-name)))
    (when method
      (parse-request-results
       (request-method method param-values)
       (query-string method)))))

;; (lfm-request :artist.gettoptracks "anathema" "3")
;; (lfm-request :track.unlove "anathema" "thin air")

(defun sign (str)
  (subseq
   (with-output-to-string (s)
     (uiop:run-program
      (format nil "echo -n ~a | md5sum" str)
      :output s))
   0 32))

(let ((token nil))
  (defun fetch-request-token ()
    (if token
        token
        (setf token
              (first (lastfm-get :auth.gettoken
                                 (sign (format nil "api_key~amethodauth.gettoken~a"
                                                   *api-key* *shared-secret*)))))))

  (defun authorize-user ()
    (uiop:run-program
     (format nil "xdg-open \"http://www.last.fm/api/auth/?api_key=~a\&token=~a\""
             *api-key* (fetch-request-token))))

  (defun fetch-web-service-session ()
    (lastfm-get :auth.getsession
                (fetch-request-token)
                (sign (format nil
                              "api_key~amethodauth.getsessiontoken~a~a"
                              *api-key*
                              (fetch-request-token)
                              *shared-secret*)))))

;; (authorize-user)
;; (fetch-web-service-session)

(defun love-track-example ()
  (http-request "http://ws.audioscrobbler.com/2.0/" 
                :method :post
                :parameters
                `(("api_key" . ,*api-key*)
                  ("artist" . "anathema")
                  ("method" . "track.love")
                  ("sk" . ,*sk*)
                  ("track" . "thin air")
                  ("api_sig" . ,(sign (format nil
                                              "api_key~aartist~amethod~ask~atrack~a~a"
                                              *api-key*
                                              "anathema"
                                              "track.love"
                                              *sk*
                                              "thin air"
                                              *shared-secret*))))))
