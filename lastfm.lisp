;;;; lastfm.lisp
(in-package :lastfm)

(eval-when (compile)

(defun config (&key api-key shared-secret username (sk ""))
  (defparameter *api-key* api-key)
  (defparameter *shared-secret* shared-secret)
  (defparameter *username* username)
  (defparameter *sk* sk))

(defun load-rc-file ()
  (load #P"~/.config/.lastfm.lisp"))

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
    ;; Services that only need to be called once, to get the session key (sk)
    (auth-gettoken       :sk       ()              "token"                       )
    (auth-getsession     :sk       (token)         "session key"                 )
    ))

(defmacro build-lastfm-functions ()
  "Create a function for each of the last.fm methods and memoize the ones that
don't need authentication."
  `(progn
     ,@(mapcar (lambda (method)
                 (let ((name (method-name method))
                       (params (method-parameters method)))
                   `(,(if (or (auth-needed-p method)
                              (session-key-p method))
                          'defun
                          'defmemo)
                     ,name ,params      ;function name with its parameters
                     (lfm-request ',method ,@params))))
               *methods*)))

(defun add-sk-to-rcfile (sk)
  "Add the session key to the user config file."
  (with-open-file (config #P"~/.config/.lastfm.lisp"
                          :if-exists :overwrite
                          :direction :io)
    (file-position config 0)
    (let ((contents (read config)))
      (file-position config 0)
      (write (append contents (list :sk sk)) :stream config))))

(defun authorize-user (token)
  "Open the broswer and let the user authorize the application."
    (uiop:run-program
     (format nil "xdg-open \"http://www.last.fm/api/auth/?api_key=~a\&token=~a\""
             *api-key* token)))

(defun generate-session-key ()
  "Fetch a token, and then let the user authorize the application in his
  browser. Only after the user authorizes the application can the session key
  fetching continue. Thus, we wait until a return from the breakpoint is
  signaled by the user. After that, call last.fm again to fetch the session key,
  save it to the rc-file and reload the rc-file."
  (let ((token (first (auth-gettoken))))
    (authorize-user token)
    (break "Continue after authorization is granted in the browser")
    (let ((sk (first (auth-getsession token))))
      (add-sk-to-rcfile sk)
      (load-rc-file))))

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

(defun session-key-p (method)
  (eql (second method) :sk))

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
    (cond ((auth-needed-p method)
           (progn (push `("sk" . ,*sk*) result)
                  (setf result (sort result #'string-lessp :key #'method-name))
                  (push `("api_sig" . ,(sign (request-string result)))
                        (cdr (last result)))
                  result))
          ((session-key-p method)
           (progn (push `("api_sig" . ,(sign (request-string result)))
                        (cdr (last result)))
                  result))
          (t result))))

(defun request-string (params)
  "The signing procedure for authentication needs all the parameters and values
lumped together in one big string without equal or ampersand symbols between
them."
  (let ((str (format nil "~{~{~a~a~}~}"
                     (mapcar (lambda (p)
                               ;; The format procedure needs a list of lists.
                               (list (first p) (rest p)))
                             params))))
    (concatenate 'string str *shared-secret*)))

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

(defun lfm-request (method &rest param-values)
  (parse-request-results
   (request-method method param-values)
   (query-string method)))

(defun sign (str)
  (subseq
   (with-output-to-string (s)
     (uiop:run-program
      (format nil "echo -n ~a | md5sum" str)
      :output s))
   0 32))
)                                       ;end eval-when

(load-rc-file)
(build-lastfm-functions)
