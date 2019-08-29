;;;; lastfm.lisp
(in-package :lastfm)

(eval-when (:compile-toplevel :execute :load-toplevel)

(defun config (&key api-key shared-secret username (sk ""))
  "This function is called from the user config file once it is loaded."
  (defparameter *api-key* api-key)
  (defparameter *shared-secret* shared-secret)
  (defparameter *username* username)
  (defparameter *sk* sk))

(defun load-rc-file ()
  "Load the config file. Create it with empty strings if the file does not
  exist. The user will have to update this file, otherwise, all calls willl
  return NIL."
  (let ((config-file (merge-pathnames ".lastfmrc"
                                      (xdg-config-home))))
    (if (file-exists-p config-file)
        (load config-file)
        (with-open-file (file config-file
                              :if-does-not-exist :create
                              :direction :output)
          (file-position file 0)
          (write '(CONFIG
                   :API-KEY ""
                   :SHARED-SECRET ""
                   :USERNAME "")
                 :stream file)))))

(defparameter *base-url* "http://ws.audioscrobbler.com/2.0/")
(defparameter *methods*
  '(;; Album
    (album-getinfo       :no-auth  (artist album)  "track > name"                )
    ;; Artist
    (artist-getinfo      :no-auth  (artist)        "bio summary"                 )
    (artist-getsimilar   :no-auth  (artist limit)  "artist name"                 )
    (artist-gettoptags   :no-auth  (artist)        "tag name"                    )
    (artist-gettopalbums :no-auth  (artist limit)  "album > name"                )
    (artist-gettoptracks :no-auth  (artist limit)  "track > name"                )
    (artist-search       :no-auth  (artist limit)  "artist name"                 )
    ;; Auth (only need to be called once, to get the session key (sk))
    (auth-gettoken       :sk       ()              "token"                       )
    (auth-getsession     :sk       (token)         "session key"                 )
    ;; Tag
    (tag-getinfo         :no-auth  (tag)           "summary"                     )
    (tag-gettoptracks    :no-auth  (tag limit)     "artist > name, track > name" )
    (tag-gettopartists   :no-auth  (tag limit)     "artist name"                 )
    ;; Track
    (track-love          :auth     (artist track)  "lfm"                         )
    (track-unlove        :auth     (artist track)  "lfm"                         )
    (track-scrobble      :auth     (artist track timestamp)  "lfm"               )
    ;; User
    (user-getlovedtracks :no-auth  (user limit)    "artist > name, track > name" )
    ))

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

(defun build-lastfm-function (method)
  "Create and export a function for the last.fm method and memoize it if it
doesn't need authentication."
  (let ((name (method-name method))
        (params (method-parameters method)))
    `(progn (,(if (or (auth-needed-p method)
                      (session-key-p method))
                  'defun
                  'defmemo)
             ,name ,params      ;function name with its parameters
             (lfm-request ',method ,@params))
            (export ',(method-name method)))))

(defmacro build-lastfm-functions ()
  "Create all the last.fm functions."
  `(progn
     ,@(mapcar (lambda (method)
                 (build-lastfm-function method))
               *methods*)))

(defun add-sk-to-rcfile (sk)
  "Add the session key to the user config file."
  (with-open-file (config-file
                   (merge-pathnames ".lastfmrc"
                                    (xdg-config-home))
                   :if-exists :overwrite
                   :direction :io)
    (file-position config-file 0)
    (let ((contents (read config-file)))
      (file-position config-file 0)
      (write (append contents (list :sk sk)) :stream config-file))))

(defun authorize-user (token)
  "Ask the user to authorize the application."
  (open-browser
   (format nil "http://www.last.fm/api/auth/?api_key=~a\&token=~a"
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

(defun param-value-list (method param-values)
  "Build the parameter/value list according to the given method and the user
supplied values."
  ;; The api key and method is needed for all calls.
  (let ((result `(("api_key" . ,*api-key*)
                  ("method" . ,(method-name-string method))
                  ;; Pair the method parameters with the user supplied values
                  ,@(mapcar (lambda (p v)
                              (cons (parameter-string p)
                                    ;; Make sure the value is a string
                                    (or (and (stringp v) v)
                                        (write-to-string v))))
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
them, and with the shared secret appended to the end of this string."
  (let ((str (format nil "~{~{~a~a~}~}"
                     (mapcar (lambda (p)
                               ;; The format procedure needs a list of lists.
                               (list (first p) (rest p)))
                             params))))
    (concatenate 'string str *shared-secret*)))

(defun request-method (method param-values &key (retries 3))
  "Make the request through the Last.fm API"
  (let ((resp (http-request *base-url*
                             :method :post
                             :external-format-out :utf-8
                             :parameters
                             (param-value-list method param-values))))
    ;; Sometimes, the response is nil. Try again.
    (unless (and resp (> retries 0))
      (request-method method param-values :retries (- retries 1)))
    ;; Sometimes, the exact same last.fm api call returns a 500 error, saying
    ;; the length is too big. I've tried changing the non-auth metods calls to
    ;; :get instead of :post, but then the sporadic error is different, saying
    ;; "this get method does not exists" or something similar. I guess this is
    ;; some last.fm error. If if does happen, then the response page will have a
    ;; title tag with the error code. If that's the case, we'll retry the call
    ;; for a few times.
    (if (emptyp ($ (inline (parse resp)) "title" (text)))
        resp                            ;Valid response; return it
        (if (> retries 0)
            (request-method method param-values :retries (- retries 1))
            nil))))

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
  (byte-array-to-hex-string
   (digest-sequence :md5
                    (ascii-string-to-byte-array str))))

(load-rc-file))                         ;end eval-when

;; Build the functions after everything else has already loaded.
(build-lastfm-functions)


;;; Extra functionality not covered by the last.fm API

(defmemo song-youtube-url (artist song)
  "Since there is no youtube link available through the last.fm API,
try and get it from the last.fm song's page."
  (let* ((url (format nil "https://www.last.fm/music/~a/_/~a"
                      (substitute #\+ #\Space artist)
                      (substitute #\+ #\Space song)))
         (request (http-request url))
         (links ($ (inline (plump:parse request))
                  "[data-playlink-affiliate]" (attr "data-youtube-url") )))
    (if (> (length links) 0)
        ;; Two identical links are available on the page.
        (aref links 0)
        ;; This song has no youtube link information.
        nil)))

(defun random-artist-song (artist &optional (limit 20))
  (random-elt (artist-gettoptracks artist limit)))

(defun random-similar-artist (artist &optional (limit 20))
  (random-elt (artist-getsimilar artist limit)))

(defun random-user-loved-song (user &optional (limit 20))
  (random-elt (user-getlovedtracks user limit)))

(defun random-tag-song (tag &optional (limit 20))
  (random-elt (tag-gettoptracks tag limit)))

(defun random-tag-artist (tag &optional (limit 20))
  (random-elt (tag-gettopartists tag limit)))

(defun create-generator (fn name nitems random &key yield-name)
  (if random
      (make-generator ()
        (loop for item = (random-elt (funcall fn name nitems))
                then (random-elt (funcall fn name nitems))
              do (yield (if yield-name
                            (list name item)
                            item))))
      (make-generator ()
        (loop for item in (apply #'circular-list (funcall fn name nitems))
              do (yield (if yield-name
                            (list name item)
                            item))))))

(defun artist-songs (artist nsongs random)
  "Return an infinite songs generator. If random is T, every new song is picked
at random from the artists' first best nsongs of all time, as seen on the
artist's last.fm page. If random is nil, the songs are picked in order. After
the last song, the first song is returned again, ad infinitum."
  (create-generator #'artist-gettoptracks artist nsongs random
                    :yield-name T))

(defun tag-songs (tagname nsongs random)
  (create-generator #'tag-gettoptracks tagname nsongs random
                    :yield-name nil))

(defun user-songs (username nsongs random)
  "Return a generator with songs from a user of your choice."
  (create-generator #'user-getlovedtracks username nsongs random
                    :yield-name nil))

(defun my-loved-songs (nsongs random)
  "Return a generator with the current user loved songs. The username is the one
specified in the .lastfmrc config file."
  (user-songs *username* nsongs random))

(defun create-double-generator (artist-fn name nartists nsongs)
  (make-generator ()
    (do* ((artist (random-elt (funcall artist-fn name nartists))
                  (random-elt (funcall artist-fn name nartists)))
          (song (random-elt (artist-gettoptracks artist nsongs))
                (random-elt (artist-gettoptracks artist nsongs))))
         ((or (null artist) (null song)))
      (yield (list artist song)))))

(defun artist-similar-artists-songs (artist nartists nsongs)
  "Return an infinite songs generator. Every new song is picked by first
  selecting a random artist from the first nartists similar to the given artist,
  according to the last.fm info. From this random artist a random song is picked
  from the first best nsongs according to the last.fm info. When called, the
  generator returns a list of two items, an artist and a song."
  (create-double-generator #'artist-getsimilar artist nartists nsongs))

(defun tag-similar-artists-songs (tag nartists nsongs)
  (create-double-generator #'tag-gettopartists tag nartists nsongs))

(defun album-songs (artist album)
  "Return a generator with all the songs on the artist's album."
  (make-generator ()
    (loop for song in (apply #'circular-list (album-getinfo artist album))
          do (yield (list artist song)))))

