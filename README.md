# lastfm
Interface for the [last.fm](www.last.fm) [API](www.last.fm/api), including the
services that need authentication and song generators for extra functionality.

# Installation

## Step one - installing the library and the dependencies

```bash
# clone to local-projects for quickload access
git clone https://github.com/mihaiolteanu/lastfm ~/quicklisp/local-projects/lastfm

# API calls are signed with an external application
sudo apt-get md5sum
```

## Step two - obtain the last.fm API key
To use this library, a last.fm API Key is needed. For that you need a last.fm
account and then an API account. Follow the instructions from the official
[documentation](https://www.last.fm/api) page and you will receive and api-key
and a shared secret. Write them down.

## Step three - create or update the config file with the API key
Create a config file, `~/.config/.lastfm.lisp`, with the info received from
last.fm on step one.
```common-lisp
;; ~/.config/.lastfm.lisp
(CONFIG
 :API-KEY "yout-api-key-string"
 :SHARED-SECRET "your-shared-secret-string"
 :USERNAME "your-last-fm-username")
```
One thing missing from this config file is the secret key (SK) which will be
added by this library after the authentication process (see below) is
completed. The authentication only needs to be done once.

## Step four - generate the session key

Load the library and generate the session key by calling the appropriate interface.
```common-lisp
(ql:quickload :lastfm)
(lastfm:generate-session-key)
```
This will open up the [last.fm/api/auth](last.fm/api/auth) page in your
 favorite browser and put a breakpoint in the code (My app is called muse, in
 this case. Yours might differ).
 
 <table><tr><td>
 <img
 src="https://user-images.githubusercontent.com/8273519/59293481-f4614600-8c87-11e9-9320-7f97bb135c44.png"
 alt="grant permission to last.fm" width="600px"/>
</td></tr></table>
 
You will need to grant this
 `lastfm` library permission to use your last.fm account ([step
 3](https://www.last.fm/api/desktopauth) in the official last.fm authentication
 process). 
 
 <table><tr><td>
 <img
 src="https://user-images.githubusercontent.com/8273519/59293493-f7f4cd00-8c87-11e9-8ea3-ebee572bf53f.png"
 alt="permission granted" width="600px"/>
</td></tr></table>
 
After that, return to your editor (Emacs) and continue from breakpoint.
 
 <img
 src="https://user-images.githubusercontent.com/8273519/59293499-fa572700-8c87-11e9-87a6-0cdaea1efbb9.png"
 alt="continue from breakpoint"/>
 
If this step is succesful, the secret key will be added to your
 config file, from step three, which should now look like this:
```common-lisp
;; ~/.config/.lastfm.lisp
(CONFIG
 :API-KEY "yout-api-key-string"
 :SHARED-SECRET "your-shared-secret-string"
 :USERNAME "your-last-fm-username"
 :SK "your-secret-key-generated-at-step-four")
``` 

If you don't follow this step, the lastfm services that need authentication
(love/unlove track, scrobble track) won't work.

# Usage

```common-lisp
; Get the first top tracks for the given artist.
(artist-gettoptracks "anathema" 5)
    => ("Fragile Dreams" "One Last Goodbye" "A Natural Disaster" "Flying" "Deep")
```

```common-lisp
; Get the best ten artist from the 80s.
(tag-gettopartists "80s" 10)
    => ("Duran Duran" "a-ha" "Hall & Oates" "Cyndi Lauper" "Eurythmics" "Erasure"
        "Wham!" "Alphaville" "Men at Work" "Bonnie Tyler")
```

```common-lisp
; If step four was done, this will add the song to your last.fm loved tracks.
(track-love "alphaville" "forever young")
```

```common-lisp
;; Extra functionality not covered by the last.fm API
(ql:quickload :generators)

; Get a generator with the first 5 toptracks from the artist
(defparameter *anathema*
  (artist-songs "anathema" 5 T))
  
; Calling next on the generator will return a random song. The list is
; infinite. If the random parameter is nil instead of T, the generator is
; circular, but still infinite
(next *anathema*)
    => ("anathema" "Fragile Dreams")
(next *anathema*)
    => ("anathema" "One Last Goodbye")
(next *anathema*)
    => ("anathema" "Fragile Dreams") 
```

# API

## last.fm API interfaces
The following last.fm API interfaces are implemented by this library. `limit`
means the number of items to return. Browse the official [last.fm
API](www.last.fm/api) page for further details. All interfaces that don't need
authentication are memoized. A second call with the exact same parameters will
be much faster and it won't result in a fresh last.fm request.

**album-getinfo** _artist album_

**artist-getinfo** _artist_

**artist-getsimilar** _artist limit_

**artist-gettoptags** _artist_

**artist-gettopalbums** _artist limit_

**artist-gettoptracks** _artist limit_

**artist-search** _artist limit_

**tag-getinfo** _tag_

**tag-gettoptracks** _tag limit_

**tag-gettopartists** _tag limit_

**user-getlovedtracks** _user limit_

**track-love** _artist track_
    
    Authentication needed (step 4)

**track-unlove** _artist track_

    Authentication needed (step 4)

**track-scrobble** _artist track timestamp_ 

    Authentication needed (step 4)
    Timestamp must be in UNIX timestamp format. For example
    
    (ql:quickload :local-time)
    (track-scrobble "anathema" "one last goodbye"
        (local-time:timestamp-to-unix (local-time:now)))

## Random items
Extra useful functionality not covered by the last.fm API, but built on top of
it and that you might find useful.

**song-youtube-url** _artist song_

    Since there is no youtube link available through the last.fm API,
    try and get it from the last.fm song's page.

**random-artist-song** _artist &optional (limit 20)_

**random-similar-artist** _artist &optional (limit 20)_

**random-user-loved-song** _user &optional (limit 20)_

**random-tag-song** _tag &optional (limit 20)_

**random-tag-artist** _tag &optional (limit 20)_

## Generators
Generators. These will return generators that can be used by calling next on
them. On each call, a new item is received. If the `random` parameter, where
available, is specified as T, a random elemenent is received on each
call. Otherwise, the elements are returned in order, as they appear on their
respective last.fm page. After the last element is returned, the `next` call
will again return the first one (i.e. the generator is recursive). You need to
use the [generators](http://quickdocs.org/generators/) library for
that. `nparameters` specify the number of elements to be taken into
consideration, similar to the `limit` parameter in the last.fm API case.

**artist-songs** _artist nsongs random_

**artist-album-songs** _artist album_

    Return a non-random generator with all the songs on the artist's album.

**tag-songs** _tagname nsongs random_

**user-songs** _username nsongs random_

**artist-similar-artists-songs** _artist nartists nsongs_

    Each call will first pick a random artist from the list of similar artists
    and then pick a random song from this chosen artist. This is a random generator.

**tag-similar-artists-songs** _tag nartists nsongs_

    Each call will first pick a random artist from the list of top artists for this tag
    and then pick a random song from this chosen artist. This is a random generator.

## Authors
Copyright (c) 2019 [Mihai Olteanu](www.mihaiolteanu.me)

Licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.
