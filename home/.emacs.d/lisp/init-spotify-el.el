;;; init-spotify-el --- Control spotify

;;; Commentary:

;;; Code:
(require 'url)
(require 'json)
(require 'helm)
(add-to-list 'load-path (expand-file-name "spotify.el" user-emacs-directory))
(require 'spotify)

;; Do not use values larger than 50 for better compatibility across endpoints
(setq spotify-api-search-limit 50)
(setq spotify-transport 'dbus)

(defvar storax/spotify-helm-current-playlist nil
  "Current playlist if viewing tracks of playlist.")

(defun storax/start-spotify ()
  "Start spotify."
  (interactive)
  (let ((process-connection-type nil))
    (async-shell-command "pgrep \"spotify\" > /dev/null || spotify &; exit 0"))
  (storax/spotify-connect)
  (global-spotify-remote-mode t))

(defun storax/spotify-connect ()
  "Start a new Spotify session."
  (interactive)
  (require 'init-secrets)
  (spotify-connect))

(defun storax/spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-track (alist-get '(href) track)))

(defun storax/spotify-play-track-hash (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-track (gethash 'uri track)))

(defun storax/spotify-play-track-album (track)
  "Get the Spotify app to play the album of TRACK."
  (spotify-play-track (alist-get '(album href) track)))

(defun storax/spotify-play-playlist (playlist)
  "Get the Spotify app to play the PLAYLIST."
  (spotify-play-track (gethash 'uri playlist)))

(defun storax/spotify-helm-playlist-tracks (playlist)
  "Go to the tracks of PLAYLIST."
  (setq storax/spotify-helm-current-playlist playlist)
  (storax/spotify-helm--playlist-tracks))

(defun storax/spotify-helm-get-playlist-tracks ()
  "Go to the tracks of the current playlist."
  (mapcar (lambda (track)
	    (cons (storax/spotify-format-track-hash (gethash 'track track))
		  (gethash 'track track)))
	  (gethash 'items
  (spotify-api-call "GET"
		    (car (cdr (split-string
			  (gethash 'href
				   (gethash 'tracks
					    storax/spotify-helm-current-playlist))
			  spotify-api-endpoint)))))))

(defun storax/spotify-track-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (format "http://ws.spotify.com/search/1/track.json?q=%s" search-term)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun storax/spotify-playlist-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (spotify-api-call "GET"
		    (format "/search?q=%s&type=playlist&limit=%d&market=from_token"
		    (url-hexify-string search-term) spotify-api-search-limit)))

(defun storax/spotify-my-playlists ()
  "Get my Spotify playlists, returning the results as a Lisp structure."
  (spotify-api-call "GET"
		    (format "/me/playlists?limit=%d" spotify-api-search-limit)))

(defun storax/spotify-format-track (track)
  "Given a TRACK, return a formatted string suitable for display."
  (let ((track-name   (alist-get '(name) track))
	(track-length (alist-get '(length) track))
	(album-name   (alist-get '(album name) track))
	(artist-names (mapcar (lambda (artist)
				(alist-get '(name) artist))
			      (alist-get '(artists) track))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60) (mod track-length 60)
	    (mapconcat 'identity artist-names "/")
	    album-name)))

(defun storax/spotify-format-track-hash (track)
  "Given a TRACK, return a formatted string suitable for display."
  (let ((track-name   (gethash 'name track))
	(track-length (gethash 'duration_ms track))
	(album-name   (gethash 'name (gethash 'album track)))
	(artist-names (mapcar (lambda (artist)
				(gethash 'name artist))
			      (gethash 'artists track))))
    (message "%s %dm%0.2ds %s %s %s" track-name (/ track-length 60000) (/ (mod track-length 60000) 1000) album-name (mapconcat 'identity artist-names "/") album-name)
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60000) (/ (mod track-length 60000) 1000)
	    (mapconcat 'identity artist-names "/")
	    album-name)))

(defun storax/spotify-format-playlist (playlist)
  "Given a PLAYLIST, return a formatted string suitable for display."
  (let ((playlist-name (gethash 'name playlist))
	(tracks-count (gethash 'total (gethash 'tracks playlist)))
	(user (gethash 'id (gethash 'owner playlist))))
    (format "%s\nTracks: %4d | User: %s"
	    playlist-name
	    tracks-count
	    user)))

(defun storax/spotify-track-search-formatted (search-term)
  "Search tracks with SEARCH-TERM and format the result."
  (mapcar (lambda (track)
	    (cons (storax/spotify-format-track track) track))
	  (gethash 'tracks (storax/spotify-track-search search-term))))

(defun storax/spotify-playlist-search-formatted (search-term)
  "Search playlists with SEARCH-TERM and format the result."
  (mapcar (lambda (playlist)
	    (cons (storax/spotify-format-playlist playlist) playlist))
	  (gethash 'items (gethash 'playlists (storax/spotify-playlist-search search-term)))))

(defun storax/spotify-my-playlists-formatted ()
  "Search playlists with SEARCH-TERM and format the result."
  (mapcar (lambda (playlist)
	    (cons (storax/spotify-format-playlist playlist) playlist))
	  (gethash 'items (storax/spotify-my-playlists))))

(defun storax/spotify-helm-search-tracks ()
  "Return a formatted list."
  (storax/spotify-track-search-formatted helm-pattern))

(defun storax/spotify-helm-search-playlists ()
  "Return a formatted list."
  (storax/spotify-playlist-search-formatted helm-pattern))

(defun storax/spotify-helm-get-my-playlists ()
  "Return a formatted list."
  (storax/spotify-my-playlists-formatted))

(defun storax/spotify-helm-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track)) . storax/spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . storax/spotify-play-track-album)
    ("Show Track Metadata" . pp)))

(defun storax/spotify-helm-actions-for-track-hash (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (gethash 'name track)) . storax/spotify-play-track-hash)
    (,(format "Play Album - %s" (gethash 'name (gethash 'album track))) . storax/spotify-play-track-album)
    ("Show Track Metadata" . pp)))

(defun storax/spotify-helm-actions-for-playlist (actions playlist)
  "Return a list of helm ACTIONS available for this PLAYLIST."
  `((,(format "Play Playlist - %s" (gethash 'name playlist)) . storax/spotify-play-playlist)
    (,(format "Look at Tracks of - %s" (gethash 'name playlist)) . storax/spotify-helm-playlist-tracks)
    ("Show Track Metadata" . pp)))

;;;###autoload
(defvar storax/spotify-helm-track-search-source
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . storax/spotify-helm-search-tracks)
    (action-transformer . storax/spotify-helm-actions-for-track)))

;;;###autoload
(defun storax/spotify-helm-tracks()
  "Search Spotify tracks with helm."
  (interactive)
  (helm :sources '(storax/spotify-helm-track-search-source)
	:buffer "*Spotify: Search tracks*"))

;;;###autoload
(defvar storax/spotify-helm-playlist-search-source
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . storax/spotify-helm-search-playlists)
    (action-transformer . storax/spotify-helm-actions-for-playlist)))

;;;###autoload
(defun storax/spotify-helm-playlists()
  "Search Spotify playlists with helm."
  (interactive)
  (helm :sources '(storax/spotify-helm-playlist-search-source)
	:buffer "*Spotify: Search playlists*"))

;;;###autoload
(defvar storax/spotify-helm-my-playlists-source
  '((name . "Spotify")
    (multiline)
    (candidates-process . storax/spotify-helm-get-my-playlists)
    (action-transformer . storax/spotify-helm-actions-for-playlist)))

;;;###autoload
(defun storax/spotify-helm-my-playlists ()
  "Show my Spotify playlists with helm."
  (interactive)
  (helm :sources '(storax/spotify-helm-my-playlists-source)
	:buffer "*Spotify: My playlists*"))

(defvar storax/spotify-helm-playlist-tracks-source
  '((name . "Spotify")
    (multiline)
    (candidates-process . storax/spotify-helm-get-playlist-tracks)
    (action-transformer . storax/spotify-helm-actions-for-track-hash)))

(defun storax/spotify-helm--playlist-tracks ()
  "Show my Spotify playlists with helm."
  (helm :sources '(storax/spotify-helm-playlist-tracks-source)
	:buffer (format "*Spotify: Tracks of playlists: %s *"
			(gethash 'name storax/spotify-helm-current-playlist))))

(provide 'init-spotify-el)
;;; init-spotify-el ends here
