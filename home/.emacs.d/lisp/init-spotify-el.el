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

(defun storax/spotify-connect ()
  "Start a new Spotify session."
  (interactive)
  (require 'init-secrets)
  (spotify-connect))

(defun storax/spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-track (alist-get '(href) track)))

(defun storax/spotify-play-track-album (track)
  "Get the Spotify app to play the album of TRACK."
  (spotify-play-track (alist-get '(album href) track)))

(defun storax/spotify-track-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (format "http://ws.spotify.com/search/1/track.json?q=%s" search-term)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

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

(defun storax/spotify-track-search-formatted (search-term)
  "Search tracks with SEARCH-TERM and format the result."
  (mapcar (lambda (track)
	    (cons (storax/spotify-format-track track) track))
	  (alist-get '(tracks) (storax/spotify-track-search search-term))))

(defun storax/spotify-helm-search-tracks ()
  "Return a formatted list."
  (storax/spotify-track-search-formatted helm-pattern))

(defun storax/spotify-helm-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track)) . storax/spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . storax/spotify-play-track-album)
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

(provide 'init-spotify-el)
;;; init-spotify-el ends here
