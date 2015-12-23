;;; init-spotify-el --- Control spotify

;;; Commentary:

;;; Code:
(add-to-list 'load-path (expand-file-name "spotify.el" user-emacs-directory))
(require 'spotify)

;; Do not use values larger than 50 for better compatibility across endpoints
(setq spotify-api-search-limit 50)

(defun storax/spotify-connect ()
  "Start a new Spotify session."
  (interactive)
  (require 'init-secrets)
  (spotify-connect))

(provide 'init-spotify-el)
;;; init-spotify-el ends here
