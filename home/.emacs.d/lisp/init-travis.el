(require 'url)

(defun travis-get-builds-deferred (prj owner repository)
  "Get build status deferred."
  (lexical-let ((proj prj))
    (let ((a-url (format "https://api.travis-ci.org/repos/%s/%s/builds" owner repository))
	  (cb (lambda (stuff)
		(goto-char url-http-end-of-headers)
		(let ((j (json-read)))
		  (travis-set-status proj (aref j 0))))))
      (url-retrieve a-url cb))))

(defun get-owner-repo ()
  "Get owner and repo"
  (let ((remote (magit-get "remote" "origin" "url")))
	(let ((match (string-match "github.com\\(:\\|/\\)\\(.+\\)/\\([^\\\\.]+\\)\\(\\\\.git\\)?" remote)))
	  (if match
	      (list (substring remote (match-beginning 2) (match-end 2))
		    (substring remote (match-beginning 3) (match-end 3)))))))

;; Save travis in each buffer
(defvar travis-statuse nil)

(defun travis-set-status (key value)
  (let ((cell (assoc key travis-statuse)))
    (if (not cell)
      (add-to-list 'travis-statuse (cons key value)))))

(defun travis-fetch-status ()
  "Save travis status"
  (let ((prj (projectile-project-p))
	(status (assoc (projectile-project-p) travis-statuse))
	(ownerrepo (get-owner-repo)))
    (if prj
	(if (not status)
	    (if (file-exists-p (concat prj ".travis.yml"))
		(travis-get-builds-deferred prj (car ownerrepo) (nth 1 ownerrepo))
	      (travis-set-status prj "NOTRAVIS"))))))

(defun travis-get-status ()
  "Get travis status"
  (let ((status (assoc (projectile-project-p) travis-statuse)))
    (if (and status (not (equal (cdr status) "NOTRAVIS")))
	status)))

(defun travis-add-hook ()
  (add-hook 'find-file-hook 'travis-fetch-status))

(add-hook 'after-init-hook 'travis-add-hook)

(provide 'init-travis)
