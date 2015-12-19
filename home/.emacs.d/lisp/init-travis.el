(require 'url)

(defun travis-get-builds (owner repository)
  "Get build status of owner/repository from travis-ci"
  (let ((a-url (format "https://api.travis-ci.org/repos/%s/%s/builds" owner repository)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun get-owner-repo ()
  "Get owner and repo"
  (let ((remote (magit-get "remote" "origin" "url"))
	(match (string-match "github.com\\(:\\|/\\)\\(.*\\)/\\(.*\\)\\(.git\\)?" (magit-get "remote" "origin" "url"))))
    (if match
	(list (substring remote (match-beginning 2) (match-end 2))
	      (substring remote (match-beginning 3) (match-end 3))))))

(defun travis-builds ()
  (let ((ownerrepo (get-owner-repo)))
    (if ownerrepo
	(travis-get-builds (car ownerrepo) (nth 1 ownerrepo)))))

(defun travis-last-build-status ()
  (aref (travis-builds) 0))

;; Save travis in each buffer
(defvar travis-statuse nil)

(defun travis-set-status (key value)
  (let ((cell (assoc key travis-statuse)))
    (if (not cell)
      (add-to-list 'travis-statuse (cons key value)))))

(defun travis-fetch-status ()
  "Save travis status"
  (let ((prj (projectile-project-p))
	(status (assoc (projectile-project-p) travis-statuse)))
    (if prj
	(if (not status)
	    (if (file-exists-p (concat prj ".travis.yml"))
		(travis-set-status prj (travis-last-build-status))
	      (travis-set-status prj "NOTRAVIS"))))))

(defun travis-get-status ()
  "Get travis status"
  (let ((status (assoc (projectile-project-p) travis-statuse)))
    (if (and status (not (equal (cdr status) "NOTRAVIS")))
	status)))

(add-hook 'find-file-hook 'travis-fetch-status)

(provide 'init-travis)
