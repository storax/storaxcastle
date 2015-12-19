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
    (travis-get-builds (car ownerrepo) (nth 1 ownerrepo))))

;; Save travis in each buffer
(defvar travis nil)

(defun settravis ()
  "Save travis"
  (make-local-variable 'travis)
  (let ((prj (projectile-project-p)))
	(if prj
	    (setq travis (file-exists-p (concat prj ".travis.yml"))))))


(add-hook 'after-change-major-mode-hook 'settravis)

(provide 'init-travis)
