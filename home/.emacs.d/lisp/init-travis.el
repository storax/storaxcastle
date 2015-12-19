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

(defun travis-last-build-status ()
  (aref (travis-builds) 0))

;; Save travis in each buffer
(defvar travis nil)

(defun setinalist (aliste key value)
  (let ((cell (assoc key list)))
    (if cell
	(setcdr cell value)
      (add-to-list 'aliste '(key . value)))))


(defun settravis ()
  "Save travis"
  (let ((prj (projectile-project-p)))
	(if (and prj (not (assoc prj travis)))
	    (setinalist travis prj (travis-last-build-status)))))

(add-hook 'after-change-major-mode-hook 'settravis)

(provide 'init-travis)
