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
