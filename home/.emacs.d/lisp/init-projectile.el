(require-package 'projectile)

(projectile-global-mode)
(setq projectile-completion-system 'helm)

(defvar tox-history)
(defun tox (args)
  (interactive (list (read-string "Tox arguments: " (car tox-history) 'tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" args))))

(provide 'init-projectile)
