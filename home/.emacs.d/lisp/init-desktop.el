;;;; Save Desktop
(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))))

(add-hook 'auto-save-hook 'my-desktop-save)

(provide 'init-desktop)
