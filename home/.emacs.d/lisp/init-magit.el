;;Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Magit Readme buffer
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(provide 'init-magit)
