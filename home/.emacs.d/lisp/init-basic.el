;;;; Custom Variables
;; Tab with 4 spaces, no tabs
(setq tab-width 4
      indent-tabs-mode nil)

;; No backup files
(setq make-backup-files nil)

;; When following sysmlinks always go to the destination
(setq vc-follow-symlinks t)

;;;; Global Builtin Modes
(global-subword-mode 1)
(global-linum-mode 1)

;;;; Builtin Auto Modes
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'jo 'just-one-space)

(provide 'init-basic)
