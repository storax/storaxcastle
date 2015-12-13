;;;; Custom Variables
;; Softer scolling
(setq redisplay-dont-pause t
      scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Tab with 4 spaces, no tabs
(setq tab-width 4
      indent-tabs-mode nil)

;; No backup files
(setq make-backup-files nil)

;; When following sysmlinks always go to the destination
(setq vc-follow-symlinks t)

;;;; Global Builtin Modes
(global-hl-line-mode t)
(global-subword-mode 1)
(global-linum-mode 1)
(column-number-mode t)

;;;; Builtin Auto Modes
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'jo 'just-one-space)

(provide 'init-basic)
