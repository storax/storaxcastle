;;; init-basic --- Customize stuff and thAAAngs

;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Custom Variables
;;----------------------------------------------------------------------------
(set-scroll-bar-mode 'right)
(setq scroll-margin 7 ; Softer scolling
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      tab-width 4 ; Tab with 4 spaces, no tabs
      indent-tabs-mode nil
      make-backup-files nil ; No backup files
      auto-save-default nil
      vc-follow-symlinks t ; When following sysmlinks always go to the destination
      require-final-newline t
      undo-limit 10000)

(defun storax/alternative-scrolling ()
  "Use scrolling suited for terminals."
  (setq scroll-margin 0))
(add-hook 'term-mode-hook 'storax/alternative-scrolling)

;; String doesnt require to escape all the stuff
(require 're-builder)
(setq reb-re-syntax 'string)

;;----------------------------------------------------------------------------
;; Global Builtin Modes
;;----------------------------------------------------------------------------
(global-hl-line-mode t)
(global-subword-mode 1)
(global-linum-mode 1)
(savehist-mode 1)

;;----------------------------------------------------------------------------
;; Builtin Auto Modes
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.qss$" . css-mode))

;;----------------------------------------------------------------------------
;; Aliases
;;----------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'jo 'just-one-space)
(defalias 'qrr 'query-replace-regexp)

;;----------------------------------------------------------------------------
;; Allow disabled Commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Global Keybindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c C-t") 'comment-or-uncomment-region)
(when window-system
  (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)
(global-set-key (kbd "C-M-d") 'delete-pair)

(provide 'init-basic)
;;; init-basic.el ends here
