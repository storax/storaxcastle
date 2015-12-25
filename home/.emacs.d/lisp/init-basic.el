;;; init-basic --- Customize stuff and thAAAngs

;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Custom Variables
;;----------------------------------------------------------------------------
;; Softer scolling
(setq scroll-margin 7
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
(setq require-final-newline t)
(setq undo-limit 10000)

;; String doesnt require to escape all the stuff
(require 're-builder)
(setq reb-re-syntax 'string)

;;----------------------------------------------------------------------------
;; Custom Functions
;;----------------------------------------------------------------------------

(defun storax/swap-place-in-region ()
  "Go to the other end of the current region.

Set a mark before moving.
If a region is active this acts like swap places."
  (interactive)
  (let ((next (if (equal (point) (region-beginning))
		  (region-end)
		(region-beginning))))
    (set-mark-command nil)
    (goto-char next)
    (setq deactivate-mark nil)))

;;----------------------------------------------------------------------------
;; Global Builtin Modes
;;----------------------------------------------------------------------------
(global-hl-line-mode t)
(global-subword-mode 1)
(global-linum-mode 1)

;;----------------------------------------------------------------------------
;; Builtin Auto Modes
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

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
(global-set-key (kbd "C-M-.") 'storax/swap-place-in-region)
(global-set-key (kbd "C-M-,") 'storax/swap-place-in-region)

(provide 'init-basic)
;;; init-basic.el ends here
