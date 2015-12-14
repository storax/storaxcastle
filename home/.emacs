;;; .emacs --- Emacs Init File

;;; Commentary:

;; This config is inspired by many others configs.
;; Thanks and sorry I forgot where I got half of the stuff.
;; Some credits i remember:
;; http://www.mygooglest.com/fni/dot-emacs.html
;; https://github.com/purcell/emacs.d
;; Thanks for the input!

;;; Code:

;;----------------------------------------------------------------------------
;; Prerequisites
;;----------------------------------------------------------------------------
(message "* --[ Loading my Emacs init file ]--")

;; Personal Info
(setq user-full-name "David Zuber"
      user-mail-address "zuber.david@gmx.de")

;; uptimes
(setq emacs-load-start-time (current-time))

;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'

;; Dir with all the actual configs
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrapping config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name ".emacs-custom.el" "~/"))
(require 'init-utils)
;; Package Installing - calls package-initialize
(require 'init-elpa)

;;----------------------------------------------------------------------------
;; Load configs for specific modes and features
;;----------------------------------------------------------------------------
(require-package 'marmalade)
(require-package 'epc)
(require 'init-basic)
(require 'init-window)
(require 'init-theme)
(require 'init-powerline)
(require 'init-fonts)
(require 'init-desktop)
(require 'init-recentf)
(require 'init-ido)
(require 'init-isearch)
(require 'init-dabbrev)
(require 'init-expand-region)
(require 'init-folding)
(require 'init-multiple-cursors)
(require 'init-drag-stuff)
(require 'init-iedit)
(require 'init-ace-jump)
(require 'init-helm)
(require 'init-elisp)
(require 'init-smartparens)
(require 'init-whitespace)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-elpy)
(require 'init-magit)
(require 'init-latex)
(require 'init-minimap)
(require 'init-spotify)
(require-package 'multi-term)
(require-package 'org)
(require-package 'xkcd)
(require-package 'sx)
(require-package 'yaml-mode)

;;----------------------------------------------------------------------------
;; Custom Faces added by Custom
;;----------------------------------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables customized via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (message "* --[ Init completed in %.2fms ]--"
;;                      (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide '.emacs)
;;; .emacs ends here
