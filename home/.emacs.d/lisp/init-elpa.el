(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Fire up package.el
(package-initialize)

(defvar storax/packages '(ace-jump-mode
			  auctex
			  dabbrev
			  drag-stuff
                          elpy
                          epc
                          expand-region
                          flycheck
                          fold-dwim
                          helm
                          helm-swoop
                          iedit
                          magit
                          magit-gitflow
                          marmalade
                          multi
                          multi-term
                          multiple-cursors
                          org
                          pdf-tools
                          popup
                          request
                          smartparens
                          sx
                          xkcd
                          yaml-mode
                          zenburn-theme)
  "Default packages")

(defun storax/packages-installed-p ()
  (loop for pkg in storax/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (storax/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg storax/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-elpa)
