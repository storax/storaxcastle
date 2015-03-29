
;;;; Header
(require 'cl)

(setq user-full-name "David Zuber"
      user-mail-address "zuber.david@gmx.de")


;;;; Packages
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))



(defvar storax/packages '(dabbrev
			  elpy
			  expand-region
			  flycheck
			  fold-dwim
                          magit
			  magit-gitflow
                          marmalade
                          org
                          smartparens
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


(setq custom-file "~/.emacs-custom.el")
(load custom-file)



;;;; Window and Visual Settings
;; No spashscreen, scratch message and default python mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'python-mode)

;; Hide tool and menu bar
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1))

;; Choose theme depending on window or terminal
(if window-system
    (load-theme 'zenburn t)
  (load-theme 'wombat t))

;; Font settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 135
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "DejaVu Sans Mono"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))

;; Show little dashes to indicate empy lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


;;;; Custom Variables
;; Tab with to 4, no tabs
(setq tab-width 4
      indent-tabs-mode nil)

;; No backup files
(setq make-backup-files nil)


;;;; Aliases
;; answer with y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

 ;; replace a string in the current region
(defalias 'rs 'replace-string)


;;;; Modes
(global-subword-mode 1)
(smartparens-global-mode 1)
(ido-mode t)
(elpy-enable)
(setq elpy-modules '(elpy-module-eldoc
		     elpy-module-pyvenv
		     elpy-module-highlight-indentation
		     elpy-module-yasnippet
		     elpy-module-sane-defaults))
(global-linum-mode 1)
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/indent-line nil)
(add-hook 'after-init-hook #'global-flycheck-mode)


;;;; Key bindings
;;Python mode move around code blocks
(global-set-key (kbd "M-p") 'python-nav-backward-block)
(global-set-key (kbd "M-n") 'python-nav-forward-block)


;;C-Tab für autovervollstaendigung
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;;Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;Expand Region
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;;Copy Searchresult with M-w
(defun hack-isearch-kill ()
   "Push current matching string into kill ring."
   (interactive)
   (kill-new (buffer-substring (point) isearch-other-end))
   (isearch-done))
(define-key isearch-mode-map (kbd "M-w") 'hack-isearch-kill)

;;Folding
(require 'fold-dwim)
(global-set-key (kbd "C-c C-h") 'fold-dwim-hide-all)
(global-set-key (kbd "C-c TAB") 'fold-dwim-toggle-selective-display)
(global-set-key (kbd "C-c C-b") 'fold-dwim-toggle)
(global-set-key (kbd "C-c C-e") 'fold-dwim-show-all)
(load-library "hideshow")

;; MOVE TEXT AROUND
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)



;;;; Save Desktop
(require 'desktop)
  (desktop-save-mode 1)
  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))


;;;; Safe flymake find file hook
(defun cwebber/safer-flymake-find-file-hook ()
  "Don't barf if we can't open this flymake file"
  (let ((flymake-filename
         (flymake-create-temp-inplace (buffer-file-name) "flymake")))
    (if (file-writable-p flymake-filename)
        (flymake-find-file-hook)
      (message
       (format
        "Couldn't enable flymake; permission denied on %s" flymake-filename)))))



;;;; Yas-Snippet Menu
;;; use popup menu for yas-choose-value
(require 'popup)

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))


;;;; Hooks
(add-hook 'find-file-hook 'cwebber/safer-flymake-find-file-hook)
(add-hook 'auto-save-hook 'my-desktop-save)
(add-hook 'python-mode-hook         'hs-minor-mode)
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
