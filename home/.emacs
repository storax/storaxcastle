(require 'package)
(package-initialize)
(elpy-enable)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;No Toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)


(load-theme 'zenburn t)

(require 'ido)
(ido-mode t)

;;speichert den letzten zustand von emacs und laedt ihn beim oeffnen wieder
(require 'desktop)
  (desktop-save-mode 1)
  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)

;;el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;;aktiviere smartparens
(smartparens-global-mode 1)


(put 'narrow-to-region 'disabled nil)

;;C-Tab für autovervollstaendigung
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;;Da ich Git benutze brauch ich kein backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;;Yasnippets
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq yas/indent-line nil)

 ;; replace a string in the current region
(defalias 'rs 'replace-string)

;; MOVE TEXT AROUND
;; ----------------
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

;;Show Linenumbers
(global-linum-mode 1)

(defun hack-isearch-kill ()
   "Push current matching string into kill ring."
   (interactive)
   (kill-new (buffer-substring (point) isearch-other-end))
   (isearch-done))

;;Copy Searchresult with M-w
(define-key isearch-mode-map (kbd "M-w") 'hack-isearch-kill)

(defun cwebber/safer-flymake-find-file-hook ()
  "Don't barf if we can't open this flymake file"
  (let ((flymake-filename
         (flymake-create-temp-inplace (buffer-file-name) "flymake")))
    (if (file-writable-p flymake-filename)
        (flymake-find-file-hook)
      (message
       (format
        "Couldn't enable flymake; permission denied on %s" flymake-filename)))))

(add-hook 'find-file-hook 'cwebber/safer-flymake-find-file-hook)

;;Flymake-cursor
(add-to-list 'load-path
              "~/.emacs.d/flymake-cursor")
(eval-after-load 'flymake '(require 'flymake-cursor))

;;Folding
(require 'fold-dwim)
(global-set-key (kbd "C-c C-h") 'fold-dwim-hide-all)
(global-set-key (kbd "C-c TAB") 'fold-dwim-toggle-selective-display)
(global-set-key (kbd "C-c C-b") 'fold-dwim-toggle)
(global-set-key (kbd "C-c C-e") 'fold-dwim-show-all)
(load-library "hideshow")
(add-hook 'python-mode-hook         'hs-minor-mode)
