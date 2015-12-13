(elpy-enable)

;; Only use these modules
(setq elpy-modules '(elpy-module-eldoc
		     elpy-module-pyvenv
		     elpy-module-highlight-indentation
		     elpy-module-yasnippet
		     elpy-module-sane-defaults))

;;Python indent right
;;set in python-mode. yas fallback has to be call-other-command.
;;the default python-indent-region sucks IMO
(defun shift-or-indent (&optional ARG)
  (interactive "P")
  (if mark-active
      (python-indent-shift-right (region-beginning)
				 (region-end))
    (indent-for-tab-command ARG)))

;;C-Tab für autovervollstaendigung
;;If region is active shift region left (in python mode)
;;if not dabbrev expand
(defun dabbrev-or-indent-left ()
  (interactive)
  (if mark-active
      (python-indent-shift-left (region-beginning)
				(region-end))
    (dabbrev-expand)))

;;; Key bindings
;;Python mode move around code blocks
(global-set-key (kbd "M-p") 'python-nav-backward-block)
(global-set-key (kbd "M-n") 'python-nav-forward-block)
(define-key python-mode-map (kbd "<tab>") 'shift-or-indent)
(define-key python-mode-map (kbd "C-<tab>") 'dabbrev-or-indent-left)

(require 'init-flycheck)
(define-key elpy-mode-map (kbd "C-c C-p") 'my-previous-error-wrapped)
(define-key elpy-mode-map (kbd "C-c C-n") 'my-next-error-wrapped)


;;; Hooks
(add-hook 'python-mode-hook 'hs-minor-mode)

(provide 'init-elpy)
