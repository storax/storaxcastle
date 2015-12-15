(require-package 'flycheck)

;;;; Safe flymake find file hook
;;(require 'flymake)
;;(defun cwebber/safer-flymake-find-file-hook ()
;;  "Don't barf if we can't open this flymake file"
;;  (let ((flymake-filename
;;         (flymake-create-temp-inplace (buffer-file-name) "flymake")))
;;    (if (file-writable-p flymake-filename)
;;        (flymake-find-file-hook)
;;      (message
;;       (format
;;        "Couldn't enable flymake; permission denied on %s" flymake-filename)))))
;;(add-hook 'find-file-hook 'cwebber/safer-flymake-find-file-hook)
(require 'compile)
(require 'flycheck)

;;; Flashing mode line and cycle navigation
(defvar my-modeline-flash-color "#af00d7")

(defun my-indicate-error-nav-wrapped (direction)
  "Display a message in minibuffer indicating that we wrapped
also flash the mode-line"
  (let ((mode-line-color (face-background 'mode-line)))
    (message "Wrapped %s error" (symbol-name direction))
    (set-face-background 'mode-line my-modeline-flash-color)
    (sit-for 0.3)
    (set-face-background 'mode-line mode-line-color)))

(defun my-next-error-wrapped (&optional arg reset)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move forwards (or
backwards, if negative). With just C-u as prefix moves to first error"
  (interactive "P")
  (condition-case nil
      (call-interactively 'next-error)
    ('user-error (progn (next-error 1 t)
			(my-indicate-error-nav-wrapped 'next)))))

(defun my-jump-to-last-error (buffer)
  "Jump to last error in the BUFFER, this assumes that
the error is at last but third line"
  (save-selected-window
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))
    (forward-line -3)
    (call-interactively 'compile-goto-error)))

(defun my-previous-error-wrapped (&optional arg)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)."
  (interactive "P")
  (condition-case nil
      (if (eq (point) (point-min))
	  (progn
	    (my-indicate-error-nav-wrapped 'previous)
	    (goto-char (point-max))
	    (call-interactively 'previous-error))
      (if (compilation-buffer-p (current-buffer))
	  (compilation-previous-error 1)
	(call-interactively 'previous-error)))
    ('user-error (progn
		   (let ((error-buffer (next-error-find-buffer)))
		     ;; If the buffer has an associated error buffer use it to
		     ;; to move to last error
		     (my-indicate-error-nav-wrapped 'previous)
		     (if (and (not (eq (current-buffer) error-buffer))
			      (compilation-buffer-p error-buffer))
			 (my-jump-to-last-error error-buffer)
		       ;; Otherwise move to last point and invoke previous error
		       (goto-char (point-max))
		       (call-interactively 'previous-error)))))))


(global-set-key (kbd "C-c C-p") 'my-previous-error-wrapped)
(global-set-key (kbd "C-c C-n") 'my-next-error-wrapped)


;;; Fix lisp checker to use load path
(defun flycheck-use-my-load-path ()
  (setq-default flycheck-emacs-lisp-load-path load-path))

(add-hook after-init-hook 'flycheck-emacs-lisp-load-path)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8
      flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; Modeline config
(setq flycheck-mode-line
      '(:eval
	(pcase flycheck-last-status-change
	  (`not-checked nil)
	  (`no-checker (propertize " -" 'face 'warning))
	  (`running (propertize " ✷" 'face 'success))
	  (`errored (propertize (format " %s" (char-to-string #x2717)) 'face 'error))
	  (`finished
	   (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
		  (no-errors (cdr (assq 'error error-counts)))
		  (no-warnings (cdr (assq 'warning error-counts)))
		  (face (cond (no-errors 'error)
			      (no-warnings 'warning)
			      (t 'success))))
	     (propertize (if (or no-errors no-warnings)
			     (format " %s%s/%s"
				     (char-to-string #x2717)
				     (or no-errors 0)
				     (or no-warnings 0))
			   (format " %s" (char-to-string #x2713)))
			 'face face)))
	  (`interrupted " -")
	  (`suspicious '(propertize " ?" 'face 'warning)))))

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init-flycheck)
