;;; init-yasnippet --- Configure yasnippets

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "yasnippet" user-emacs-directory))

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-indent-line nil)


;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

;;----------------------------------------------------------------------------
;; use popup menu for yas-choose-value
;;----------------------------------------------------------------------------
(require-package 'popup)
(require 'popup)

(defun storax/yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Promt with isearch when there are two yasnippets.

PROMPT is the prompt to use.
CHOICES are a list of available choices.
DISPLAY-FN determines if choice is displayed."
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

(setq yas-prompt-functions nil)

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------
(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))

(provide 'init-yasnippet)
;;; init-yasnippet ends here
