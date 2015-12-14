(require-package 'auctex)
(require-package 'pdf-tools)

(pdf-tools-install)

;;; Latex PDF mode
(require 'tex)
(TeX-global-PDF-mode t)
(setq TeX-source-correlate-start-server t)

;;; Key-Bindings
;; Latex umlauts
(defun latex-ae () (interactive) (insert "\\\"a"))
(defun latex-oe () (interactive) (insert "\\\"o"))
(defun latex-ue () (interactive) (insert "\\\"u"))
(defun latex-ss () (interactive) (insert "{\\ss}"))

(defun latex-bindings-my ()
  (define-key LaTeX-mode-map (kbd "C-' C-a") 'latex-ae)
  (define-key LaTeX-mode-map (kbd "C-' C-o") 'latex-oe)
  (define-key LaTeX-mode-map (kbd "C-' C-u") 'latex-ue)
  (define-key LaTeX-mode-map (kbd "C-' C-s") 'latex-ss)
)

;;; Hooks
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
;; use special hook for tex-default-command, as it is a local variable
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'latex-bindings-my)

(provide 'init-latex)
