;;; init-avy --- The New Ace Jump

;;; Commentary:

;;; Code:
(require-package 'avy)
(require 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)
(define-key global-map (kbd "C-c c SPC") 'avy-goto-char)
(define-key global-map (kbd "C-c c c SPC") 'avy-goto-line)

(provide 'init-avy)
;;; init-avy ends here
