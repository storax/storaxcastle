;;; init-avy --- The New Ace Jump

;;; Commentary:

;;; Code:
(add-to-list 'load-path (expand-file-name "avy" user-emacs-directory))
(require 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)
(define-key global-map (kbd "C-c c SPC") 'avy-goto-char)
(define-key global-map (kbd "C-c c c SPC") 'avy-goto-line)

(provide 'init-avy)
;;; init-avy ends here
