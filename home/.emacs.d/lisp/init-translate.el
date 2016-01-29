;;; init-translate --- Translate text

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'google-translage)
(require 'google-translate)
(require 'google-translate-default-ui)

(global-set-key (kbd "C-c d") 'google-translate-at-point)
(global-set-key (kbd "C-c D") 'google-translate-query-translate)
(provide 'init-translate)
;;; init-translate ends here
