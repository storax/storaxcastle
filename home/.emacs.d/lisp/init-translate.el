;;; init-translate --- Translate text

;;; Commentary:

;;; Code:

(require 'init-elpa)
(require-package 'google-translate)

(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key (kbd "C-c t") 'google-translate-smooth-translate)

(provide 'init-translate)
;;; init-translate ends here
