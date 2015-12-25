;;; init-acewindow --- Summary

;;; Commentary:

;;; Code:
(add-to-list 'load-path (expand-file-name "avy" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "ace-window" user-emacs-directory))
(require 'avy)
(require 'ace-window)

(global-set-key (kbd "C-z") 'ace-window)

(provide 'init-acewindow)
;;; init-acewindow ends here
