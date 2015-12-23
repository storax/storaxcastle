;;; init-spotify --- Control spotify player in emacs

;;; Commentary:

;;; Code:
(require-package 'multi)

(add-to-list 'load-path (expand-file-name "helm-spotify" user-emacs-directory))
(require 'helm-spotify)

(provide 'init-spotify)
;;; init-spotify ends here
