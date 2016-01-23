;;; init-gitgutter.el --- Display diffs in side column

;;; Commentary:

;;; Code:

(require-package 'git-gutter)
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

(provide 'init-gitgutter.el)
;;; init-gitgutter.el ends here
