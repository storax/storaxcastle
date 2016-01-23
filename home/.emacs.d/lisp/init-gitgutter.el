;;; init-gitgutter.el --- Display diffs in side column

;;; Commentary:

;;; Code:

(require-package 'git-gutter)
(require 'git-gutter)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)
(add-hook 'rst-mode-hook 'git-gutter-mode)
(add-hook 'yaml-mode-hook 'git-gutter-mode)
(add-hook 'nxml-mode-hook 'git-gutter-mode)
(add-hook 'nxml-mode-hook 'git-gutter-mode)
(add-hook 'conf-mode-hook 'git-gutter-mode)

(provide 'init-gitgutter)
;;; init-gitgutter.el ends here
