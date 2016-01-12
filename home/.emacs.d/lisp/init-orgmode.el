;;; init-orgmode --- Emacs! Organize my life!

;;; Commentary:

;;; Code:

(require 'init-elpa)
(require-package 'org)
(require-package 'org-plus-contrib)
(require-package 'orgbox)
(require 'org)

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "PAUSED" "|" "DONE" "CANCELED"))
      org-default-notes-file (concat org-directory "/notes.org"))

(unless (file-exists-p org-directory)
  (make-directory org-directory))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'orgbox)

(provide 'init-orgmode)
;;; init-orgmode ends here
