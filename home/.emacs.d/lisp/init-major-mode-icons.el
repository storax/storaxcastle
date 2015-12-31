;;; init-major-mode-icons --- Icons for major modes

;;; Commentary:

;;; Code:

(require 'storax-icons)

(defun storax/major-mode-icon (feature hook icon &optional prefix suffix)
  "After FEATURE is loaded add lambda to HOOK to replace the major mode text.

Text will be replaced with ICON and add an optional PREFIX and SUFFIX."
  (eval-after-load feature
    `(add-hook ',hook (lambda ()
		      (setq mode-name (concat ,prefix ,icon ,suffix))))))

(defvar storax/major-mode-icon-list
  (list
   '("lisp-mode" emacs-lisp-mode-hook storax/icon-el)
   '("lisp-mode" lisp-mode-hook storax/icon-cl)
   '("image-mode" image-mode-hook storax/icon-image)
   '("python" python-mode-hook storax/icon-python)
   '("sh-script" sh-mode-hook storax/icon-shell)
   '("shell" shell-hook storax/icon-shell)
   '("nxml-mode" nxml-mode-hook storax/icon-xml)
   '("ruby-mode" ruby-mode-hook storax/icon-ruby)
   '("json-mode" json-mode-hook storax/icon-json)
   '("help-mode" help-mode-hook storax/icon-help)
   '("rst" rst-mode-hook storax/icon-rst)
   '("info" Info-mode-hook storax/icon-help)
   '("cc-mode" java-mode-hook storax/icon-java)
   '("cc-mode" c++-mode-hook storax/icon-cpp)
   '("css-mode" css-mode-hook storax/icon-css)
   '("arc-mode" archive-mode-hook storax/icon-archive)
   '("yaml-mode" yaml-mode-hook storax/icon-yaml)
   '("js" js-mode-hook storax/icon-js)
   '("sgml-mode" html-mode-hook storax/icon-xml "HTML")
   '("tar-mode" tar-mode-hook storax/icon-archive)
   '("wl-summary" wl-summary-mode-hook storax/icon-mail "Summary ")
   '("wl-folder" wl-folder-mode-hook storax/icon-mail "Folder ")
   '("mime-view" mime-view-mode-hook storax/icon-mail "MIME ")
   '("pdf-view" pdf-view-mode-hook storax/icon-pdf)
   '("magit" magit-status-mode-hook storax/icon-git)
   '("magit-diff" magit-diff-mode-hook storax/icon-git storax/icon-diff)
   '("magit-diff" magit-revision-mode-hook storax/icon-git "Revision ")
   '("magit-process" magit-process-mode-hook storax/icon-git "Process ")
   '("magit" magit-merge-review-mode-hook storax/icon-git "Merge Review ")
   '("magit-log" magit-cherry-mode-hook storax/icon-git storax/icon-cherry)
   '("magit-popup" magit-popup-mode-hook storax/icon-git "Popup ")
   '("magit-log" magit-log-mode-hook storax/icon-git "Log ")
   '("magit-stash" magit-stash-mode-hook storax/icon-git "Stash ")
   '("magit-stash" magit-stashes-mode-hook storax/icon-git "Stashes ")
   '("magit-log" magit-reflog-mode-hook storax/icon-git "Reflog ")
   '("magit" magit-blob-mode-hook storax/icon-git "Blob ")
   '("magit" magit-refs-mode-hook storax/icon-git "Refs ")
   '("magit" magit-file-mode-hook storax/icon-git "File ")
   '("magit-blame" magit-blame-mode-hook storax/icon-git "Blame ")
   '("text-mode" text-mode-hook storax/icon-txt)
   '("simple" messages-buffer-mode-hook storax/icon-log)
   '("sql" sql-mode-hook storax/icon-sql)
   '("elpy-refactor" elpy-refactor-mode-hook storax/icon-python "Refactor ")))

(defun storax/add-major-mode-icon-hooks ()
  "Call 'storax/major-mode-icon' for each args in 'storax/major-mode-icon-list'."
  (dolist (e storax/major-mode-icon-list)
    (apply 'storax/major-mode-icon e)))

(storax/add-major-mode-icon-hooks)
;; Also apply to current messages buffer
(with-current-buffer "*Messages*" (setq mode-name storax/icon-log))

(provide 'init-major-mode-icons)
;;; init-major-mode-icons ends here
