;;; init-diff-hl.el --- Display diffs in side column

;;; Commentary:

;;; Code:
(require-package 'diff-hl)

(defadvice diff-hl-mode (after storax/diff-hl-fix-magit activate)
  "Fix nesting error by removing the hook."
  (when diff-hl-mode
      (remove-hook 'magit-revert-buffer-hook 'diff-hl-update t)))

(global-diff-hl-mode)
(diff-hl-flydiff-mode)

(provide 'init-diff-hl)
;;; init-diff-hl.el ends here
