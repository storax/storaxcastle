;;Copy Searchresult with M-w
(defun hack-isearch-kill ()
   "Push current matching string into kill ring."
   (interactive)
   (kill-new (buffer-substring (point) isearch-other-end))
   (isearch-done))
(define-key isearch-mode-map (kbd "M-w") 'hack-isearch-kill)

(provide 'init-isearch)