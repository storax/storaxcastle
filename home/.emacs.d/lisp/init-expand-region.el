(require-package 'expand-region)

;;Expand Region
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(provide 'init-expand-region)
