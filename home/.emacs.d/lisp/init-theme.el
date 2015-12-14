(require-package 'zenburn-theme)

(if window-system
    (load-theme 'zenburn t)
    (load-theme 'wombat t))

(provide 'init-theme)
