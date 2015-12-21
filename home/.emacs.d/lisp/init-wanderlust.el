(require-package 'wanderlust)

;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; Directory where icons are placed.
;; Default: the peculiar value to the running version of Emacs.
;; (Not required if the default value points properly)
;(setq wl-icon-directory "~/work/wl/etc")

;; SMTP server for mail posting. Default: 'nil'
;(setq wl-smtp-posting-server "your.smtp.example.com")
;; NNTP server for news posting. Default: 'nil'
;(setq wl-nntp-posting-server "your.nntp.example.com")
