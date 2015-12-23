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

(require 'mailcrypt)
(load-library "mailcrypt") ; provides "mc-setversion"
(mc-setversion "gpg")    ; for PGP 2.6 (default); also "5.0" and "gpg"

(add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
(add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

(defun storax/wl-auto-decrypt-message ()
  (condition-case err
      (mc-decrypt)
  (error (message "%s" (error-message-string err)))))

(add-hook 'mime-view-mode-hook 'storax/wl-auto-decrypt-message)

(defun storax/wlr ()
  (interactive)
  (with-selected-frame (make-frame-command)
    (delete-other-windows)
    (toggle-frame-maximized)
    (wl)
    (wl-folder-jump-folder "%INBOX:\"zuber.david@gmx.de\"@imap.gmx.net:993!")
    (wl-folder-jump-to-current-entity)
    (window-configuration-to-register ?w)))

(provide 'init-wanderlust)
