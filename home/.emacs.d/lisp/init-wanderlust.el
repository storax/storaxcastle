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

(defun mc-wl-verify-signature ()
  (interactive)
  (save-window-excursion
    (wl-summary-jump-to-current-message)
    (mc-verify)))

(defun mc-wl-decrypt-message ()
  (interactive)
  (save-window-excursion
    (wl-summary-jump-to-current-message)
    (let ((inhibit-read-only t))
      (mc-decrypt))))

(eval-after-load "mailcrypt"
  '(setq mc-modes-alist
       (append
	(quote
	 ((wl-draft-mode (encrypt . mc-encrypt-message)
	    (sign . mc-sign-message))
	  (wl-summary-mode (decrypt . mc-wl-decrypt-message)
	    (verify . mc-wl-verify-signature))))
	mc-modes-alist)))

(provide 'init-wanderlust)
