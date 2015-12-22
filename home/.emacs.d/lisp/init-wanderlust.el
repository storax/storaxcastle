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

(require 'epa-mail)
(require 'epa)
(defun storax/epa-verify-region (start end)
  "Verify the current region between START and END.

Replaces the text of the region and removes the
signature gibberish.

Don't use this command in Lisp programs!
Since this function operates on regions, it does some tricks such
as coding-system detection and unibyte/multibyte conversion.  If
you are sure how the data in the region should be treated, you
should consider using the string based counterpart
`epg-verify-string', or the file based counterpart
`epg-verify-file' instead.

For example:

\(let ((context (epg-make-context 'OpenPGP)))
  (decode-coding-string
    (epg-verify-string context (buffer-substring start end))
    'utf-8))"
  (interactive "r")
  (let ((context (epg-make-context epa-protocol))
	plain)
    (epg-context-set-progress-callback context
				       (cons
					#'epa-progress-callback-function
					"Verifying..."))
    (message "Verifying...")
    (setq plain (epg-verify-string
		 context
		 (epa--encode-coding-string
		  (buffer-substring start end)
		  (or coding-system-for-write
		      (get-text-property start 'epa-coding-system-used)))))
    (message "Verifying...done")
    (setq plain (epa--decode-coding-string
		 plain
		 (or coding-system-for-read
		     (get-text-property start 'epa-coding-system-used)
		     'undecided)))
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (delete-region start end)
	  (goto-char start)
	  (insert plain))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

(defun storax/epa-verify-cleartext-in-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (cleartext-start cleartext-end)
	(while (re-search-forward "-----BEGIN PGP SIGNED MESSAGE-----$"
				  nil t)
	  (setq cleartext-start (match-beginning 0))
	  (unless (re-search-forward "^-----BEGIN PGP SIGNATURE-----$"
					   nil t)
	    (error "Invalid cleartext signed message"))
	  (setq cleartext-end (re-search-forward
			   "^-----END PGP SIGNATURE-----$"
			   nil t))
	  (unless cleartext-end
	    (error "No cleartext tail"))
	  (storax/epa-verify-region cleartext-start cleartext-end))))))

(defun storax/wl-verify-signature ()
  (interactive)
  (save-window-excursion
      (storax/epa-verify-cleartext-in-region (point-min) (point-max))))

(defun storax/wl-auto-decrypt-message ()
  (ignore-errors
    (mc-decrypt)))

(add-hook 'mime-view-mode-hook 'storax/wl-verify-signature)
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
