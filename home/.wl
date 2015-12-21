;; You should set this variable if you use multiple e-mail addresses.
(setq wl-user-mail-address-list (quote ("zuber.david@gmx.de" "zuber.david@web.de")))

;;(NOTE: "M-: wl-draft-parent-folder" => %INBOX:myname/clear@imap.gmail.com:993)
(setq wl-draft-config-alist
      '(((string-match "web.de" wl-draft-parent-folder)
	 (template . "web"))
	((string-match "gmx.de" wl-draft-parent-folder)
	 (template . "gmx"))
	;; automatic for replies
	(reply "\\(To\\|Cc\\|Delivered-To\\): .*web.de*"
	 (template . "web"))
	(reply "\\(To\\|Cc\\|Delivered-To\\): .*gmx.de*"
	 (template . "gmx"))))

;;choose template with C-c C-j
(setq wl-template-alist
      '(("gmx"
	 (wl-from . "David Zuber <zuber.david@gmx.de>")
	 (wl-smtp-posting-user . "zuber.david")
	 (wl-smtp-posting-server . "mail.gmx.net")
	 (wl-smtp-authenticate-type ."plain")
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-posting-port . 587)
	 (wl-local-domain . "gmx.net")
	 (wl-message-id-domain . "mail.gmx.net"))
	("web"
	 (wl-from . "David Zuber <zuber.david@web.de>")
	 (wl-smtp-posting-user . "zuber.david")
	 (wl-smtp-posting-server . "smtp.web.de")
	 (wl-smtp-authenticate-type ."plain")
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-posting-port . 587)
	 (wl-local-domain . "web.de")
	 (wl-message-id-domain . "smtp.web.de")))))
