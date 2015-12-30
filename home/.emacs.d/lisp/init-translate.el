;;; init-translate --- Translate text

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'thingatpt)
(require 'init-utils)
(require 'helm)

(defgroup storax/translate nil
  "Customization group for storax/tranlate."
  :group 'emacs
  :prefix "storax/translate-")

(defface storax/translate-entry-face
  '((t :inherit font-lock-keyword-face))
  "Face for dictionary entries."
  :group 'storax/translate)

(defface storax/translate-phonetic-face
  '((t :inherit font-lock-type-face :slant italic))
  "Face for phonetic language."
  :group 'storax/translate)

(defface storax/translate-meta-face
  '((t :inherit font-lock-variable-name-face))
  "Face for attributes like noun or female."
  :group 'storax/translate)

(defface storax/translate-translations-face
  '((t :inherit default))
  "Face for translations."
  :group 'storax/translate)

(defvar storax/translate-database "all"
  "The current database.")

(defvar storax/translate-database-list
  (list
   `("The Collaborative International Dictionary of English v.0.48" . "gcide")
   `("WordNet (r) 3.0 (2006)" . "wn")
   `("Moby Thesaurus II by Grady Ward, 1.0" . "moby-thesaurus")
   `("The Elements (07Nov00)" . "elements")
   `("V.E.R.A. -- Virtual Entity of Relevant Acronyms (January 2014)" . "vera")
   `("The Jargon File (version 4.4.7, 29 Dec 2003)" . "jargon")
   `("The Free On-line Dictionary of Computing (20 July 2014)" . "foldoc")
   `("Easton's 1897 Bible Dictionary" . "easton")
   `("Hitchcock's Bible Names Dictionary (late 1800's)" . "hitchcock")
   `("Bouvier's Law Dictionary, Revised 6th Ed (1856)" . "bouvier")
   `("The Devil's Dictionary (1881-1906)" . "devil")
   `("CIA World Factbook 2002" . "world02")
   `("U.S. Gazetteer Counties (2000)" . "gaz2k-counties")
   `("U.S. Gazetteer Places (2000)" . "gaz2k-places")
   `("U.S. Gazetteer Zip Code Tabulation Areas (2000)" . "gaz2k-zips")
   `("Turkish-English FreeDict Dictionary ver. 0.2.1" . "fd-tur-eng")
   `("Portuguese-German FreeDict Dictionary ver. 0.1.1" . "fd-por-deu")
   `("Dutch-English Freedict Dictionary ver. 0.1.3" . "fd-nld-eng")
   `("English-Arabic FreeDict Dictionary ver. 0.6.2" . "fd-eng-ara")
   `("Spanish-English FreeDict Dictionary ver. 0.1.1" . "fd-spa-eng")
   `("English-Hungarian FreeDict Dictionary ver. 0.1" . "fd-eng-hun")
   `("Italian-English FreeDict Dictionary ver. 0.1.1" . "fd-ita-eng")
   `("Welsh-English Freedict dictionary" . "fd-wel-eng")
   `("English-Dutch FreeDict Dictionary ver. 0.1.1" . "fd-eng-nld")
   `("French-English FreeDict Dictionary ver. 0.3.4" . "fd-fra-eng")
   `("Turkish-German FreeDict Dictionary ver. 0.1.1" . "fd-tur-deu")
   `("Swedish-English FreeDict Dictionary ver. 0.1.1" . "fd-swe-eng")
   `("Nederlands-French FreeDict Dictionary ver. 0.1.1" . "fd-nld-fra")
   `("English-Swahili xFried/FreeDict Dictionary" . "fd-eng-swa")
   `("German-Dutch FreeDict Dictionary ver. 0.1.1" . "fd-deu-nld")
   `("French-German FreeDict Dictionary ver. 0.1.1" . "fd-fra-deu")
   `("English-Croatian Freedict Dictionary" . "fd-eng-cro")
   `("English-Italian FreeDict Dictionary ver. 0.1.1" . "fd-eng-ita")
   `("English-Latin FreeDict Dictionary ver. 0.1.1" . "fd-eng-lat")
   `("Latin-English FreeDict Dictionary ver. 0.1.1" . "fd-lat-eng")
   `("French-Dutch FreeDict Dictionary ver. 0.1.2" . "fd-fra-nld")
   `("Italian-German FreeDict Dictionary ver. 0.1.1" . "fd-ita-deu")
   `("English-Hindi FreeDict Dictionary ver. 1.5.1" . "fd-eng-hin")
   `("German-English FreeDict Dictionary ver. 0.3.3" . "fd-deu-eng")
   `("Portuguese-English FreeDict Dictionary ver. 0.1.1" . "fd-por-eng")
   `("Latin - German FreeDict dictionary ver. 0.4" . "fd-lat-deu")
   `("Japanese-German FreeDict Dictionary ver. 0.1.1" . "fd-jpn-deu")
   `("English-German FreeDict Dictionary ver. 0.3.5" . "fd-eng-deu")
   `("English-Serbo-Croat Freedict dictionary" . "fd-eng-scr")
   `("English-Romanian FreeDict Dictionary ver. 0.6.1" . "fd-eng-rom")
   `("Irish-English Freedict dictionary" . "fd-iri-eng")
   `("Czech-English Freedict dictionary" . "fd-cze-eng")
   `("Serbo-Croat-English Freedict dictionary" . "fd-scr-eng")
   `("English-Czech fdicts/FreeDict Dictionary" . "fd-eng-cze")
   `("English-Russian FreeDict Dictionary ver. 0.3" . "fd-eng-rus")
   `("Afrikaans-German FreeDict Dictionary ver. 0.3" . "fd-afr-deu")
   `("English-Portuguese FreeDict Dictionary ver. 0.2.2" . "fd-eng-por")
   `("Hungarian-English FreeDict Dictionary ver. 0.3" . "fd-hun-eng")
   `("English-Swedish FreeDict Dictionary ver. 0.1.1" . "fd-eng-swe")
   `("German-Italian FreeDict Dictionary ver. 0.1.1" . "fd-deu-ita")
   `("Croatian-English Freedict Dictionary" . "fd-cro-eng")
   `("Danish-English FreeDict Dictionary ver. 0.2.1" . "fd-dan-eng")
   `("English-Turkish FreeDict Dictionary ver. 0.2.1" . "fd-eng-tur")
   `("English-Spanish FreeDict Dictionary ver. 0.2.1" . "fd-eng-spa")
   `("Dutch-German FreeDict Dictionary ver. 0.1.1" . "fd-nld-deu")
   `("German-Portuguese FreeDict Dictionary ver. 0.2.1" . "fd-deu-por")
   `("Swahili-English xFried/FreeDict Dictionary" . "fd-swa-eng")
   `("English-Hindi Freedict Dictionary [reverse index]" . "fd-hin-eng")
   `("German-French FreeDict Dictionary ver. 0.3.1" . "fd-deu-fra")
   `("English-French FreeDict Dictionary ver. 0.1.4" . "fd-eng-fra")
   `("Slovak-English Freedict dictionary" . "fd-slo-eng")
   `("Scottish Gaelic-German FreeDict Dictionary ver. 0.1.1" . "fd-gla-deu")
   `("English-Welsh Freedict dictionary" . "fd-eng-wel")
   `("English-Irish Freedict dictionary" . "fd-eng-iri")
   `("English Monolingual Dictionaries" . "english")
   `("Translating Dictionaries" . "trans")
   `("All Dictionaries (English-Only and Translating)" . "all")
   )
  "List of dict databases.")

(defvar storax/translate-database-common-list
  (list
   `("German-English FreeDict Dictionary ver. 0.3.3" . "fd-deu-eng")
   `("English-German FreeDict Dictionary ver. 0.3.5" . "fd-eng-deu")
   `("Translating Dictionaries" . "trans")
   `("All Dictionaries (English-Only and Translating)" . "all")
   )
  "List of common dict databases.")

(defvar storax/translate-result-regexp
  "\\(From \\(.*?\\):

  \\(.*?\\)
   \\(.*?\\)
\\(?:
\\)*\\)"
  "For parsing the output of /usr/bin/dict.")

(defun storax/translate-match (regexp string &optional group)
  "With REGEXP match STRING and return the nth GROUP."
  (if (string-match regexp string)
      (match-string group string)))

(defun storax/translate-transform (key value)
  "Return an alist for the given KEY and VALUE.

The list has the following form:
'(ENTRY PHONETIC META TRANSLATIONS)

ENTRY is the dictionary entry.
PHONETIC is the phonetic notation of ENTRY.
META is a string with comma seperated attributes
like n for noun or fem for female.
TRANSLATIONS is a list of possible translations."
  (let ((entry (storax/translate-match "^\\([^/<]+[^ /<]\\)" key 1))
	(phonetic (storax/translate-match "/\\(.+\\)/" key 1))
	(meta (storax/translate-match "<\\(.+\\)>" key 1))
	(translations (split-string value "; ")))
    (list entry phonetic meta translations)))

(defun storax/translate-format (entry phonetic meta translations)
  "Format the given ENTRY PHONETIC META and TRANSLATIONS from the dict."
  (format "%s%s%s\n%s"
	  (propertize entry 'face 'storax/translate-entry-face)
	  (if phonetic (propertize (format " /%s/" phonetic) 'face 'storax/translate-phonetic-face) "")
	  (if meta (propertize (format " <%s>" meta) 'face 'storax/translate-meta-face) "")
   (propertize (mapconcat #'identity translations "; ") 'face '(:inherit font-lock-doc-face))))

(defun storax/translate--transform-format (key value)
  "Transform KEY and VALUE, then format result."
  (apply 'storax/translate-format (storax/translate-transform key value)))

(defun storax/translate-fetch (database query)
  "Fetch entries in DATABASE for QUERY and return transformed result."
  (let* ((output (shell-command-to-string (format "/usr/bin/dict -d %s %s" database query)))
	 (defs (sanityinc/string-all-matches storax/translate-result-regexp output 3))
	 (exps (sanityinc/string-all-matches storax/translate-result-regexp output 4)))
    (cl-mapcar #'storax/translate-transform defs exps)))

(defun storax/translate (database query)
  "Translate with DATABASE the given QUERY."
  (let* ((parsed (storax/translate-fetch database query))
	 (buf (get-buffer-create "Translation")))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (mapconcat 'identity
		  (cl-mapcar #'(lambda (args) (apply 'storax/translate-format args)) parsed)
		  "\n\n")))
    (unless (get-buffer-window buf 0)
      (pop-to-buffer buf nil t))))

(defun storax/translate-helm-fetch ()
  "Fetch entries in english dict for helm pattern."
  (let ((parsed (storax/translate-fetch storax/translate-database helm-pattern)))
    (mapcar (lambda (p)
	      (cons (apply 'storax/translate-format p) p)) parsed)))

(defun storax/translate-input ()
  "Return a suitable input.

Either region, word at point or nothing."
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    (word-at-point)))

(defun storax/translate-helm-select-translation (parsed)
  (let ((sources `((name . ,(format "Translations for \"%s\"" (car parsed)))
		   (candidates . ,(nth 3 parsed)))))
    (helm
     :sources '(sources))))

(defun storax/translate-helm-actions (actions parsed)
  "Return a list of ACTIONS for PARSED dict entry."
  `((,(format "What is parsed: %s" parsed) . storax/translate-helm-select-translation)))

(defvar storax/translate-helm-source
  '((name . "Translate")
    (multiline)
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (candidates-process . storax/translate-helm-fetch)
    (action-transformer . storax/translate-helm-actions)))

(defvar storax/translate-helm-select-history nil
  "History of selected databases.")

(defun storax/translate-helm-select-actions (actions db)
  "Return a list of helm ACTIONS for this DB."
    `((,(format "Use %s as Database" db) . (lambda (&rest args) (setq storax/translate-database ,db) (storax/translate-helm)))))

(defvar storax/translate-helm-select-source
  '((name . "Dictionary Databases")
    (candidates . storax/translate-database-list)
    (action-transformer . storax/translate-helm-select-actions)))

(defvar storax/translate-helm-select-source-common
  '((name . "Dictionary Databases")
    (candidates . storax/translate-database-common-list)
    (action-transformer . storax/translate-helm-select-actions)))

(defun storax/translate-helm-select (arg)
  "Select database first then translate something.

If ARG is non-nil choose form all databases.
Else use only the 'storax/translate-database-common-list'."
  (interactive "P")
  (if arg
      (helm
       :sources '(storax/translate-helm-select-source)
       :buffer "*Dictionary Databases*"
       :prompt "Database: "
       :history storax/translate-helm-select-history
       :preselect (car (rassoc storax/translate-database storax/translate-database-list))))
  (helm
   :sources '(storax/translate-helm-select-source-common)
   :buffer "*Dictionary Database*"
   :prompt "Database: "
   :history storax/translate-helm-select-history
   :preselect (car (rassoc storax/translate-database storax/translate-database-list))))

(defun storax/translate-helm ()
  "Translate with helm."
  (interactive)
  (helm
   :sources '(storax/translate-helm-source)
   :buffer "*Translate*"
   :prompt "Translate: "
   :input (storax/translate-input)))

(global-set-key (kbd "C-c d") 'storax/translate-helm-select)
(provide 'init-translate)
;;; init-translate ends here
