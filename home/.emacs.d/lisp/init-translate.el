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
	      (cons (apply 'storax/translate-format p) (car p))) parsed)))

(defun storax/translate-input ()
  "Return a suitable input.

Either region, word at point or nothing."
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    (word-at-point)))

(defvar storax/translate-helm-source
  '((name . "Translate")
    (multiline)
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (candidates-process . storax/translate-helm-fetch)))

(defun storax/translate-helm-deu-eng ()
  "Translate with helm interface."
  (interactive)
  (setq storax/translate-database "fd-deu-eng")
  (helm
   :sources '(storax/translate-helm-source)
   :buffer "*Translate*"
   :promt "German -> English:"
   :input (storax/translate-input)))

(provide 'init-translate)
;;; init-translate ends here
