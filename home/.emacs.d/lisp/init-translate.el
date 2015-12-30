;;; init-translate --- Translate text

;;; Commentary:

;;; Code:

(require 'init-utils)

(defun storax/translate--format (key value)
  "Format the given KEY and VALUE from the dict."
  (format "%s\n%s"
   (propertize key 'face '(:inherit 'font-lock-keyword-face))
   (propertize value 'face '(:inherti 'font-lock-doc-face))))

(defun storax/translate (database query)
  "Translate with DATABASE the given QUERY."
  (let* ((output (shell-command-to-string (format "/usr/bin/dict -d %s %s" database query)))
	 (defs (sanityinc/string-all-matches "\\(From \\(.*?\\):

  \\(.*?\\)
   \\(.*?\\)
\\(?:
\\)*\\)" output 3))
	 (exps (sanityinc/string-all-matches "\\(From \\(.*?\\):

  \\(.*?\\)
   \\(.*?\\)
\\(?:
\\)*\\)" output 4))
	 (buf (get-buffer-create "Translation")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (mapconcat 'identity (mapcar* #'storax/translate--format defs exps) "\n\n")))
    (unless (get-buffer-window buf 0)
      (pop-to-buffer buf nil t))))

(provide 'init-translate)
;;; init-translate ends here
