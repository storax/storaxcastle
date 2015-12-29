;;; storax-icons --- A set of icons

;;; Commentary:

;;; Code:

(require 'cl-lib)
(cl-defun storax/create-icon (path &optional (asc 80) (text "  "))
  "Return string with icon at PATH displayed with ascent ASC and TEXT."
  (propertize text 'display
	      (create-image
	       (with-temp-buffer
		 (insert-file-contents path) (buffer-string))
	       nil t :ascent asc :mask 'heuristic)))

(defconst storax/icon-file (storax/create-icon "~/.emacs.d/icons/file-file.svg"))
(defconst storax/icon-archive (storax/create-icon "~/.emacs.d/icons/file-archive.svg"))
(defconst storax/icon-config (storax/create-icon "~/.emacs.d/icons/file-config.svg"))
(defconst storax/icon-cpp (storax/create-icon "~/.emacs.d/icons/file-cpp.svg"))
(defconst storax/icon-image (storax/create-icon "~/.emacs.d/icons/file-image.svg"))
(defconst storax/icon-xml (storax/create-icon "~/.emacs.d/icons/file-xml.svg"))
(defconst storax/icon-shell (storax/create-icon "~/.emacs.d/icons/file-shell.svg"))
(defconst storax/icon-yaml (storax/create-icon "~/.emacs.d/icons/file-yaml.svg"))
(defconst storax/icon-movie (storax/create-icon "~/.emacs.d/icons/file-movie.svg"))
(defconst storax/icon-python (storax/create-icon "~/.emacs.d/icons/file-py.svg"))
(defconst storax/icon-folder (storax/create-icon "~/.emacs.d/icons/file-folder.svg"))
(defconst storax/icon-json (storax/create-icon "~/.emacs.d/icons/file-json.svg"))
(defconst storax/icon-rst (storax/create-icon "~/.emacs.d/icons/file-rst.svg"))
(defconst storax/icon-txt (storax/create-icon "~/.emacs.d/icons/file-txt.svg"))
(defconst storax/icon-pdf (storax/create-icon "~/.emacs.d/icons/file-pdf.svg"))
(defconst storax/icon-coffee (storax/create-icon "~/.emacs.d/icons/file-coffee.svg"))
(defconst storax/icon-qss (storax/create-icon "~/.emacs.d/icons/file-qss.svg"))
(defconst storax/icon-css (storax/create-icon "~/.emacs.d/icons/file-css.svg"))
(defconst storax/icon-js (storax/create-icon "~/.emacs.d/icons/file-js.svg"))
(defconst storax/icon-el (storax/create-icon "~/.emacs.d/icons/file-el.svg"))
(defconst storax/icon-ruby (storax/create-icon "~/.emacs.d/icons/file-ruby.svg"))
(defconst storax/icon-md (storax/create-icon "~/.emacs.d/icons/file-md.svg"))

(provide 'storax-icons)
;;; storax-icons ends here
