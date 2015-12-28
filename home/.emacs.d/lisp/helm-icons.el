;;; helm-icons --- Add icons to helm

;;; Commentary:

;; Useless!

;;; Code:
(require 'cl-lib)
(require 'helm-files)

(cl-defun storax/create-helm-icon (path &optional (acc 85) (text " "))
  "Create an icon for a specific filetype"
  (propertize text 'display
	      (create-image
	       (with-temp-buffer
		 (insert-file-contents path) (buffer-string))
	       nil t :ascent acc :mask 'heuristic)))

(defconst storax/icon-file (storax/create-helm-icon "~/.emacs.d/icons/file-file.svg"))
(defconst storax/icon-archive (storax/create-helm-icon "~/.emacs.d/icons/file-archive.svg"))
(defconst storax/icon-config (storax/create-helm-icon "~/.emacs.d/icons/file-config.svg"))
(defconst storax/icon-cpp (storax/create-helm-icon "~/.emacs.d/icons/file-cpp.svg"))
(defconst storax/icon-image (storax/create-helm-icon "~/.emacs.d/icons/file-image.svg"))
(defconst storax/icon-xml (storax/create-helm-icon "~/.emacs.d/icons/file-xml.svg"))
(defconst storax/icon-shell (storax/create-helm-icon "~/.emacs.d/icons/file-shell.svg"))

(defvar storax/helm-icons (list (cons "py" (storax/create-helm-icon "~/.emacs.d/icons/file-py.svg"))
				(cons "/" (storax/create-helm-icon "~/.emacs.d/icons/file-folder.svg"))
				(cons "zip" storax/icon-archive)
				(cons "gz" storax/icon-archive)
				(cons "whl" storax/icon-archive)
				(cons "json" (storax/create-helm-icon "~/.emacs.d/icons/file-json.svg"))
				(cons "rst" (storax/create-helm-icon "~/.emacs.d/icons/file-rst.svg"))
				(cons "ini" storax/icon-config)
				(cons "cfg" storax/icon-config)
				(cons "cpp" storax/icon-cpp)
				(cons "hpp" storax/icon-cpp)
				(cons "h" storax/icon-file)
				(cons "coffee" (storax/create-helm-icon "~/.emacs.d/icons/file-coffee.svg"))
				(cons "css" (storax/create-helm-icon "~/.emacs.d/icons/file-css.svg"))
				(cons "qss" (storax/create-helm-icon "~/.emacs.d/icons/file-qss.svg"))
				(cons "js" (storax/create-helm-icon "~/.emacs.d/icons/file-js.svg"))
				(cons "jpg" storax/icon-image)
				(cons "jpeg" storax/icon-image)
				(cons "png" storax/icon-image)
				(cons "gif" storax/icon-image)
				(cons "svg" storax/icon-image)
				(cons "tiff" storax/icon-image)
				(cons "xml" storax/icon-xml)
				(cons "html" storax/icon-xml)
				(cons "htm" storax/icon-xml)
				(cons "rb" (storax/create-helm-icon "~/.emacs.d/icons/file-ruby.svg"))
				(cons "md" (storax/create-helm-icon "~/.emacs.d/icons/file-md.svg"))
				(cons "sh" storax/icon-shell))
  "Icons for helm find file")

(defun storax/icon-for-file (file)
  "Return a string with an icon display property for FILE."
  (let ((icon (if (file-directory-p file)
		  (cdr (assoc "/" storax/helm-icons))
		(cdr (assoc-string (file-name-extension file) storax/helm-icons)))))
    (if icon
	icon
      storax/icon-file)))

(defun storax/add-icons-to-files (old-function file)
  "Add to candidates of OLD-FUNCTION an icon for each FILE according to the type."
  (let* ((boring (funcall old-function file))
	 (disp (car boring)))
    (if disp
	(cons (concat (storax/icon-for-file file) " " disp) file)
      boring)))

(advice-add #'helm-ff-filter-candidate-one-by-one :around #'storax/add-icons-to-files)

(provide 'helm-icons)
;;; helm-icons ends here
