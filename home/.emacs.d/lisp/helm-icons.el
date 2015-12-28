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

(defvar storax/helm-icons (list (cons "py" (storax/create-helm-icon "~/.emacs.d/icons/file-py.svg"))
				(cons "/" (storax/create-helm-icon "~/.emacs.d/icons/file-folder.svg")))
  "Icons for helm find file")

(defun storax/icon-for-file (file)
  "Return a string with an icon display property for FILE."
  (let ((icon (if (file-directory-p file)
		  (cdr (assoc "/" storax/helm-icons))
		(cdr (assoc (file-name-extension file) storax/helm-icons)))))
    (if icon
	icon
      "   ")))

(defun storax/add-icons-to-files (old-function file)
  "Add to candidates of OLD-FUNCTION an icon for each FILE according to the type."
  (let* ((boring (funcall old-function file))
	 (disp (car boring)))
    (if disp
	(cons (concat (storax/icon-for-file file) disp) file)
      boring)))

(advice-add #'helm-ff-filter-candidate-one-by-one :around #'storax/add-icons-to-files)

(provide 'helm-icons)
;;; helm-icons ends here
