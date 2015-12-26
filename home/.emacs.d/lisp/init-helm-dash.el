;;; init-helm-dash --- Browse Docs

;;; Commentary:

;;; Code:
(add-to-list 'load-path (expand-file-name "helm-dash" user-emacs-directory))
(require 'helm-dash)

(defvar storax/docsets-to-install '("Ansible"
				    "Bash"
				    "Boost"
				    "C"
				    "C++"
				    "CMake"
				    "Chef"
				    "Common_Lisp"
				    "Django"
				    "Docker"
				    "ElasticSearch"
				    "Emacs_Lisp"
				    "Flask"
				    "Jinja"
				    "LaTeX"
				    "PostgreSQL"
				    "Python_2"
				    "Python_3"
				    "Qt"
				    "SQLAlchemy"
				    "Vagrant"))

(defun storax/dash-install-set (docset)
  "Install the DOCSET if it does not exist already."
  (let ((name-escaped (replace-regexp-in-string "_" " " docset)))
    (unless (file-exists-p
	     (expand-file-name (concat (file-name-as-directory ".docsets")
				       (concat name-escaped ".docset"))
			       "~"))
      (helm-dash-install-docset docset))))

(dolist (docset storax/docsets-to-install)
  (storax/dash-install-set docset))

(provide 'init-helm-dash)
;;; init-helm-dash ends here
