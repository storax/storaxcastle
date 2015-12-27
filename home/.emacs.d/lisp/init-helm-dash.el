;;; init-helm-dash --- Browse Docs

;;; Commentary:

;;; Code:
(require-package 'helm-dash)
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
				    "Qt_4"
				    "Qt_5"
				    "SQLAlchemy"
				    "Vagrant"))

(defvar storax/user-docsets-to-install '("Alembic"
					 "Emacs"
					 "Packer"
					 "PyMel"
					 "Requests"
					 "Sphinx"))


(defun storax/dash-install-set (docset)
  "Install the DOCSET if it does not exist already."
  (let ((name-escaped (replace-regexp-in-string "_" " " docset)))
    (unless (file-exists-p
	     (expand-file-name (concat (file-name-as-directory ".docsets")
				       (concat name-escaped ".docset"))
			       "~"))
      (helm-dash-install-docset docset))))

(defun storax/dash-install-user-set (docset)
  "Install the user DOCSET if it does not exist already."
  (let ((name-escaped (replace-regexp-in-string "_" " " docset)))
    (unless (file-exists-p
	     (expand-file-name (concat (file-name-as-directory ".docsets")
				       (concat name-escaped ".docset"))
			       "~"))
      (helm-dash-install-user-docset docset))))

(dolist (docset storax/docsets-to-install)
  (storax/dash-install-set docset))
(dolist (docset storax/user-docsets-to-install)
  (storax/dash-install-user-set docset))

(provide 'init-helm-dash)
;;; init-helm-dash ends here
