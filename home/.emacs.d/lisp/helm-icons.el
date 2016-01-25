;;; helm-icons --- Add icons to helm

;;; Commentary:

;; Useless!

;;; Code:
(require 'helm-files)
(require 'storax-icons)

(defvar storax/helm-file-icons
  (list
   `("py" . ,storax/icon-python)
   `("/" . ,storax/icon-folder)
   `("zip" . ,storax/icon-archive)
   `("gz" . ,storax/icon-archive)
   `("rar" . ,storax/icon-archive)
   `("xz" . ,storax/icon-archive)
   `("whl" . ,storax/icon-archive)
   `("json" . ,storax/icon-json)
   `("rst" . ,storax/icon-rst)
   `("txt" . ,storax/icon-txt)
   `("pdf" . ,storax/icon-pdf)
   `("ini" . ,storax/icon-config)
   `("cfg" . ,storax/icon-config)
   `("conf" . ,storax/icon-config)
   `("cpp" . ,storax/icon-cpp)
   `("hpp" . ,storax/icon-cpp)
   `("h" . ,storax/icon-file)
   `("mp4" . ,storax/icon-movie)
   `("mkv" . ,storax/icon-movie)
   `("avi" . ,storax/icon-movie)
   `("mov" . ,storax/icon-movie)
   `("flv" . ,storax/icon-movie)
   `("mpeg" . ,storax/icon-movie)
   `("mpg" . ,storax/icon-movie)
   `("coffee" . ,storax/icon-coffee)
   `("css" . ,storax/icon-css)
   `("qss" . ,storax/icon-qss)
   `("js" . ,storax/icon-js)
   `("jpg" . ,storax/icon-image)
   `("jpeg" . ,storax/icon-image)
   `("png" . ,storax/icon-image)
   `("gif" . ,storax/icon-image)
   `("svg" . ,storax/icon-image)
   `("tiff" . ,storax/icon-image)
   `("xml" . ,storax/icon-xml)
   `("html" . ,storax/icon-xml)
   `("htm" . ,storax/icon-xml)
   `("el" . ,storax/icon-el)
   `("rb" . ,storax/icon-ruby)
   `("md" . ,storax/icon-md)
   `("sh" . ,storax/icon-shell)
   `("db" . ,storax/icon-db)
   `("sql" . ,storax/icon-sql)
   `("yaml" . ,storax/icon-yaml)
   `("yml" . ,storax/icon-yaml))
  "Icons for helm find file.")

(defun storax/icon-for-file (file)
  "Return a string with an icon display property for FILE."
  (let ((icon (if (file-directory-p file)
		  (cdr (assoc "/" storax/helm-file-icons))
		(cdr (assoc-string (file-name-extension file) storax/helm-file-icons)))))
    (if icon
	icon
      storax/icon-file)))

(defun storax/add-icons-to-files (oldfunc file)
  "Add icons to the candidates of OLDFUNC called with REQUIRE-MATCH."
  (let ((oldval (funcall oldfunc file)))
    (if oldval
	(cons (concat " " (storax/icon-for-file (car oldval)) " " (car oldval)) (cdr oldval)))))

(advice-add #'helm-ff-filter-candidate-one-by-one :around #'storax/add-icons-to-files)

(provide 'helm-icons)
;;; helm-icons ends here
