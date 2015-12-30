;;; init-major-mode-icons --- Icons for major modes

;;; Commentary:

;;; Code:

(require 'storax-icons)

(defun storax/major-mode-icon (feature hook icon &optional prefix suffix)
  "After FEATURE is loaded add lambda to HOOK to replace the major mode text.

Text will be replaced with ICON and add an optional PREFIX and SUFFIX."
  (eval-after-load feature
    `(add-hook ',hook (lambda ()
		      (setq mode-name (concat ,prefix ,icon ,suffix))))))

(defvar storax/major-mode-icon-list
  (list
   '("lisp-mode" emacs-lisp-mode-hook storax/icon-el)
   '("lisp-mode" lisp-mode-hook storax/icon-cl)
   '("image-mode" image-mode-hook storax/icon-image)
   '("python" python-mode-hook storax/icon-python)
   '("sh-script" sh-mode-hook storax/icon-shell)
   '("nxml-mode" nxml-mode-hook storax/icon-xml)
   '("cc-mode" c++-mode-hook storax/icon-cpp)
   '("css-mode" css-mode-hook storax/icon-css)
   '("arc-mode" archive-mode-hook storax/icon-archive)))

(defun storax/add-major-mode-icon-hooks ()
  "Call 'storax/major-mode-icon' for each args in 'storax/major-mode-icon-list'."
  (dolist (e storax/major-mode-icon-list)
    (apply 'storax/major-mode-icon e)))

(storax/add-major-mode-icon-hooks)

(provide 'init-major-mode-icons)
;;; init-major-mode-icons ends here
