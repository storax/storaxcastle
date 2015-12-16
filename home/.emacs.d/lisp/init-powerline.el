(require 'cl-lib)
(require-package 'powerline)
(require 'powerline)

;;;; Customization
(defface flycheck-color-mode-line-error-face-active
  '((t :foreground "#efefef" :weight normal :background "#990A1B"))
  "Face for the modeline in buffers with Flycheck errors."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-warning-face-active
  '((t :foreground "#efefef" :weight normal :background "#7B6000"))
  "Face for the modeline in buffers with only Flycheck warnings."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-info-face-active
  '((t :foreground "#efefef" :weight normal :background "#00629D"))
  "Face for the modeline in buffers with only Flycheck info."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-error-face-inactive
  '((t :foreground "#efefef" :weight normal :background "#B22222"))
  "Face for the modeline in buffers with Flycheck errors."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-warning-face-inactive
  '((t :foreground "#efefef" :weight normal :background "#8B7000"))
  "Face for the modeline in buffers with only Flycheck warnings."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-info-face-inactive
  '((t :foreground "#efefef" :weight normal :background "#0072AD"))
  "Face for the modeline in buffers with only Flycheck info."
  :group 'flycheck-faces)

;;;; Custom Functions
(defun dz-string-from-file (file)
  "Read a file"
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun dz-color-svg (image color1)
  "Hacky stuff. The svgs have %s for fill color."
  (format image color1))

(cl-defun dz-create-image (img color1 &optional (acc 90))
  "Creates a image out of the a image data and colors it.
  ascent 90 seems to work best. mask is for transparent background"
  (create-image (dz-color-svg img color1) 'svg t :ascent acc :mask 'heuristic))

(defun dz-create-image-with-face (image face)
  "Read the font color and appy it to the image"
  (dz-create-image image (face-attribute face :foreground nil t)))

;; Icons
(defvar dz-github-mark-data (dz-string-from-file "~/.emacs.d/icons/mark-github.svg"))
(defvar dz-bitbucket-mark-data (dz-string-from-file "~/.emacs.d/icons/mark-bitbucket.svg"))

;; Save the current remote url in each buffer
(defvar remoteurl "")

(defun setremoteurl ()
  "Save the current remote url"
  (make-local-variable 'remoteurl)
  (setq remoteurl (magit-get "remote" "origin" "url")))

;; Kinda like each time we open a file we set the remoteurl
;; then we can display an icon in the modeline accordingly
(add-hook 'after-change-major-mode-hook 'setremoteurl)

;; Github or Bitbucket logo
(defpowerline powerline-remote
  (when (or (string-match "magit" (format "%s" major-mode)) (and (buffer-file-name (current-buffer)) vc-mode))
    (cond ((string-match "github.com" remoteurl)
	   (propertize " " 'display (dz-create-image-with-face dz-github-mark-data face)))
	  ((string-match "bitbucket.org" remoteurl)
	   (propertize " " 'display (dz-create-image-with-face dz-bitbucket-mark-data face))))))

;; Custom Version Control indicator
(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if window-system
      (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
	(when backend
	  (if (string= backend "Git")
	      (format " %s %s" (char-to-string #xf020)
		      (vc-working-revision (buffer-file-name (current-buffer)) backend))
	    (format " %s:%s" backend
		    (vc-working-revision (buffer-file-name (current-buffer)) backend)))))
      (format-mode-line '(vc-mode vc-mode)))))

;; Hide some minor modes
(require-package 'diminish)
(require 'diminish)
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(eval-after-load "helm-mode" '(diminish 'helm-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-current-column-mode))
(eval-after-load "subword" '(diminish 'subword-mode))

;;;; Custom theme with flycheck color
(defun my-powerline-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active
				     (cond ((flycheck-has-current-errors-p 'error)
					  'flycheck-color-mode-line-error-face-active)
					 ((flycheck-has-current-errors-p 'warning)
					  'flycheck-color-mode-line-warning-face-active)
					 ((flycheck-has-current-errors-p 'info)
					  'flycheck-color-mode-line-info-face-active)
					 ((flycheck-running-p)
					  'powerline-active2)
					 ('powerline-active2))
				   (cond ((flycheck-has-current-errors-p 'error)
					  'flycheck-color-mode-line-error-face-inactive)
					 ((flycheck-has-current-errors-p 'warning)
					  'flycheck-color-mode-line-warning-face-inactive)
					 ((flycheck-has-current-errors-p 'info)
					  'flycheck-color-mode-line-info-face-inactive)
					 ((flycheck-running-p)
					  'powerline-inactive2)
					 ('powerline-inactive2))))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" nil 'l)
				     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-raw " " face1)
				     (powerline-remote face1)
				     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)))
			  (center (list (powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					  (powerline-raw erc-modified-channels-object face2 'l))
					(powerline-major-mode face2 'l)
					(powerline-process face2)
					(powerline-raw ":" face2)
					(powerline-minor-modes face2 'l)
					(powerline-raw " " face2)
					(funcall separator-right face2 face1))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))

;; (powerline-default-theme)
;; (powerline-center-theme)
;; (powerline-center-evil-theme)
;; (powerline-vim-theme)
;; (powerline-nano-theme)
(my-powerline-center-theme)

(provide 'init-powerline)
