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

(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if window-system
      (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
	(when backend
	  (format " %s %s"
		  (char-to-string #xf020)
		  (vc-working-revision (buffer-file-name (current-buffer)) backend))))
      (format-mode-line '(vc-mode vc-mode)))))

(defun my-powerline-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
;;			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (face2 (if active
				     (cond ((flycheck-has-current-errors-p 'error)
					  'flycheck-color-mode-line-error-face-active)
					 ((flycheck-has-current-errors-p 'warning)
					  'flycheck-color-mode-line-warning-face-active)
					 ((flycheck-has-current-errors-p 'info)
					  'flycheck-color-mode-line-info-face-active)
					 ('powerline-active1))
				   (cond ((flycheck-has-current-errors-p 'error)
					  'flycheck-color-mode-line-error-face-inactive)
					 ((flycheck-has-current-errors-p 'warning)
					  'flycheck-color-mode-line-warning-face-inactive)
					 ((flycheck-has-current-errors-p 'info)
					  'flycheck-color-mode-line-info-face-inactive)
					 ('powerline-inactive1))))
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
				     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
				     (powerline-hud face2 face1)))
			  (center (list (powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					  (powerline-raw erc-modified-channels-object face2 'l))
					(powerline-major-mode face2 'l)
					(powerline-process face2)
					(powerline-raw " :" face2)
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

(powerline-raw global-mode-string)
(provide 'init-powerline)
