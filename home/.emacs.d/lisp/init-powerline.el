(require-package 'powerline)
(require-package 'octicons)
(require 'powerline)
(require 'octicons)
(defface powerline-octicons-active '((t (:inherit mode-line :background "grey22" :width condensed :family "github-octicons")))
  "Powerline face vc active."
  :group 'powerline)

(defface powerline-octicons-inactive '((t (:inherit mode-line-inactive :background "#4F4F4F" :width condensed :family "github-octicons")))
  "Powerline face vc inactive."
  :group 'powerline)


(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if window-system
      (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
	(when backend
	  (format " %s %s %s"
		  backend
		  (char-to-string #xe0a0)
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
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (face-vc (if active 'powerline-octicons-active 'powerline-octicons-inactive))
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
				     (powerline-vc face-vc)))
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

(provide 'init-powerline)
