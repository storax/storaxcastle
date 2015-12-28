;;; init-shell-pop --- Shell popup

;;; Commentary:

;;; Code:

(require-package 'shell-pop)
(require 'shell-pop)

(setq shell-pop-shell-type (quote ("multi-term" "*zsh*" (lambda nil (multi-term))))
      shell-pop-universal-key "<f10>"
      shell-pop-full-span t
      shell-pop-window-position "bottom"
      shell-pop-default-directory nil
      shell-pop-window-height 60)

(provide 'init-shell-pop)
;;; init-shell-pop ends here
