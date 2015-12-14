;;;; Helm
(add-to-list 'load-path (expand-file-name "helm" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "helm-swoop" user-emacs-directory))

(require 'helm-config)

;;; Helm Variables
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-dabbrev-cycle-threshold 5
      helm-kill-ring-threshold 1
      ido-use-virtual-buffers t ; Needed in helm-buffers-list

)

;;; Helm activate modes
(helm-mode 1)
(helm-adaptive-mode 1)
(helm-push-mark-mode 1)

;;; Custom Helm Functions


;;; Key Bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-h C-i") 'helm-imenu)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;; Helm-Swoop
(require 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

(provide 'init-helm)
