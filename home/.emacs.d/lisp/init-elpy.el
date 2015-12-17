(add-to-list 'load-path (expand-file-name "elpy" user-emacs-directory))
(require-package 'company)
(require-package 'find-file-in-project)
(require-package 'highlight-indentation)
(require-package 'pyvenv)
(require 'elpy)
(elpy-enable)

;; Only use these modules
(setq elpy-modules '(elpy-module-sane-defaults
		     elpy-module-company
		     elpy-module-eldoc
		     elpy-module-highlight-indentation
		     elpy-module-pyvenv
		     elpy-module-yasnippet))

(setq elpy-rpc-backend "rope")

;;Python indent right
;;set in python-mode. yas fallback has to be call-other-command.
;;the default python-indent-region sucks IMO
(defun shift-or-indent (&optional ARG)
  (interactive "P")
  (if mark-active
      (python-indent-shift-right (region-beginning)
				 (region-end))
    (indent-for-tab-command ARG)))

;;C-Tab für autovervollstaendigung
;;If region is active shift region left (in python mode)
;;if not dabbrev expand
(defun dabbrev-or-indent-left (ARG)
  (interactive "*P")
  (if mark-active
      (python-indent-shift-left (region-beginning)
				(region-end))
    (dabbrev-expand arg)))

;;; Test runner
;;Python testing with tox
(defvar tox-history (list))
(defun tox (args)
  (interactive (list (read-string "Tox arguments: " (car tox-history) 'tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" args))))

(defun elpy-test-tox-runner (top file module test)
  "Test the project using tox.

This requires the tox package to be installed."
  (interactive (elpy-test-at-point))
  (let (toxargs '(read-string "Tox arguments: " (car tox-history) 'tox-history))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" toxargs)))))
(put 'elpy-test-tox-runner 'elpy-test-runner-p t)

(defvar pytest-history (list "-vv"))

(defun run-tox-pytest (toxargs pytestargs top file module test)
  (projectile-with-default-dir (projectile-project-root)
    (cond
     (test
	(async-shell-command (concat
			      (format "tox %s -- py.test %s -k \"%s\" %s "
				      toxargs pytestargs test file))))
     (module
      (async-shell-command (format "tox %s -- py.test %s %s" toxargs pytestargs file)))
     (t
      (async-shell-command (format "tox %s -- py.test %s" toxargs pytestargs))))))

(defun elpy-test-tox-pytest-runner (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox."
  (interactive (elpy-test-at-point))
  (let ((toxargs (read-string "Tox arguments: " (car tox-history) 'tox-history))
	(pytestargs (read-string "py.test arguments: " (car pytest-history) 'pytest-history)))
  (run-tox-pytest toxargs pytestargs top file module test)))

(defun elpy-test-tox-pytest-runner-default (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox."
  (interactive (elpy-test-at-point))
  (let ((toxargs (car tox-history))
	(pytestargs (car pytest-history)))
  (run-tox-pytest toxargs pytestargs top file module test)))

(put 'elpy-test-tox-pytest-runner 'elpy-test-runner-p t)
(setq elpy-test-runner 'elpy-test-tox-pytest-runner)

;;; Key bindings
;;Python mode move around code blocks
(global-set-key (kbd "M-p") 'python-nav-backward-block)
(global-set-key (kbd "M-n") 'python-nav-forward-block)
(define-key python-mode-map (kbd "<tab>") 'shift-or-indent)
(define-key python-mode-map (kbd "C-<tab>") 'dabbrev-or-indent-left)
(define-key elpy-mode-map (kbd "C-c t") 'elpy-test-tox-pytest-runner-default)
(require 'init-flycheck)
(define-key elpy-mode-map (kbd "C-c C-p") 'my-previous-error-wrapped)
(define-key elpy-mode-map (kbd "C-c C-n") 'my-next-error-wrapped)



;;; Hooks
(add-hook 'python-mode-hook 'hs-minor-mode)

(provide 'init-elpy)
