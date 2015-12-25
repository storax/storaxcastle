;;; init-elpy --- Configure elpy/python mode

;;; Commentary:

;;; Code:
;; Use Git version
(add-to-list 'load-path (expand-file-name "elpy" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Install requirements manually
;;----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;;Python indent the right way
;;----------------------------------------------------------------------------
;;set in python-mode. yas fallback has to be call-other-command.
;;the default python-indent-region sucks IMO
(defun storax/shift-or-indent (&optional arg)
  "'indent-for-tab-command' with ARG or shift with region."
  (interactive "P")
  (if mark-active
      (python-indent-shift-right (region-beginning)
				 (region-end))
    (indent-for-tab-command arg)))

(defun storax/dabbrev-or-indent-left (arg)
  "Shift left with region else use 'dabbrev-expand' with ARG."
  (interactive "*P")
  (if mark-active
      (python-indent-shift-left (region-beginning)
				(region-end))
    (dabbrev-expand arg)))

;;----------------------------------------------------------------------------
;; Test runner
;;----------------------------------------------------------------------------
;;Python testing with tox
(defvar storax/tox-history (list) "History of tox arguments.")

(defun storax/tox (args)
  "Test with tox.

ARGS is a string with arguments for tox."
  (interactive (list (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" args))))

(defun storax/elpy-test-tox-runner (top file module test)
  "Test the project using tox.

This requires the tox package to be installed.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (elpy-test-at-point))
  (let (toxargs '(read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" toxargs)))))
(put 'storax/elpy-test-tox-runner 'elpy-test-runner-p t)

(defvar storax/pytest-history (list "-vv"))

(defun storax/run-tox-pytest (toxargs pytestargs top file module test)
  "Run tox with pytest.

TOXARGS are the arguments for tox.
PYTESTARGS are the arguments for pytest.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
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

(defun storax/elpy-test-tox-pytest-runner (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (elpy-test-at-point))
  (let ((toxargs (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
	(pytestargs (read-string "py.test arguments: " (car storax/pytest-history) 'storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

(defun storax/elpy-test-tox-pytest-runner-default (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
Call 'storax/elpy-test-tox-pytest-runner' with default values.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (elpy-test-at-point))
  (let ((toxargs (car storax/tox-history))
	(pytestargs (car storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

(put 'storax/elpy-test-tox-pytest-runner 'elpy-test-runner-p t)
(setq elpy-test-runner 'storax/elpy-test-tox-pytest-runner)

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------
;;Python mode move around code blocks
(global-set-key (kbd "M-p") 'python-nav-backward-block)
(global-set-key (kbd "M-n") 'python-nav-forward-block)

(define-key python-mode-map (kbd "<tab>") 'storax/shift-or-indent)
(define-key python-mode-map (kbd "C-<tab>") 'storax/dabbrev-or-indent-left)
(define-key elpy-mode-map (kbd "C-c t") 'storax/elpy-test-tox-pytest-runner-default)

(require 'init-flycheck)
(define-key elpy-mode-map (kbd "C-c C-p") 'storax/previous-error-wrapped)
(define-key elpy-mode-map (kbd "C-c C-n") 'storax/next-error-wrapped)

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------
(defalias 'workon 'pyvenv-workon)

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------
(add-hook 'python-mode-hook 'hs-minor-mode)

(provide 'init-elpy)
;;; init-elpy ends here
