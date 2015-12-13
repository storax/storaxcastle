(setq redisplay-dont-pause t
      scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; No spashscreen, scratch message and default python mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'python-mode)

;; Hide tool and menu bar
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1))

;; Show little dashes to indicate empy lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(provide 'init-window)
