;;; init.el --- Emacs init file.

;;; Commentary:


;;; Code:

;;; [ Debug ]

(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)


;;; [ profiler ]

;; (profiler-start 'cpu+mem)


;;; benchmark

;; (require 'init-my-emacs-benchmark)


;;; [ things before load ]

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

(toggle-frame-maximized)


;;; some settings for easy usage

(defalias 'yes-or-no-p 'y-or-n-p)



(setq projectile-rails-keymap-prefix (kbd "C-c C-r")) ; or [C-c C-p]


;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


;;; [ package.el ]

(load "~/.emacs.d/init/init-my-packages.el")
(load "~/.emacs.d/init/init-my-pm-package.el")
(require 'init-my-pm-package)

(require 'use-package)

(require 'color)
(require 'color-theme)


;;; my custom functions
(require 'init-my-library)
(require 'init-my-functions)



(require 'init-my-keybindings)
(require 'init-my-emacs-keybinding)
(require 'init-my-prog-programming)

(require 'init-my-emacs-completion)
(require 'init-my-prog-complete)

(require 'init-my-prog-lang-lisp)
(require 'init-my-prog-lang-emacs-lisp)
