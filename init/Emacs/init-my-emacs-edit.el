;;; init-my-emacs-edit.el --- init Emacs editing

;;; Commentary:

;;; Code:


(unless (boundp 'my-edit-prefix)
  (define-prefix-command 'my-edit-prefix))
(global-set-key (kbd "C-c e") 'my-edit-prefix)


;;;_ Edit

;; typed text replaces the active selection
(delete-selection-mode t)

;;;_ Rectangle

;;; - [C-x SPC] / [C-x r r m] (custom keybinding) :: `rectangle-mark-mode'


;;;_ [ edit-server ]

;; A Chrome "clone" of It's All Text for spawning an editor to edit text areas in browsers with Emacs.
;; https://github.com/stsquad/emacs_chrome

(use-package edit-server
  :ensure t
  :config
  (setq edit-server-new-frame t)
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("stackoverflow\\.com" . markdown-mode)
          ("segmentfault\\.com" . markdown-mode)
          ))
  
  (edit-server-start)
  )


;;;_ whitespace-mode

;; (require 'whitespace)

;; ;; automatically clean up bad whitespace
;; (setq whitespace-action '(auto-cleanup))
;; ;; only show bad whitespace
;; (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

;; (whitespace-mode 1)
;; (global-whitespace-mode)


(require 'init-my-emacs-kill-ring)
(require 'init-my-emacs-region)
(require 'init-my-emacs-macro)
(require 'init-my-emacs-edit-electric)
(require 'init-my-emacs-edit-narrow)
(require 'init-my-emacs-edit-tabulate)
(require 'init-my-emacs-edit-multiple-cursors)
(require 'init-my-emacs-edit-sudo)


(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
