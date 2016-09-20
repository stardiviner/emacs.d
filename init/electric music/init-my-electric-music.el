;;; init-my-electric-music.el --- init for Electric Music
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SuperCollider ] -- an audio server, programming language, and IDE for sound synthesis and algorithmic composition.

;; from Linux System Package (which only contains *.elc files)
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/SuperCollider/")
;; from Git source code repository (contains *.el source code files)
(add-to-list 'load-path (concat (getenv "HOME")
                                "/Code/SuperCollider/supercollider"
                                "/editors/scel/el/"))

(require 'sclang)

;; (setq sclang-extension-path '("/usr/share/SuperCollider/Extensions"
;;                               "~/.local/share/SuperCollider/Extensions"))

;;; Sclang Interface
(setq sclang-auto-scroll-post-buffer t
      sclang-show-workspace-on-startup nil
      sclang-use-symbol-table t
      sclang-main-run nil
      sclang-main-stop nil
      )

;;; Sclang mode
(setq sclang-indent-level 2)

(define-key sclang-mode-map (kbd "C-c C-z") 'sclang-switch-to-post)

;;; Sclang minor mode

;;; company-mode for SuperCollider
(add-hook 'sclang-mode-hook
          (lambda ()
            (make-local-variable completion-at-point-functions)
            (setq completion-at-point-functions '(sclang-complete-symbol t))))


;;; [ sclang-extensions ] -- A collection of minor modes that improve your SuperCollider experience within Emacs.

(use-package sclang-extensions
  :ensure t
  :config
  (setq sclang-bury-post-on-start? t
        sclang-run-supercollider-if-not-active? nil
        )
  
  (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
  )


(provide 'init-my-electric-music)

;;; init-my-electric-music.el ends here
