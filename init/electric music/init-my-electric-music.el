;;; init-my-electric-music.el --- init for Electric Music
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SuperCollider ] -- an audio server, programming language, and IDE for sound synthesis and algorithmic composition.

;; from Linux System Package (which only contains *.elc files)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/SuperCollider/")
;; from Git source code repository (contains *.el source code files)
;; (add-to-list 'load-path (concat (getenv "HOME")
;;                                 "/Code/SuperCollider/supercollider"
;;                                 "/editors/scel/el/"))

(require 'sclang)

(require 'w3m)

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

;; switch between sclang special buffers ([C-c C-z], [C-c C-w])
(define-key sclang-mode-map (kbd "C-c C-z") 'sclang-switch-to-post)
(define-key sclang-mode-map (kbd "C-c C-w") 'sclang-switch-to-workspace)
;; (define-key sclang-post-buffer-mode-map (kbd "C-c C-z") 'sclang-switch-to-src)
;; (define-key sclang-post-buffer-mode-map (kbd "C-c C-w") 'sclang-switch-to-workspace)

;; definitions navigation
(define-key sclang-mode-map (kbd "M-.") 'sclang-find-definitions)
(define-key sclang-mode-map (kbd "M-,") 'sclang-pop-definition-mark)


;;; Sclang minor mode

;;; auto-complete for SuperCollider
;; (add-hook 'sclang-mode-hook
;;           (lambda ()
;;             (company-mode -1)
;;             (setq-local ac-auto-start 1)
;;             ))

;;; company-mode for SuperCollider
(add-hook 'sclang-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'sclang-complete-symbol nil t)
            ))

;;; auto start SuperCollider inferior process
(defun my-sclang-auto-start ()
  "Start SuperCollider inferior process."
  (interactive)
  (unless (or (equal (buffer-name) sclang-post-buffer)
              (sclang-get-process))
    (sclang-start)))

;; (add-hook 'sclang-mode-hook #'my-sclang-auto-start)

(define-key sclang-mode-map (kbd "C-c C-s") 'my-sclang-auto-start)
(define-key sclang-mode-map (kbd "C-c M-s") 'sclang-main-stop)
(define-key sclang-mode-map (kbd "C-c M-r") 'sclang-main-run)


;;; [ sclang-extensions ] -- A collection of minor modes that improve your SuperCollider experience within Emacs.

(use-package sclang-extensions
  :ensure t
  :defer t
  :config
  (setq sclang-bury-post-on-start? t
        sclang-run-supercollider-if-not-active? nil ; run SuperCollider process will mute System sound.
        )
  
  (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
  )

(provide 'init-my-electric-music)

;;; init-my-electric-music.el ends here
