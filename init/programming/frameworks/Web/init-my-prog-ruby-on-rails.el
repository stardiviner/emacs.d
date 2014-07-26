;;; init-my-prog-ruby-on-rails.el --- init Ruby on Rails
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_
;;; [ Rinari ] -- A Ruby on Rails minor mode for Emacs

;;; Rinari Is Not A Ruby IDE.
;;;
;;; Well, OK it kind of is. Rinari is a set of Emacs Lisp functions aimed
;;; towards making Emacs (or XEmacs) into a top-notch Ruby on Rails development
;;; environment.
;;;
;;; Currently Rinari focuses on the core functionality most everyone would use when working on a Rails applications including...
;; - Navigation between files in your Rails project (see Navigation)
;; - Facilitation of Test/Behavior Driven Development (see Test/Behavior Driven Development)
;; - Execution of tests, consoles, and web-servers (see Execution)
;;
;;; Usage:
;; - [C-h b] :: show all key bindings of Rinari.
;; - [M-x rinari-<tab>] :: all rinari functions.
;; - TAGS
;;   To have Rinari automatically update your tags-file-name variable to point
;;   to the tags of your current rails project, set rinari-tags-file-name (see
;;   Navigation) to the path to your tags file relative to the root of your
;;   rails applications.
;;   (setq rinari-tags-file-name "TAGS")

(require 'rinari)

(add-hook 'rinari-minor-mode-hook
          '(lambda ()
             (setq yas-extra-modes (cons 'rails-mode yas-extra-modes))))

;; (setq rinari-tags-file-name "TAGS")

;; (setq rinari-major-modes
;;       rinari-minor-mode-prefixes
;;       rinari-exclude-major-modes
;;       rinari-minor-mode-keybindings
;;       )

(global-rinari-mode t)



;;; Misc Functions
;;; --------------------------------------
(defun rails-open-browse-development ()
  "Browse Rails development url."
  (interactive)
  (browse-url "http://127.0.0.1:3000"))

(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (define-key rinari-minor-mode-map (kbd "b") 'rails-open-browse-development)))
;;; --------------------------------------


;;; Rhtml setup

;;; There are three options for editing .rhtml files in Emacs. They are presented here in order of decreasing functionality.
;; - nXhtml-Mode: a package for web development
;; - MuMaMo-Mode: allows multiple major modes in a single buffer
;; - rhtml-Mode: edit rhtml files without using multiple major modes

(require 'rhtml-mode)

(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))

;;; MuMaMo-Mode
;; (require 'mumamo-fun)
;; (setq mumamo-chunk-coloring 'submode-colored)
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

;;; [ nXhtml-Mode ]
;; (setq nxhtml-global-minor-mode t
;;       mumamo-chunk-coloring 'submode-colored
;;       nxhtml-skip-welcome t
;;       indent-region-mode t
;;       rng-nxml-auto-validate-flag nil
;;       nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))




;;; [ projectile-rails ]

;;; Usage:
;;; - <prefix> -> [C-c p r] + [key] (default: [C-c r])

(require 'projectile-rails)

;; FIXME: this does not work!
(setq projectile-rails-keymap-prefix (kbd "C-c p C-r"))
;; or
;; (custom-set-variables
;;  '(projectile-rails-keymap-prefix (kbd "C-c p C-r")))
;; or
;; (eval-after-load "projectile-rails"
;;   '(progn
;;      (define-key projectile-rails-mode-map (kbd "C-c p C-r") 'projectile-rails-keymap-prefix)
;;      ;; (setq projectile-rails-keymap-prefix (kbd "C-c p C-r"))
;;      ))

(setq projectile-rails-add-keywords t)  ; highlight rails keywords.
(setq projectile-rails-expand-snippet t) ; yasnippet expand skeleton class snippet.

(add-hook 'projectile-mode-hook 'projectile-rails-on)



(provide 'init-my-prog-ruby-on-rails)

;;; init-my-prog-ruby-on-rails.el ends here
