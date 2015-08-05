;;; init-my-prog-ruby-on-rails.el --- init Ruby on Rails
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Rhtml (.html.erb, .rhtml) ]

;;; There are three options for editing .rhtml files in Emacs. They are presented here in order of decreasing functionality.
;; - nXhtml-Mode: a package for web development
;; - MuMaMo-Mode: allows multiple major modes in a single buffer
;; - rhtml-Mode: edit rhtml files without using multiple major modes


;;; [ rhtml-mode ]

(require 'rhtml-mode)

;; (add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))

(set-face-attribute 'erb-face nil ; ruby code
                    :background (color-darken-name (face-background 'default) 2)
                    )
(set-face-attribute 'erb-exec-face nil ; exec in <% ... %>
                    :inherit 'erb-face
                    :background (color-darken-name (face-background 'default) 5)
                    )
(set-face-attribute 'erb-out-face nil ; exec in <%= ... %>
                    :inherit 'erb-face
                    :background (color-darken-name (face-background 'default) 5)
                    )
(set-face-attribute 'erb-exec-delim-face nil ; <% ... %>
                    :inherit 'erb-face
                    :foreground "deep pink"
                    :weight 'bold
                    )
(set-face-attribute 'erb-out-delim-face nil ; <%= ... %>
                    :inherit 'erb-face
                    :foreground "red"
                    )


;;; MuMaMo-Mode
;; (require 'mumamo-fun)
;; (setq mumamo-chunk-coloring 'submode-colored)
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.html.erb$" . eruby-html-mumamo))

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
;; - <prefix> -> [C-c p C-r] + [key] (default: [C-c r])
;; - [M-x projectile-rails-on] -- depend on whether is a Rails project root.
;; - [M-x projectile-rails-mode]
;; - [TAB] support for projectile-rails-generate.

(require 'projectile-rails)

(setq projectile-rails-add-keywords t)  ; highlight rails keywords.
(setq projectile-rails-expand-snippet t) ; yasnippet expand skeleton class snippet.
(setq projectile-rails-server-mode-ansi-colors t)

(setq projectile-rails-keymap-prefix (kbd "C-c C-r"))
;; (setq projectile-rails-keymap-prefix (kbd "C-c p C-r"))

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(defun rails-open-browser-development ()
  "Browse Rails development url."
  (interactive)
  (browse-url "http://127.0.0.1:3000"))

(define-key projectile-rails-command-map (kbd "O") 'rails-open-browser-development)



;;; [ helm-rails ]

;; (require 'helm-rails-loaddefs)

;; TODO: test whether has keybinding set by default.
;; (define-key global-map (kbd "s-t") 'helm-rails-controllers)
;; (define-key global-map (kbd "s-y") 'helm-rails-models)
;; (define-key global-map (kbd "s-u") 'helm-rails-views)
;; (define-key global-map (kbd "s-o") 'helm-rails-specs)
;; (define-key global-map (kbd "s-r") 'helm-rails-all)


;;; [ rails-new ] -- Emacs version of command $ rails new ...

;;; Handy emacs command for generating rails application.

;;; Usage:
;;
;; - [M-x rails-new] :: create a new Rails project.

(autoload 'rails-new "rails-new" "Handy emacs command for generating rails application." nil nil)


;;; [ yasnippet-rails ]


;;; [ sass-mode ]


;;; [ slim-mode ]


;;; [ haml-mode ]


(provide 'init-my-prog-ruby-on-rails)

;;; init-my-prog-ruby-on-rails.el ends here
