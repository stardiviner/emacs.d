;;; init-my-prog-framework-ruby-on-rails.el --- init Ruby on Rails
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Rhtml (.html.erb, .rhtml) ]

;;; There are three options for editing .rhtml files in Emacs. They are presented here in order of decreasing functionality.
;; - nXhtml-Mode: a package for web development
;; - MuMaMo-Mode: allows multiple major modes in a single buffer
;; - rhtml-Mode: edit rhtml files without using multiple major modes


;;; [ rhtml-mode ]

;; (add-to-list 'auto-mode-alist '("\\.html.erb\\'" . rhtml-mode))
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))

(use-package rhtml-mode
  :ensure t
  :config
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
  )


;;; MuMaMo-Mode
;; (require 'mumamo-fun)
;; (setq mumamo-chunk-coloring 'submode-colored)
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.html.erb\\'" . eruby-html-mumamo))

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
;; - `projectile-rails-server' -> [C-x C-q] `inf-ruby-switch-from-compilation'
;;   switch current rails server buffer to inf-ruby buffer.

(use-package projectile-rails
  :ensure t
  :init
  (setq projectile-rails-keymap-prefix (kbd "C-c C-r"))
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  
  :config
  (setq projectile-rails-add-keywords t)
  (setq projectile-rails-expand-snippet t)
  (setq projectile-rails-server-mode-ansi-colors t) ; disable it if it is SLOW.

  ;; Enable Ruby on Rails completion between rhtml tag <% ... %> or <%= ... %>.
  (defadvice company-robe (before web-mode-set-up-ac-sources activate)
    "Set `robe-mode' based on current language before running `company-robe'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language (web-mode-language-at-pos)))
          (if (string= web-mode-cur-language "erb")
              (unless robe-mode (robe-mode))
            (if robe-mode (robe-mode -1))))))

  (defun my-projectile-rails-setup ()
    (my-company-add-backend-locally 'company-robe)
    
    ;; `nil': disable auto complete, manually.
    (setq-local company-idle-delay .3)

    (defun rails-open-browser-development ()
      "Browse Rails development url."
      (interactive)
      (browse-url "http://127.0.0.1:3000"))

    (define-key projectile-rails-mode-run-map (kbd "O")
      'rails-open-browser-development)
    )
  
  (add-hook 'projectile-rails-mode-hook 'my-projectile-rails-setup)
  )


;;; [ rails-new ] -- Emacs version of command $ rails new ...

;;; Usage:
;;
;; - [M-x rails-new] :: create a new Rails project.
;; - [M-x rails-plugin-new] :: create a new Rails plugin.

;; (use-package rails-new
;;   :ensure t)


;;; [ yasnippet-rails ]


;;; [ sass-mode ]


;;; [ slim-mode ]


;;; [ haml-mode ]



;;; load rails environment for completion

;; TODO:
;; (comint-send-string (inf-ruby-proc) (format "require 'sqlite'"))
;; (process-send-string (inf-ruby-proc) "require 'rails'")


(provide 'init-my-prog-framework-ruby-on-rails)

;;; init-my-prog-framework-ruby-on-rails.el ends here
