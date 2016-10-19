;;; init-my-prog-framework-ruby-on-rails.el --- init Ruby on Rails
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rhtml-mode ]

(use-package rhtml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
  
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


;;; [ projectile-rails ]

(use-package projectile-rails
  :ensure t
  :defer t
  :init
  ;; NOTE: some settings need to be set before required or loaded.
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

    ;; optimize Rails company-robe completion.
    (setq-local company-minimum-prefix-length 4)
    ;; `nil': disable auto complete, manually.
    (setq-local company-idle-delay 0.2)

    (defun rails-open-browser-development ()
      "Browse Rails development url."
      (interactive)
      (browse-url "http://127.0.0.1:3000"))

    (define-key projectile-rails-mode-run-map (kbd "O")
      'rails-open-browser-development)
    )
  
  (add-hook 'projectile-rails-mode-hook 'my-projectile-rails-setup)
  )


(provide 'init-my-prog-framework-ruby-on-rails)

;;; init-my-prog-framework-ruby-on-rails.el ends here
