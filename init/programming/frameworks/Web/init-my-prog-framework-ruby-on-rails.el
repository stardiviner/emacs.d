;;; init-my-prog-framework-ruby-on-rails.el --- init Ruby on Rails
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rhtml-mode ]

(use-package rhtml-mode
  :ensure t
  :mode (("\\.html.erb\\'" . rhtml-mode)
         ("\\.rhtml\\'" . rhtml-mode)))

(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-expanders '("r/" . "<%= | %>")) ; ruby erb: <%= | %>.
  (add-to-list 'web-mode-expanders '("%/" . "<%= | %>")) ; ruby erb: <%= | %>.
  (add-to-list 'web-mode-expanders '("R/" . "<% | %>")) ; ruby erb: <% | %>.
  (add-to-list 'web-mode-expanders '("#/" . "<%# | %>")) ; ruby erb comment: <%# | %>.
  (add-to-list 'web-mode-expanders '("e/" . "<% end %>")) ; ruby erb end: <% end %>
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
  (add-hook 'web-mode-hook 'robe-mode)
  
  ;; (defadvice company-robe (before web-mode-set-up-ac-sources activate)
  ;;   "Set `robe-mode' based on current language before running `company-robe'."
  ;;   (if (equal major-mode 'web-mode)
  ;;       (let ((web-mode-cur-language (web-mode-language-at-pos)))
  ;;         (if (string= web-mode-cur-language "erb")
  ;;             (unless robe-mode (robe-mode))
  ;;           (if robe-mode (robe-mode -1))))))

  (advice-add 'company-robe :before
              #'(lambda (&rest _)
                  (if (equal major-mode 'web-mode)
                      (let ((web-mode-cur-language (web-mode-language-at-pos)))
                        (if (string= web-mode-cur-language "erb")
                            (unless robe-mode (robe-mode))
                          (if robe-mode (robe-mode -1)))))))

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
