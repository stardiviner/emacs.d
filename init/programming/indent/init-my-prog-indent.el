;;; init-my-prog-indent.el --- indent init for programming

;;; Commentary:


;;; Code:

;;; [ Tab ]

;;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
;;; Tab length
(setq default-tab-width 2
      tab-width 2
      standard-indent 2)
(setq-default tab-stop-list (number-sequence 2 120 2))


;;; [ electric-indent-mode ]

(electric-indent-mode t)


;;; [ custom indent functions ]

(global-set-key (kbd "C-c >")
                (lambda (start end)
                  (interactive "rP")
                  (indent-rigidly-right-to-tab-stop start end)))

(global-set-key (kbd "C-c <")
                (lambda (start end)
                  (interactive "rP")
                  (indent-rigidly-left-to-tab-stop start end)))

;;; [ indent-guide ]

(use-package indent-guide
  :ensure t
  :config
  (setq indent-guide-recursive t
        ;; - 0 to avoid zero-column guide line.
        ;; - -1 to show all indent lines.
        indent-guide-threshold 0
        )

  ;; custom indent line char
  ;; 1: use `indent-guide-char'.
  ;; │ ┃  ▍ ┇ ┋ ┊ ┆ ╽ ╿
  (setq indent-guide-char "┃")
  (set-face-attribute 'indent-guide-face nil
                      :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light
                                     (color-darken-name (face-background 'default) 30))
                                    ('dark
                                     (color-lighten-name (face-background 'default) 5)))
                      )

  ;; 2: use face-attribute stipple pixmap data.
  ;; (setq indent-guide-char " ")
  ;; (set-face-attribute 'indent-guide-face nil
  ;;                     :foreground (cl-case (alist-get 'background-mode (frame-parameters))
  ;;                                   ('light
  ;;                                    (color-darken-name (face-background 'default) 30))
  ;;                                   ('dark
  ;;                                    (color-lighten-name (face-background 'default) 5)))
  ;;                     :inherit nil
  ;;                     :stipple (list 7 4 (string 16 0 0 0)))

  ;; (indent-guide-global-mode)

  ;; works with `indent-guide-global-mode'
  (add-to-list 'indent-guide-inhibit-modes 'org-mode)
  (add-to-list 'indent-guide-inhibit-modes 'web-mode)
  (add-to-list 'indent-guide-inhibit-modes 'emacs-lisp-mode)

  (defun my/indent-guide-mode-enable ()
    (unless (member major-mode indent-guide-inhibit-modes)
      (indent-guide-mode 1)))
  (add-hook 'prog-mode-hook #'my/indent-guide-mode-enable)
  )


;;; [ aggressive-indent-mode ]

(use-package aggressive-indent
  :ensure t
  :config
  (setq aggressive-indent-sit-for-time 0.1)

  ;; The variable `aggressive-indent-dont-indent-if' lets you customize when you
  ;; **don't** want indentation to happen.  For instance, if you think it's
  ;; annoying that lines jump around in `c++-mode' because you haven't typed the
  ;; `;' yet, you could add the following clause:
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c++-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line)))))

  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'lua-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'coq-mode)

  ;; (global-aggressive-indent-mode t)
  ;; or
  (dolist (hook '(prog-mode-hook
                  nxml-mode-hook
                  ))
    (add-hook hook #'aggressive-indent-mode))
  )



(provide 'init-my-prog-indent)

;;; init-my-prog-indent.el ends here
