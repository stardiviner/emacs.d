;;; init-my-prog-indent.el --- indent init for programming

;;; Commentary:


;;; Code:


(setq standard-indent 4)


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
  :defer t
  :init
  (defvar indent-guide-inhibit-modes nil)
  ;; works with `indent-guide-global-mode'
  (add-to-list 'indent-guide-inhibit-modes 'org-mode)
  (add-to-list 'indent-guide-inhibit-modes 'web-mode)
  (add-to-list 'indent-guide-inhibit-modes 'emacs-lisp-mode)
  
  (dolist (hook '(prog-mode-hook
                  emacs-lisp-mode-hook
                  lisp-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook
                  python-mode-hook
                  c-mode-hook
                  c++-mode-hook
                  js-mode-hook
                  js2-mode-hook
                  js3-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (unless (member major-mode indent-guide-inhibit-modes)
                  (indent-guide-mode 1))
                )))
  
  :config
  (setq indent-guide-delay nil
        indent-guide-recursive t
        ;; - 0 to avoid zero-column guide line.
        ;; - -1 to show all indent lines.
        indent-guide-threshold 0
        )

  ;; custom indent line char
  ;; 1: use `indent-guide-char'.
  ;; : │ ┃ ▏┃ | ❘ │ ┃ ▍ ┇ ┋ ┊ ┆ ╽ ╿ ▏▕
  (setq indent-guide-char "╿")
  (set-face-attribute 'indent-guide-face nil
                      ;; :foreground "olive drab"
                      ;; :foreground "dark violet"
                      :foreground (color-lighten-name
                                   (face-background 'default) 6)
                      )

  ;; 2: use face-attribute stipple pixmap data.
  ;; (setq indent-guide-char " ")
  ;; (set-face-attribute 'indent-guide-face nil
  ;;                     :foreground "cyan"
  ;;                     :inherit nil
  ;;                     :stipple (list 7 4 (string 16 0 0 0)))

  ;; (indent-guide-global-mode)
  )


;;; [ aggressive-indent-mode ]

(use-package aggressive-indent
  :ensure t
  :defer t
  :init
  (global-aggressive-indent-mode)
  ;; or
  ;; (dolist (hook '(prog-mode-hook
  ;;                 nxml-mode-hook
  ;;                 ))
  ;;   (add-hook hook #'aggressive-indent-mode))
  
  :config
  ;; The variable `aggressive-indent-dont-indent-if' lets you customize when you
  ;; **don't** want indentation to happen.  For instance, if you think it's
  ;; annoying that lines jump around in `c++-mode' because you haven't typed the
  ;; `;' yet, you could add the following clause:
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c++-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line)))))


  (add-to-list 'aggressive-indent-excluded-modes 'ruby-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'enh-ruby-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'inf-ruby-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'lua-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
  )



(provide 'init-my-prog-indent)

;;; init-my-prog-indent.el ends here
