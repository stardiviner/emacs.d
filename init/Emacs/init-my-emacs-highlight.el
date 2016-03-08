;;; init-my-emacs-highlight.el --- init for highlight
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-highlight-prefix)
  (define-prefix-command 'my-highlight-prefix))
(global-set-key (kbd "C-x h") 'my-highlight-prefix)
(define-key my-search-prefix (kbd "h") 'my-highlight-prefix)


;;; [ hi-lock ]

;; unbind default keybindings
(with-eval-after-load 'hi-lock
  (unbind-key (kbd "C-x w .") hi-lock-map)
  (unbind-key (kbd "C-x w b") hi-lock-map)
  (unbind-key (kbd "C-x w h") hi-lock-map)
  (unbind-key (kbd "C-x w i") hi-lock-map)
  (unbind-key (kbd "C-x w l") hi-lock-map)
  (unbind-key (kbd "C-x w p") hi-lock-map)
  (unbind-key (kbd "C-x w r") hi-lock-map)
  )
;; re-bind keybindings
(unless (boundp 'hi-lock-prefix)
  (define-prefix-command 'hi-lock-prefix))
(define-key my-highlight-prefix (kbd "h") 'hi-lock-prefix)

(define-key hi-lock-prefix (kbd "s") 'highlight-symbol-at-point)
(define-key hi-lock-prefix (kbd "p") 'highlight-phrase)
(define-key hi-lock-prefix (kbd "r") 'highlight-regexp)
(define-key hi-lock-prefix (kbd "u") 'unhighlight-regexp)
(define-key hi-lock-prefix (kbd "F") 'hi-lock-find-patterns)
(define-key hi-lock-prefix (kbd "P") 'hi-lock-write-interactive-patterns)
(define-key hi-lock-prefix (kbd "l") 'highlight-lines-matching-regexp)


;;; [ highlight-symbol ] -- automatic and manual symbol highlighting for Emacs

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 3
        ;; highlight-symbol-border-pattern '("\\_<" . "\\_>")
        highlight-symbol-colors '("brown" "tomato" "dark green" "dark slate gray"
                                  "deep pink" "cyan" "yellow"
                                  )
        highlight-symbol-foreground-color nil ; nil: keep original color.
        )

  (set-face-attribute 'highlight-symbol-face nil
                      :background "midnight blue"
                      :slant 'italic)

  ;; (add-hook 'prog-mode-hook 'highlight-symbol-mode)

  (unless (boundp 'my-highlight-symbol-prefix)
    (define-prefix-command 'my-highlight-symbol-prefix))
  (define-key my-highlight-prefix (kbd "s") 'my-highlight-symbol-prefix)

  ;; setting up keybindings
  (define-key my-highlight-symbol-prefix (kbd "h") 'highlight-symbol-at-point)
  (define-key my-highlight-symbol-prefix (kbd "n") 'highlight-symbol-next)
  (define-key my-highlight-symbol-prefix (kbd "p") 'highlight-symbol-prev)
  (define-key my-highlight-symbol-prefix (kbd "N") 'highlight-symbol-next-in-defun)
  (define-key my-highlight-symbol-prefix (kbd "P") 'highlight-symbol-prev-in-defun)
  (define-key my-highlight-symbol-prefix (kbd "l") 'highlight-symbol-list-all)
  (define-key my-highlight-symbol-prefix (kbd "o") 'highlight-symbol-occur)
  (define-key my-highlight-symbol-prefix (kbd "C") 'highlight-symbol-count)
  (define-key my-highlight-symbol-prefix (kbd "r") 'highlight-symbol-query-replace)
  (define-key my-highlight-symbol-prefix (kbd "U") 'highlight-symbol-remove-all)
  (define-key my-highlight-symbol-prefix (kbd "u") 'highlight-symbol-at-point)
  (define-key my-highlight-symbol-prefix (kbd "m") 'highlight-symbol-nav-mode)
  )


;;; [ highlight-thing ] -- global minor mode to highlight the thing under point.

(use-package highlight-thing
  ;; :ensure t
  :config
  ;; (setq highlight-thing-what-thing 'word) ; 'symbol
  (setq highlight-thing-delay-seconds 1.0)
  (setq highlight-thing-limit-to-defun t)
  (set-face-attribute 'highlight-thing nil
                      :foreground "white"
                      :background "forest green"
                      )
  ;; (global-highlight-thing-mode)
  (add-hook 'prog-mode-hook 'highlight-thing-mode)
  )


;;; highlight-quoted

;; (load-file (expand-file-name "init/extensions/highlight-quoted.el" user-emacs-directory))

;; (add-hook 'prog-mode-hook 'highlight-quoted-mode)

;; ;; (setq highlight-quoted-highlight-symbols t)

;; (eval-after-load 'highlight-quoted
;;   (progn
;;     (set-face-attribute 'highlight-quoted-quote nil
;;                         :inherit 'font-lock-keyword-face)
;;     (set-face-attribute 'highlight-quoted-symbol nil
;;                         :inherit 'font-lock-constant-face))
;;   )


;;; highlight-numbers

;; (load-file (expand-file-name "init/extensions/highlight-numbers.el" user-emacs-directory))
;;
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)
;;
;; ;; (setq highlight-numbers-modelist)
;;
;; (eval-after-load 'highlight-numbers
;;   (set-face-attribute 'highlight-numbers-number nil
;;                       :inherit 'font-lock-constant-face))


;;; [ highlight-escape-sequences ]

;; (use-package highlight-escape-sequences)

;; NOTE: add the following line to the theme.
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)


;;; [ highlight-stages ]

;; (use-package highlight-stages)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 emacs-lisp-mode-hook
;;                 scheme-mode-hook
;;                 clojure-mode-hook
;;                 cider-repl-mode-hook
;;                 ))
;;   (add-hook hook 'highlight-stages-mode))
;;
;; ;; (highlight-stages-global-mode 1)
;;
;; (set-face-attribute 'highlight-stages-negative-level-face nil
;;                     :background "#003745")
;; (set-face-attribute 'highlight-stages-level-1-face nil
;;                     :background "#001e26")
;; (set-face-attribute 'highlight-stages-level-2-face nil
;;                     :background "#001217")
;; (set-face-attribute 'highlight-stages-level-3-face nil
;;                     :background "#000608")
;; (set-face-attribute 'highlight-stages-higher-level-face nil
;;                     :background "#000000")




(provide 'init-my-emacs-highlight)

;;; init-my-emacs-highlight.el ends here
