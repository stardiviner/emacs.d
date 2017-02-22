;;; init-my-emacs-highlight.el --- init for highlight
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-highlight-prefix)
  (define-prefix-command 'my-highlight-prefix))
(global-set-key (kbd "M-g h") 'my-highlight-prefix)

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

;; rebind commands
(define-key my-highlight-prefix (kbd "C-s") 'highlight-regexp)
(define-key my-highlight-prefix (kbd "M-u") 'unhighlight-regexp)
(define-key my-highlight-prefix (kbd "M-s") 'highlight-lines-matching-regexp)

;;; [ highlight-symbol ] -- automatic and manual symbol highlighting for Emacs

(use-package highlight-symbol
  :ensure t
  :init
  ;; (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 2
        ;; highlight-symbol-border-pattern '("\\_<" . "\\_>")
        highlight-symbol-colors '("brown" "tomato" "dark green" "dark slate gray"
                                  "deep pink" "cyan" "yellow"
                                  )
        highlight-symbol-foreground-color nil ; nil: keep original color.
        )

  ;; setting up keybindings
  (define-key my-highlight-prefix (kbd "h") 'highlight-symbol-at-point)
  (define-key my-highlight-prefix (kbd "n") 'highlight-symbol-next)
  (define-key my-highlight-prefix (kbd "p") 'highlight-symbol-prev)
  (define-key my-highlight-prefix (kbd "M-n") 'highlight-symbol-next-in-defun)
  (define-key my-highlight-prefix (kbd "M-p") 'highlight-symbol-prev-in-defun)
  (define-key my-highlight-prefix (kbd "l") 'highlight-symbol-list-all)
  (define-key my-highlight-prefix (kbd "o") 'highlight-symbol-occur)
  (define-key my-highlight-prefix (kbd "C") 'highlight-symbol-count)
  (define-key my-highlight-prefix (kbd "M-%") 'highlight-symbol-query-replace)
  (define-key my-highlight-prefix (kbd "u") 'highlight-symbol-remove-all)
  (define-key my-highlight-prefix (kbd "m") 'highlight-symbol-nav-mode)
  )


(provide 'init-my-emacs-highlight)

;;; init-my-emacs-highlight.el ends here
