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

;;; [ symbol-overlay ] -- highlighting symbols with keymap-enabled overlays.

(use-package symbol-overlay
  :ensure t
  :bind (:map my-highlight-prefix
              ("h" . symbol-overlay-put)
              ("H" . symbol-overlay-toggle-in-scope)
              ("p" . symbol-overlay-jump-prev)
              ("n" . symbol-overlay-jump-next)
              ("d" . symbol-overlay-jump-to-definition)
              ("c" . symbol-overlay-remove-all)
              ("r" . symbol-overlay-rename)
              ("q" . symbol-overlay-query-replace)
              ("P" . symbol-overlay-switch-backward)
              ("N" . symbol-overlay-switch-forward))
  )


(provide 'init-my-emacs-highlight)

;;; init-my-emacs-highlight.el ends here
