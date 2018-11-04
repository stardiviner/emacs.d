;;; init-emacs-highlight.el --- init for highlight
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'highlight-prefix)
  (define-prefix-command 'highlight-prefix))
(global-set-key (kbd "M-g h") 'highlight-prefix)

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
(define-key highlight-prefix (kbd "M-p") 'highlight-symbol-at-point)
(define-key highlight-prefix (kbd "M-r") 'highlight-regexp)
(define-key highlight-prefix (kbd "M-w") 'highlight-phrase)
(define-key highlight-prefix (kbd "M-u") 'unhighlight-regexp)
(define-key highlight-prefix (kbd "M-l") 'highlight-lines-matching-regexp)

;;; [ symbol-overlay ] -- highlighting symbols with keymap-enabled overlays.

(use-package symbol-overlay
  :ensure t
  :defer t
  :delight symbol-overlay-mode
  :bind (:map highlight-prefix
              ("h" . symbol-overlay-put)
              ("M-h" . symbol-overlay-toggle-in-scope)
              ("t" . symbol-overlay-toggle-in-scope)
              ("s" . symbol-overlay-isearch-literatelly)
              ("p" . symbol-overlay-jump-prev)
              ("n" . symbol-overlay-jump-next)
              ("d" . symbol-overlay-jump-to-definition)
              ("M-," . symbol-overlay-echo-mark)
              ("c" . symbol-overlay-save-symbol)
              ("k" . symbol-overlay-remove-all)
              ("r" . symbol-overlay-rename)
              ("q" . symbol-overlay-query-replace)
              ("P" . symbol-overlay-switch-backward)
              ("N" . symbol-overlay-switch-forward))
  :config
  (set-face-attribute 'symbol-overlay-default-face nil
                      :inherit t :foreground nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light
                                     (color-darken-name (face-background 'default) 10))
                                    ('dark
                                     (color-darken-name (face-background 'default) 5))))
  )


(provide 'init-emacs-highlight)

;;; init-emacs-highlight.el ends here
