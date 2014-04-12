;;; init-my-prog-lang-css.el --- init CSS settings language for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ css-mode ]

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

(setq auto-mode-alist
      (append '(("\\.css$" . css-mode))
              auto-mode-alist))


;;; [ css-eldoc ]

(require 'css-eldoc)

(turn-on-css-eldoc)

;;; ------------------------------------------------------------------------------------
;;; Attention!

;; If your less mode's isearch became really slow, add the following code to your .emacs

;; (defun isearch-forward-noeldoc ()
;;   "close eldoc temperaily"
;;   (interactive)
;;   (eldoc-mode -1)
;;   (isearch-forward)
;;   (eldoc-mode 1))
;; (add-hook 'less-css-mode-hook (lambda ()
;;                 (local-set-key [remap isearch-forward] 'isearch-forward-noeldoc)))

;; (defun isearch-backward-noeldoc ()
;;   "close eldoc temperaily"
;;   (interactive)
;;   (eldoc-mode -1)
;;   (isearch-backward)
;;   (eldoc-mode 1))
;; (add-hook 'less-css-mode-hook (lambda ()
;;                 (local-set-key [remap isearch-backward] 'isearch-backward-noeldoc)))
;;; ------------------------------------------------------------------------------------




(provide 'init-my-prog-lang-css)

;;; init-my-prog-lang-css.el ends here
