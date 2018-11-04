;;; init-prog-code.el --- init Code writting for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

(unless (boundp 'prog-code-prefix)
  (define-prefix-command 'prog-code-prefix))
(global-set-key (kbd "C-c c") 'prog-code-prefix)

;;; [ subword ] -- editing code WithCamelCaseWritingLikeThis

(use-package subword
  :ensure t
  :defer t
  :delight subword-mode
  ;; :init (add-hook 'prog-mode-hook 'subword-mode)
  )

;;; [ glasses ] -- make CamelCase identifiers easy look.

;; (use-package glasses
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; (add-hook 'prog-mode-hook 'glasses-mode)
;;   (add-hook 'java-mode-hook 'glasses-mode)
;;   )


(provide 'init-prog-code)

;;; init-prog-code.el ends here
