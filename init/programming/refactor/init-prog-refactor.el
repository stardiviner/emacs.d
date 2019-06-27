;;; init-prog-refactor.el --- init for programming refactoring
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'refactor-prefix)
  (define-prefix-command 'refactor-prefix))
;; (global-set-key (kbd "M-RET") 'refactor-prefix)
(global-set-key (kbd "<M-return>") 'refactor-prefix)
;; (global-set-key (kbd "C-c RET") 'refactor-prefix)


;;; [ emacs-refactor (emr) ] -- provides language-specific refactoring support for Emacs.

(use-package emr
  :ensure t
  :delight emr-c-mode
  :bind (:map prog-mode-map ("<M-return>" . emr-show-refactor-menu))
  :init (add-hook 'prog-mode-hook #'emr-initialize))


(provide 'init-prog-refactor)

;;; init-prog-refactor.el ends here
