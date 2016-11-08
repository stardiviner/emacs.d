;;; init-my-prog-refactor.el --- init for programming refactoring
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'refactor-prefix)
  (define-prefix-command 'refactor-prefix))
(global-set-key (kbd "M-RET") 'refactor-prefix)
(global-set-key (kbd "C-c RET") 'refactor-prefix)


;;; [ emacs-refactor (emr) ] -- Emacs Refactor (EMR) provides language-specific refactoring support for Emacs.

(use-package emr
  :ensure t
  :init
  (eval-after-load "emr" '(emr-initialize))

  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
  (define-key prog-mode-map (kbd "C-c RET") 'emr-show-refactor-menu)
  )


(provide 'init-my-prog-refactor)

;;; init-my-prog-refactor.el ends here
