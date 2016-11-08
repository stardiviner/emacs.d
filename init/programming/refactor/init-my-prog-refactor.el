;;; init-my-prog-refactor.el --- init for programming refactoring
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'refactor-prefix)
  (define-prefix-command 'refactor-prefix))
(global-set-key (kbd "M-RET") 'refactor-prefix)
;; (global-set-key (kbd "C-c RET") 'refactor-prefix)


;;; [ emacs-refactor (emr) ] -- provides language-specific refactoring support for Emacs.

(use-package emr
  :ensure t
  :config
  (define-key refactor-prefix (kbd "M-RET") 'emr-show-refactor-menu)
  (eval-after-load "emr" '(emr-initialize))
  )


(provide 'init-my-prog-refactor)

;;; init-my-prog-refactor.el ends here
