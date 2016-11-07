;;; init-my-prog-refactor.el --- init for programming refactoring
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'my-prog-refactor-map)
  (define-prefix-command 'my-prog-refactor-map))
(global-set-key (kbd "C-c RET") 'my-prog-refactor-map)


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
