;;; init-stack-overflow.el --- init for Stack Exchange
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SX ] -- Stack Exchange

(use-package sx
  :ensure t
  :defer t
  :init
  (unless (boundp 'stack-overflow-prefix)
    (define-prefix-command 'stack-overflow-prefix))
  (define-key tools-prefix (kbd "s") 'stack-overflow-prefix)
  :bind (:map stack-overflow-prefix
              ("q" . sx-tab-all-questions)
              ("i" . sx-inbox)
              ("o" . sx-open-link)
              ("u" . sx-tab-unanswered-my-tags)
              ("a" . sx-ask)
              ("s" . sx-search)
              )
  :config
  (setq sx-question-mode-display-buffer-function #'pop-to-buffer
        sx-question-mode-recenter-line 2)
  )


(provide 'init-stack-overflow)

;;; init-stack-overflow.el ends here
