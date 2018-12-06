;;; init-prog-lang-scala.el --- init for Scala
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ scala-mode ]

(use-package scala-mode
  :ensure t
  :config
  ;; let the awesome Smartparens take care of parentheses in Scala buffers.
  (remove-hook 'post-self-insert-hook
               'scala-indent:indent-on-parentheses)

  (sp-local-pair 'scala-mode "(" nil
                 :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil
                 :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  )


;;; [ sbt-mode ]

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :bind (:map scala-mode-map
              ("C-c C-s" . run-scala))
  )


;;; [ ENSIME ]

(use-package ensime
  :ensure t
  :config
  (setq ensime-startup-snapshot-notification nil)
  
  (setq ensime-completion-style 'company
        ensime-graphical-tooltips t
        ensime-auto-generate-config t)

  (add-hook 'scala-mode-hook 'ensime-mode)
  
  ;; auto start ENSIME
  (defun my-ensime-auto-start ()
    (interactive)
    (unless (get-buffer-process "*ENSIME*")
      (ensime)))
  
  ;; (add-hook 'ensime-mode-hook #'my-ensime-auto-start)
  )


(provide 'init-prog-lang-scala)

;;; init-prog-lang-scala.el ends here
