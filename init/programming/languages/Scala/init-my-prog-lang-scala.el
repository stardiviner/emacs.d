;;; init-my-prog-lang-scala.el --- init for Scala
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ scala-mode ]

(use-package scala-mode
  :ensure t
  :defer t)


;;; [ sbt-mode ]

(use-package sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :init
  (define-key scala-mode-map (kbd "C-c C-s") 'run-scala)
  )


;;; [ Ensime ]

(use-package ensime
  :ensure t
  :defer t
  :init
  (add-hook 'scala-mode-hook 'ensime-mode)
  
  :config
  (setq ensime-completion-style 'company
        ensime-graphical-tooltips t
        ensime-auto-generate-config t)
  )


(provide 'init-my-prog-lang-scala)

;;; init-my-prog-lang-scala.el ends here
