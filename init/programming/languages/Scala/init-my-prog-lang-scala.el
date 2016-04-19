;;; init-my-prog-lang-scala.el --- init for Scala
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ scala-mode ]

;; (use-package scala-mode
;;   :ensure t
;;   )


;;; [ scala-mode2 ]

(use-package scala-mode2
  :ensure t)


;;; [ sbt-mode ]

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  (define-key scala-mode-map (kbd "C-c C-s") 'run-scala)
  )


;;; [ Ensime ]

(use-package ensime
  :ensure t
  :config
  (setq ensime-completion-style 'company
        ensime-graphical-tooltips t
        ensime-auto-generate-config t)

  (add-hook 'scala-mode-hook 'ensime-mode)
  )


;;; [ ob-scala ]

(use-package ob-scala
  :ensure t)


(provide 'init-my-prog-lang-scala)

;;; init-my-prog-lang-scala.el ends here
