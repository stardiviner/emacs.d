;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Eclim ]

(use-package emacs-eclim
  :ensure t
  :config
  ;; Control `eclimd' from emacs
  (require 'eclimd)

  (setq eclimd-default-workspace "~/Projects/Eclipse"
        ;; eclim-executable
        eclimd-executable "~/.eclipse/org.eclipse.platform_4.5.1_155965261_linux_gtk_x86_64/eclimd"
        ;; eclimd-port 45620
        eclimd-wait-for-process t)

  ;; for company-mode
  (require 'company-emacs-eclim)

  ;; (company-emacs-eclim-setup)
  (add-hook 'java-mode-hook
            (lambda ()
              (eclim-mode 1)
              (my-company-add-backend-locally 'company-emacs-eclim)
              (local-set-key (kbd "C-M-i") 'company-complete)
              ))

  ;; (global-eclim-mode t)
  )


;;; [ javadoc-help ]



;;; [ malabar-mode ] -- EMCS JVM Integration for Java and other JVM based languages.

;; (require 'malabar-mode)


;;; [ groovy-mode ] -- Groovy mode derived mode.

;; (require 'groovy-mode)


;;; [ inf-groovy ] -- minor-mode that adds some Grails project management to a grails project.

;; (require 'inf-groovy)


;;; [ gradle-mode ]




(provide 'init-my-prog-lang-java)

;;; init-my-prog-lang-java.el.el ends here
