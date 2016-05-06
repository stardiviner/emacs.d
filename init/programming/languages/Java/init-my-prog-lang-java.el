;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ jdee ] -- Java Development Environment for Emacs

;; The JDEE is an add-on software package that turns Emacs into a comprehensive
;; system for creating, editing, debugging, and documenting Java applications.

;; (require 'jdee)
;;
;; (autoload 'jde-mode "jde" "JDE mode" t)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . jde-mode))
;;
;; (setq jdee-server-dir "~/compile/Emacs/jdee-server/target/")
;;
;; (setq jdee-key-bindings '())


;;; [ jde-maven ] -- 

;; (require 'jde-maven)


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


;;; [ javadoc-lookup ]

;;; Usage:
;; - [M-x javadoc-lookup] -- [C-h d]
;;
;;; Import functions
;;
;; Two functions for managing Java imports is provided: `add-java-import' and
;; `sort-java-imports'. The former integrates with the javadoc-lookup index to
;; provide completions.

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-h d") 'my-prog-help-document-map)
;;             (define-key my-prog-help-document-map (kbd "d") 'javadoc-lookup)
;;             ))
;;
;; (javadoc-add-roots "/usr/share/doc/openjdk-7-doc/api"
;;                    ;; "~/src/project/doc"
;;                    )


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
