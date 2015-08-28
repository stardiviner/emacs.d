;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ jdee ] -- Java Development Environment for Emacs

(require 'jdee)

;; (autoload 'jde-mode "jde" "JDE mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


;;; [ jde-maven ] -- 

;; (require 'jde-maven)


;;; [ eclim ]

;; (require 'eclim)
(autoload 'eclim "eclim" nil t)

;; ;; If you want to control eclimd from emacs, also add:
;; (require 'eclimd)


;; (setq eclim-eclipse-dirs '("/Applications/eclipse"
;;                            "/usr/lib/eclipse"
;;                            "/usr/local/lib/eclipse"
;;                            "/usr/share/eclipse")
;;       ;; eclim-executable "~/Libraries/Emacs/eclim"
;;       )

;; for company-mode
(require 'company-emacs-eclim)

;; (company-emacs-eclim-setup)
(add-hook 'java-mode-hook
          (lambda ()
            (eclim-mode)
            
            ;; (setq-local company-backends
            ;;             (cons 'company-emacs-eclim
            ;;                   (remove-if (lambda (b) (find b '(company-nxml company-eclim)))
            ;;                              company-backends)))

            (add-to-list (make-local-variable 'company-backends)
                         'company-emacs-eclim)
            ))

;; (global-eclim-mode t)


;;; [ java-complete ]


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



(provide 'init-my-prog-lang-java)

;;; init-my-prog-lang-java.el.el ends here
