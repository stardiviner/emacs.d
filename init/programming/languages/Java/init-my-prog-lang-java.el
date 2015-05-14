;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ javadoc-lookup ]

;;; Usage:
;; - [M-x javadoc-lookup] -- [C-h d]
;;
;;; Import functions
;;
;; Two functions for managing Java imports is provided: `add-java-import' and
;; `sort-java-imports'. The former integrates with the javadoc-lookup index to
;; provide completions.

(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h d") 'my-prog-help-document-map)
            (define-key my-prog-help-document-map (kbd "d") 'javadoc-lookup)
            ))

(javadoc-add-roots "/usr/share/doc/openjdk-7-doc/api"
                   ;; "~/src/project/doc"
                   )


;;; [ javadoc-help ]


;;; [ java-complete ]



;;; [ eclim ]

;; Eclim is an Eclipse plugin which exposes Eclipse features through a server
;; interface. When this server is started, the command line utility eclim can be
;; used to issue requests to that server.
;;
;; Emacs-eclim uses the eclim server to integrate eclipse with emacs. This
;; project wants to bring some of the invaluable features from eclipse to
;; emacs. Please note, emacs-eclim is limited to mostly java support at this
;; time.
;;
;; It is also possible to start and stop the eclim daemon from emacs using the
;; eclimd package.

;; (require 'eclim)
;; ;; If you want to control eclimd from emacs, also add:
;; (require 'eclimd)
(autoload 'eclim "eclim" nil t)

;; (setq eclim-eclipse-dirs '("/Applications/eclipse"
;;                            "/usr/lib/eclipse"
;;                            "/usr/local/lib/eclipse"
;;                            "/usr/share/eclipse")
;;       ;; eclim-executable "~/Libraries/Emacs/eclim"
;;       )

;; for auto-complete, add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

;; for company-mode
;; (require 'company-emacs-eclim)

;; (company-emacs-eclim-setup)
(add-hook 'java-mode-hook
          (lambda ()
            (eclim-mode)
            (setq-local company-backends
                        (cons 'company-emacs-eclim
                              (remove-if (lambda (b) (find b '(company-nxml company-eclim)))
                                         company-backends)))
            (add-to-list (make-local-variable 'company-backends)
                         'company-emacs-eclim)))


;; (global-eclim-mode t)


;;; [ company-eclim ]

(require 'company-emacs-eclim)

(add-hook 'java-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-eclim)))


(provide 'init-my-prog-lang-java)

;;; init-my-prog-lang-java.el.el ends here
