;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))

;;; [ javap-mode ] -- show the ouput of javap when opening a jvm class file in Emacs.

(use-package javap-mode
  :ensure t)

;;; [ autodisass-java-bytecode ] -- Automatically disassemble emacs buffers containing Java bytecode.

(use-package autodisass-java-bytecode
  :ensure t)

;;; [ Eclim ]

(use-package eclim
  :ensure t
  :defer t
  :init
  ;; Control `eclimd' from emacs
  (require 'eclimd)

  (setq eclimd-autostart nil
        eclimd-autostart-with-default-workspace t
        eclimd-default-workspace "~/Code/Java"
        eclimd-wait-for-process nil)
  
  ;; auto enable `eclim-mode' in `java-mode'.
  (add-hook 'java-mode-hook 'eclim-mode)
  ;; (global-eclim-mode t)
  :config
  ;; Eclim Java
  ;; (setq eclim-java-documentation-root nil
  ;;       eclim-java-android-documentation-root
  ;;       )
  )


;;; for company-mode
(use-package company-emacs-eclim
  :ensure t
  :config
  ;; (company-emacs-eclim-setup)
  (defun my-company-eclim-setup ()
    (my-company-add-backend-locally 'company-emacs-eclim))

  (add-hook 'eclim-mode-hook 'my-company-eclim-setup)
  )

;;; [ meghanada ] -- A New Java Develop Environment for Emacs.

;; (use-package meghanada
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'java-mode-hook #'meghanada-mode)
;;   :config
;;   (setq meghanada-server-install-dir (locate-user-emacs-file
;;                                       "init/extra/meghanada/")
;;         meghanada-auto-start t
;;         meghanada-debug t
;;         meghanada-use-company nil
;;         meghanada-use-flycheck t)
;;
;;   (defun my-meghanada-settings ()
;;     (interactive)
;;     ;; (if (not (meghanada-alive-p))
;;     ;;     (warn "meghanada not started."))
;;
;;     (my-company-add-backend-locally 'company-meghanada)
;;     )
;;
;;   (add-hook 'java-mode-hook 'my-meghanada-settings)
;;   )

;;; [ malabar-mode ] -- JVM Integration for Java and other JVM based languages.

;; (use-package malabar-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (message "activate-malabar-mode")
;;               (activate-malabar-mode)))
;;   :config
;;   (add-hook 'malabar-java-mode-hook 'flycheck-mode)
;;   (add-hook 'malabar-groovy-mode-hook 'flycheck-mode)
;;   )

;;; [ java-imports ] -- dealing with Java imports.

(use-package java-imports
  :ensure t
  :defer t
  :bind (:map java-mode-map
              ("C-c M-i" . java-imports-add-import-dwim))
  :config
  (add-hook 'java-mode-hook 'java-imports-scan-file)
  )


;;; [ gradle-mode ] -- Gradle integration with Emacs' compile.

(use-package gradle-mode
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook #'gradle-mode)
  )

;;; [ sdkman ] -- Software Development Kit

;; http://sdkman.io/

;;; add Gradle & Groovy bins to TRAMP path.
(add-to-list 'tramp-remote-path
             (concat (getenv "HOME")
                     "/.sdkman/candidates/gradle/current/bin"))
(add-to-list 'tramp-remote-path
             (concat (getenv "HOME")
                     "/.sdkman/candidates/groovy/current/bin"))

;;; [ jdecomp ] -- Emacs interface to Java decompilers.

(use-package jdecomp
  :ensure t
  :config
  ;; (setq jdecomp-decompiler-paths '((cfr . "~/.java/cfr.jar")
  ;;                                  (fernflower . "")))
  ;; (setq jdecomp-decompiler-type )
  (jdecomp-mode 1)
  )


(provide 'init-my-prog-lang-java)

;;; init-my-prog-lang-java.el.el ends here
