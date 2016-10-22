;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Eclim ]

(use-package eclim
  :ensure t
  :defer t
  :init
  ;; Control `eclimd' from emacs
  (require 'eclimd)
  
  ;; auto enable `eclim-mode' in `java-mode'.
  (add-hook 'java-mode-hook 'eclim-mode)
  ;; (global-eclim-mode t)

  ;; auto start eclimd
  (defun eclimd-auto-start ()
    "Auto start eclimd in a selected directory."
    (interactive)
    (unless (or (get-buffer "*eclimd*")
                (process-live-p (get-process "eclimd")))
      (start-eclimd (or eclimd-default-workspace
                        (read-file-name "Eclim workspace directory: ")))
      ))

  ;; (add-hook 'java-mode-hook 'eclimd-auto-start)
  
  :config
  (setq eclimd-default-workspace "~/Projects/Eclipse"
        ;; eclim-executable
        ;; eclimd-executable "~/.eclipse/org.eclipse.platform_4.5.1_155965261_linux_gtk_x86_64/eclimd"
        ;; eclimd-port 45620
        eclimd-wait-for-process t
        )
  )


;;; for company-mode
(use-package company-emacs-eclim
  :ensure t
  :defer t
  :init
  ;; (company-emacs-eclim-setup)
  (defun my-company-eclim-setup ()
    (my-company-add-backend-locally 'company-emacs-eclim))

  (add-hook 'eclim-mode-hook 'my-company-eclim-setup)
  )

;;; for auto-complete
;; (use-package ac-emacs-eclim
;;   :ensure t
;;   )


;;; [ meghanada ] -- A New Java Develop Environment for Emacs.

;; (use-package meghanada
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'java-mode-hook #'meghanada-mode)
;;
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
;;
;;   :config
;;   (add-hook 'malabar-java-mode-hook 'flycheck-mode)
;;   (add-hook 'malabar-groovy-mode-hook 'flycheck-mode)
;;   )


;;; [ gradle-mode ] -- Gradle integration with Emacs' compile.

(use-package gradle-mode
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook #'gradle-mode)
  )


(provide 'init-my-prog-lang-java)

;;; init-my-prog-lang-java.el.el ends here
