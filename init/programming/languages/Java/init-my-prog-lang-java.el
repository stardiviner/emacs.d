;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Eclim ]

(use-package eclim
  :ensure t
  :init
  ;; Control `eclimd' from emacs
  (require 'eclimd)
  :config
  (setq eclimd-default-workspace "~/Projects/Eclipse"
        ;; eclim-executable
        ;; eclimd-executable "~/.eclipse/org.eclipse.platform_4.5.1_155965261_linux_gtk_x86_64/eclimd"
        ;; eclimd-port 45620
        eclimd-wait-for-process t
        )

  ;; auto enable `eclim-mode' in `java-mode'.
  (add-hook 'java-mode-hook 'eclim-mode)
  ;; (global-eclim-mode t)


  ;; auto start eclimd
  (defun eclimd-auto-start ()
    (interactive)
    (unless (and (get-buffer "*eclimd*")
                 (process-live-p (get-process "eclimd")))
      (if (yes-or-no-p "start eclimd? ")
          ;; (start-eclimd (file-name-directory (buffer-file-name)))
          (call-interactively 'start-eclimd)))
    )

  (add-hook 'java-mode-hook 'eclimd-auto-start)
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

;;; for auto-complete
;; (use-package ac-emacs-eclim
;;   :ensure t)


;;; [ malabar-mode ] -- JVM Integration for Java and other JVM based languages.

;; (use-package malabar-mode
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (message "activate-malabar-mode")
;;               (activate-malabar-mode)))
;;
;;   (add-hook 'malabar-java-mode-hook 'flycheck-mode)
;;   (add-hook 'malabar-groovy-mode-hook 'flycheck-mode)
;;   )


;;; [ gradle-mode ]


(provide 'init-my-prog-lang-java)

;;; init-my-prog-lang-java.el.el ends here
