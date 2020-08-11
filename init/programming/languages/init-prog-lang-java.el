;;; init-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))

(defun my/java-mode-setup ()
  "My setup for `java-mode'."
  ;; set tab with to 4.
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local standard-indent 4)
  (setq-local completion-ignore-case t))
(add-hook 'java-mode-hook #'my/java-mode-setup)

(add-hook 'java-mode-hook #'electric-pair-local-mode)

(defun java-mode-electric-layout-setting ()
  "auto insert newline after specific characters."
  (setq-local electric-layout-rules '((?\; . after)))
  (add-to-list 'electric-layout-rules '( ?\{ .  after))
  (add-to-list 'electric-layout-rules '( ?\} .  before)))
(add-hook 'java-mode-hook #'electric-layout-local-mode)
(add-hook 'java-mode-hook #'java-mode-electric-layout-setting)

;;; [ ob-java ]

(use-package ob-java
  :defer t
  :commands (org-babel-execute:java)
  :config
  (add-to-list 'org-babel-load-languages '(java . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("java" . "java"))

  (defvar org-babel-default-header-args:java nil)
  (add-to-list 'org-babel-default-header-args:java
               '(:results . "output")))

;;; [ lsp-java ]

(use-package lsp-java
  :ensure t
  :hook ((java-mode . lsp)
         (java-mode . dap-mode)
         (java-mode . dap-ui-mode))
  ;; `aggressive-indent-mode' caused long suspend with lots of requests.
  :init (with-eval-after-load 'aggressive-indent
          (add-to-list 'aggressive-indent-excluded-modes 'java-mode))
  :config (require 'dap-java))

;;; [ gradle-mode ] -- Gradle integration with Emacs' `compile'.

(use-package gradle-mode
  :ensure t
  :delight gradle-mode)

;;; [ sdkman ] -- Software Development Kit

;;; add Gradle & Groovy bins to TRAMP path.
;; (add-to-list 'tramp-remote-path
;;              (concat (getenv "HOME")
;;                      "/.sdkman/candidates/gradle/current/bin"))
;; (add-to-list 'tramp-remote-path
;;              (concat (getenv "HOME")
;;                      "/.sdkman/candidates/groovy/current/bin"))

;;; [ javap-mode ] -- show the ouput of javap when opening a jvm class file in Emacs.

(use-package javap-mode
  :ensure t
  :commands (javap-buffer javap-mode))


(provide 'init-prog-lang-java)

;;; init-prog-lang-java.el.el ends here
