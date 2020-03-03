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
               '(:results . "output"))
  ;; (add-to-list 'org-babel-default-header-args:java
  ;;              '(:cmdline . "-cp ."))
  )

;;; [ lsp-java ]

(use-package lsp-java
  :ensure t
  :after lsp
  :hook ((java-mode . lsp)
         (java-mode . lsp-ui-doc-mode))
  :commands (lsp-java-update-server)
  ;; `aggressive-indent-mode' caused long suspend with lots of requests.
  :init (with-eval-after-load 'aggressive-indent
          (add-to-list 'aggressive-indent-excluded-modes 'java-mode))
  )

;;; [ lsp-javacomp ] -- Emacs Language Server client backed by JavaComp.

;; (use-package lsp-javacomp
;;   :ensure t
;;   :after lsp
;;   :config
;;   (lsp-javacomp-install-server)
;;   (add-hook 'java-mode-hook #'lsp-javacomp-enable))

;;; [ lsp-intellij ] -- Emacs client for lsp-intellij-server.

;; (use-package lsp-intellij
;;   :ensure t
;;   :after lsp
;;   :init (add-hook 'java-mode-hook #'lsp-intellij-enable))

;;; [ JDEE ] -- Java Development Environment for Emacs.

;; (use-package jdee
;;   :ensure t
;;   :defer t
;;   ;; :mode ("\\.java\\'" . jdee-mode)
;;   :commands (jdee-mode)
;;   :init (setq jdee-server-dir "~/Code/Emacs/jdee-server/target/"))

;;; [ java-imports ] -- dealing with Java imports.

;; (use-package java-imports
;;   :ensure t
;;   :defer t
;;   :bind (:map java-mode-map
;;               ("C-c M-i" . java-imports-add-import-dwim))
;;   :config
;;   (add-hook 'java-mode-hook 'java-imports-scan-file)
;;   )


;;; [ gradle-mode ] -- Gradle integration with Emacs' compile.

;; (use-package gradle-mode
;;   :ensure t
;;   :config
;;   (add-hook 'java-mode-hook #'gradle-mode)
;;   )

;;; [ sdkman ] -- Software Development Kit

;; http://sdkman.io/

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

;;; [ jdecomp ] -- Emacs interface to Java decompilers.

;; (use-package jdecomp
;;   :ensure t
;;   :commands (jdecomp)
;;   :config
;;   ;; (setq jdecomp-decompiler-paths '((cfr . "~/.java/cfr.jar")
;;   ;;                                  (fernflower . "")))
;;   ;; (setq jdecomp-decompiler-type )
;;   (jdecomp-mode 1)
;;   )


(use-package dap-java
  :after 'lsp-java)

;;; [ thread-dump ] -- Emacs mode for java thread dumps.

;; (use-package thread-dump
;;   :ensure t
;;   :config
;;   ;; config for Dired.
;;   ;; support open Dired directory.
;;   (defun thread-dump-open-dired-dir ()
;;     (interactive)
;;     (thread-dump-open-dir (dired-current-directory)))
;;   ;; support open Dired marked files.
;;   (defun thread-dump-open-marked-files ()
;;     (interactive)
;;     (let ((files (dired-get-marked-files)))
;;       (thread-dump-open-files files)))
;;   ;; add keybindings for Dired mode.
;;   (add-hook 'dired-mode-hook
;;             (lambda ()
;;               (define-key dired-mode-map (kbd "C-c t d") 'thread-dump-open-dired-dir)
;;               (define-key dired-mode-map (kbd "C-c t f") 'thread-dump-open-marked-files)))
;;   )

;;; [ helm-jstack ] -- Helm interface to jps & jstack for JVM.

;; (use-package helm-jstack
;;   :ensure t)


(provide 'init-prog-lang-java)

;;; init-prog-lang-java.el.el ends here
