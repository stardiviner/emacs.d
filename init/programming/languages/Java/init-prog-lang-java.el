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
  )

(add-hook 'java-mode-hook #'my/java-mode-setup)

;;; [ ob-java ]

(require 'ob-java)

(add-to-list 'org-babel-load-languages '(java . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("java" . "java"))

(defvar org-babel-default-header-args:java nil)
(add-to-list 'org-babel-default-header-args:java
             '(:results . "output"))
;; (add-to-list 'org-babel-default-header-args:java
;;              '(:cmdline . "-cp ."))

;;; [ lsp-java ]

(use-package lsp-java
  :ensure t
  :init (add-hook 'java-mode-hook #'lsp-java-enable)
  :config
  ;; set the projects that are going to be imported into the workspace.
  (setq lsp-java--workspace-folders (list "~/Documents/learning/Java/test-project/"))
  (setq lsp-java-progress-report nil
        lsp-java-trace-server nil)
  (setq lsp-response-timeout 20)
  )

(defun org-babel-edit-prep:java (babel-info)
  "Prepare buffer local environment for Org source block Java."
  (if-let* ((lang (car babel-info))
            (ext (cdr (assoc lang org-babel-tangle-lang-exts)))
            ;; detect the header argument :lsp-file exist, if not, use default
            ;; "/tmp/tmp.EXT".
            (lsp-file (or (->> babel-info
                               caddr
                               (alist-get :lsp-file))
                          (format "/tmp/tmp.%s" ext)))
            (lsp-file-url (lsp--path-to-uri lsp-file)))
      (progn
        (setq-local buffer-file-name lsp-file)
        (setq-local lsp-buffer-uri lsp-file-url)))
  ;; detect lsp-mode language enable function exist?
  ;; (if (boundp ))
  ;; (lsp-java-enable)
  (message "lsp-mode workspace file setup for source block done!")
  )

;;; [ lsp-javacomp ] -- Emacs Language Server client backed by JavaComp.

;; (use-package lsp-javacomp
;;   :ensure t
;;   :config
;;   (lsp-javacomp-install-server)
;;   (add-hook 'java-mode-hook #'lsp-javacomp-enable))

;;; [ lsp-intellij ] -- Emacs client for lsp-intellij-server.

;; (use-package lsp-intellij
;;   :ensure t
;;   :init (add-hook 'java-mode-hook #'lsp-intellij-enable))

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
  :ensure t)

;;; [ jdecomp ] -- Emacs interface to Java decompilers.

;; (use-package jdecomp
;;   :ensure t
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
