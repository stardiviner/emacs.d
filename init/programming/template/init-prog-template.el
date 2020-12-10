;;; init-prog-template.el --- init for Template
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ tempo ]

;; (require 'tempo)

;; (setq tempo-interactive t               ; `tempo-insert' prompt.
;;       tempo-insert-region t)


;;; [ tempo-snippets ]


;;; [ skeleton ] -- Lisp language extension for writing statement skeletons.

;; (require 'skeleton)

;;; [ auto-insert ]

(use-package autoinsert
  :defer t
  :init
  ;; (setq auto-insert-query 'function)
  (setq auto-insert-directory (locate-user-emacs-file "templates/"))
  
  ;; templates: `auto-insert-alist'
  (setq auto-insert-alist nil)
  ;; (define-auto-insert '("\\.html?$") "default.html")
  ;; (define-auto-insert '("^build\\.xml\\'" . "Java Ant compile file") "build.xml")

  :config
  ;; combining YASnippet & Auto Insert
  (defun autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (define-auto-insert "\\.el$" ["default-elisp.el" autoinsert-yas-expand])
  (define-auto-insert "\\.lisp$" ["default-lisp.lisp" autoinsert-yas-expand])
  (define-auto-insert "\\.clj$" ["default-clojure.clj" autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["default-python.py" autoinsert-yas-expand])
  (define-auto-insert "\\.rb$" ["default-ruby.rb" autoinsert-yas-expand])
  (define-auto-insert "\\.c$" ["default-C.c" autoinsert-yas-expand])
  (define-auto-insert "\\.cpp$" ["default-C++.cpp" autoinsert-yas-expand])
  (define-auto-insert "\\.java$" ["default-Java.java" autoinsert-yas-expand])

  (auto-insert-mode 1))


;;; [ skeleton ] -- Lisp language extension for writing statement skeletons.

;; (require 'skeleton)

;;; [ yatemplate ] -- with YASnippet + auto-insert-mode.

;; (use-package yatemplate
;;   :ensure t
;;   :defer t
;;   :custom (yatemplate-dir (locate-user-emacs-file "templates")))

;;; [ time-stamp ] -- auto-update last change time stamps in files edited by Emacs.

(use-package time-stamp
  :defer t
  :init (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t ; enable `time-stamp'
        time-stamp-line-limit 20))


(provide 'init-prog-template)

;;; init-prog-template.el ends here
