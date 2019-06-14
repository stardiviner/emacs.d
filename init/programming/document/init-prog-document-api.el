;;; init-prog-document-api.el --- init for API
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dash-docs ] -- Offline documentation browser using Dash docsets.

(use-package dash-docs
  :ensure t
  :defer t
  :init (setq dash-docs-docsets-path (expand-file-name "~/.docsets")
              dash-docs-min-length 3
              ;; 'eww-browse-url, 'browse-url, 'browse-url-generic, 'helm-browse-url
              dash-docs-browser-func 'browse-url-generic
              dash-docs-candidate-format "%d  %n  (%t)"
              dash-docs-enable-debugging nil)
  ;; (setq dash-docs-common-docsets ; it will DUPLICATE with major mode docsets.
  ;;       '("Clojure" "Java"
  ;;         ;; "Common Lisp"
  ;;         ;; "Python 3"
  ;;         "HTML" "CSS"
  ;;         "JavaScript" "NodeJS"))
  )

;;; [ helm-dash ] -- Offline documentation browser for +150 APIs using Dash docsets.

(use-package helm-dash
  :ensure t
  :bind (:map document-prefix ("d" . helm-dash-at-point) ("M-d" . helm-dash))
  :init (setq helm-case-fold-search 'smart)

  ;; buffer local docsets
  (defun my-helm-dash-buffer-local-docsets-add (docsets-list)
    (mapc
     (lambda (docset)
       (setq-local dash-docs-docsets (add-to-list 'dash-docs-docsets docset)))
     docsets-list))
  
  ;; Bash
  (defun helm-dash-buffer-local-shell-docsets ()
    (setq-local dash-docs-docsets '("Bash")))
  (add-hook 'sh-mode-hook 'helm-dash-buffer-local-shell-docsets)
  ;; Ruby
  (defun helm-dash-buffer-local-ruby-docsets ()
    (setq-local dash-docs-docsets '("Ruby")))
  (add-hook 'enh-ruby-mode-hook 'helm-dash-buffer-local-ruby-docsets)
  ;; Ruby on Rails
  (defun helm-dash-buffer-local-rails-docsets ()
    (setq-local dash-docs-docsets '("Ruby on Rails" "Ruby"))
    (my-helm-dash-buffer-local-docsets-add '("HTML" "JavaScript" "CSS" "jQuery")))
  (add-hook 'projectile-rails-mode-hook 'helm-dash-buffer-local-rails-docsets)
  ;; Python
  (defun helm-dash-buffer-local-python-docsets ()
    (setq-local dash-docs-docsets '("Python 3")))
  (add-hook 'python-mode-hook 'helm-dash-buffer-local-python-docsets)
  ;; Web
  (with-eval-after-load 'web-mode
    (defun helm-dash-buffer-local-web-docsets ()
      (setq-local dash-docs-docsets '("HTML" "CSS" "JavaScript"))
      (my-helm-dash-buffer-local-docsets-add '("jQuery" "React")))
    (add-hook 'web-mode-hook 'helm-dash-buffer-local-web-docsets))
  ;; JavaScript
  (with-eval-after-load 'js2-mode
    (defun helm-dash-buffer-local-javascript-docsets ()
      (setq-local dash-docs-docsets '("JavaScript" "NodeJS"))
      (my-helm-dash-buffer-local-docsets-add '("jQuery" "React")))
    (add-hook 'js2-mode-hook 'helm-dash-buffer-local-javascript-docsets))
  ;; HTML
  (defun helm-dash-buffer-local-html-docsets ()
    (setq-local dash-docs-docsets '("HTML"))
    (my-helm-dash-buffer-local-docsets-add '("JavaScript" "jQuery" "React" "D3JS")))
  (add-hook 'html-mode-hook 'helm-dash-buffer-local-html-docsets)
  ;; CSS
  (defun helm-dash-buffer-local-css-docsets ()
    (setq-local dash-docs-docsets '("CSS"))
    (my-helm-dash-buffer-local-docsets-add '("Bootstrap_4")))
  (add-hook 'css-mode-hook 'helm-dash-buffer-local-css-docsets)
  ;; Common Lisp
  (defun helm-dash-buffer-local-common-lisp-docsets ()
    (setq-local dash-docs-docsets '("Common Lisp")))
  (add-hook 'common-lisp-mode-hook 'helm-dash-buffer-local-common-lisp-docsets)
  ;; Clojure
  (defun helm-dash-buffer-local-clojure-docsets ()
    (setq-local dash-docs-docsets '("Clojure" "Java"))
    (my-helm-dash-buffer-local-docsets-add '("ClojureDocs")))
  (add-hook 'clojure-mode-hook 'helm-dash-buffer-local-clojure-docsets)
  ;; ClojureScript
  (defun helm-dash-buffer-local-clojurescript-docsets ()
    (setq-local dash-docs-docsets '("Clojure" "ClojureScript" "JavaScript"))
    (my-helm-dash-buffer-local-docsets-add '("ClojureDocs")))
  (add-hook 'clojurescript-mode-hook 'helm-dash-buffer-local-clojurescript-docsets)
  ;; CIDER REPL
  (defun helm-dash-buffer-local-cider-docsets ()
    (if (equal cider-repl-type "clj")
        (progn
          (setq-local dash-docs-docsets '("Clojure"))
          (my-helm-dash-buffer-local-docsets-add '("ClojureDocs")))
      (setq-local dash-docs-docsets '("Clojure" "ClojureScript"))
      (my-helm-dash-buffer-local-docsets-add '("ClojureDocs"))))
  (add-hook 'cider-repl-mode-hook 'helm-dash-buffer-local-cider-docsets)
  ;; C
  (defun helm-dash-buffer-local-C-docsets ()
    (setq-local dash-docs-docsets '("C"))
    (my-helm-dash-buffer-local-docsets-add '("GNU Make" "CMake" "LLVM" "Clang" "GLib")))
  (add-hook 'c-mode-hook 'helm-dash-buffer-local-C-docsets)
  ;; C++
  (defun helm-dash-buffer-local-C++-docsets ()
    (setq-local dash-docs-docsets '("C++")))
  (add-hook 'c++-mode-hook 'helm-dash-buffer-local-C++-docsets)
  ;; Go
  (defun helm-dash-buffer-local-go-docsets ()
    (setq-local dash-docs-docsets '("Go")))
  (add-hook 'go-mode-hook 'helm-dash-buffer-local-go-docsets)
  ;; Java
  (defun helm-dash-buffer-local-java-docsets ()
    (setq-local dash-docs-docsets '("Java")))
  (add-hook 'java-mode-hook 'helm-dash-buffer-local-java-docsets)
  ;; SQL
  (defun helm-dash-buffer-local-sql-docsets ()
    (setq-local dash-docs-docsets '("SQLite" "MySQL" "PostgreSQL")))
  (add-hook 'sql-mode-hook 'helm-dash-buffer-local-sql-docsets)
  (add-hook 'sql-interactive-mode-hook 'helm-dash-buffer-local-sql-docsets)
  ;; Redis
  (defun helm-dash-buffer-local-redis-docsets ()
    (setq-local dash-docs-docsets '("Redis")))
  (add-hook 'redis-mode-hook 'helm-dash-buffer-local-redis-docsets)
  ;; LaTeX
  (defun helm-dash-buffer-local-latex-docsets ()
    (setq-local dash-docs-docsets '("LaTeX")))
  (add-hook 'latex-mode-hook 'helm-dash-buffer-local-latex-docsets)
  (add-hook 'LaTeX-mode-hook 'helm-dash-buffer-local-latex-docsets)
  ;; PlantUML
  (defun helm-dash-buffer-local-PlantUML-docsets ()
    (setq-local dash-docs-docsets '("PlantUML")))
  (add-hook 'plantuml-mode-hook 'helm-dash-buffer-local-PlantUML-docsets)
  ;; R
  (defun helm-dash-buffer-local-R-docsets ()
    (setq-local dash-docs-docsets '("R")))
  (add-hook 'ess-r-mode-hook 'helm-dash-buffer-local-R-docsets)
  ;; Julia
  (defun helm-dash-buffer-local-julia-docsets ()
    (setq-local dash-docs-docsets '("Julia")))
  (add-hook 'ess-julia-mode-hook 'helm-dash-buffer-local-julia-docsets)
  (add-hook 'julia-mode-hook 'helm-dash-buffer-local-julia-docsets)
  ;; Docker
  (with-eval-after-load 'dockerfile-mode
    (defun helm-dash-buffer-local-docker-docsets ()
      (setq-local dash-docs-docsets '("Docker")))
    (add-hook 'dockerfile-mode-hook 'helm-dash-buffer-local-docker-docsets))
  ;; Kubernetes
  (with-eval-after-load 'k8s-mode
    (defun helm-dash-buffer-local-kubernetes-docsets ()
      (setq-local dash-docs-docsets '("kubernetes")))
    (add-hook 'k8s-mode-hook 'helm-dash-buffer-local-kubernetes-docsets))
  ;; Vagrant
  ;; GraphQL
  (with-eval-after-load 'graphql-mode
    (defun helm-dash-buffer-local-GraphQL-docsets ()
      (setq-local dash-docs-docsets '("GraphQL Specification")))
    (add-hook 'graphql-mode-hook 'helm-dash-buffer-local-GraphQL-docsets))
  ;; Swift
  (with-eval-after-load 'swift-mode
    (defun helm-dash-buffer-local-swift-docsets ()
      (setq-local dash-docs-docsets '("Swift"))
      (my-helm-dash-buffer-local-docsets-add '("iOS" "OS_X")))
    (add-hook 'swift-mode-hook 'helm-dash-buffer-local-swift-docsets))
  ;; Android
  (with-eval-after-load 'android-mode
    (defun helm-dash-buffer-local-android-docsets ()
      (setq-local dash-docs-docsets '("Android"))
      (my-helm-dash-buffer-local-docsets-add '("Java")))
    (add-hook 'android-mode-hook 'helm-dash-buffer-local-android-docsets))
  ;; Arduino
  (with-eval-after-load 'arduino-mode
    (defun helm-dash-buffer-local-arduino-docsets ()
      (setq-local dash-docs-docsets '("Arduino")))
    (add-hook 'arduino-mode-hook 'helm-dash-buffer-local-arduino-docsets))
  ;; Linux
  (with-eval-after-load 'nginx-mode
    (defun helm-dash-buffer-local-nginx-docsets ()
      (setq-local dash-docs-docsets '("Nginx")))
    (add-hook 'nginx-mode-hook 'helm-dash-buffer-local-nginx-docsets))
  (with-eval-after-load 'apache-mode
    (defun helm-dash-buffer-local-nginx-docsets ()
      (setq-local dash-docs-docsets '("Apache_HTTP_Server")))
    (add-hook 'apache-mode-hook 'helm-dash-buffer-local-apache-docsets))
  )


;;; [ zeal-at-point ]

;; (use-package zeal-at-point
;;   :ensure t
;;   :ensure-system-package zeal
;;   :defer t
;;   :bind (:map document-prefix
;;               ("C-d" . zeal-at-point))
;;   :init
;;   (setq zeal-at-point-zeal-version "0.3.0")
;;   :config
;;   ;; multiple docsets search
;;   (add-to-list 'zeal-at-point-mode-alist
;;                '(clojurescript-mode . ("clojure" "clojurescript")))
;;   (add-to-list 'zeal-at-point-mode-alist
;;                '(enh-ruby-mode . ("ruby" "rails")))
;;   (add-to-list 'zeal-at-point-mode-alist
;;                '(python-mode . ("python" "django")))
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (setq-local zeal-at-point-docset '("javascript" "html" "css"))))
;;   (add-hook 'projectile-rails-mode-hook
;;             (lambda ()
;;               (setq zeal-at-point-docset '("rails" "javascript" "html" "css"))))
;;   )


(provide 'init-prog-document-api)

;;; init-prog-document-api.el ends here
