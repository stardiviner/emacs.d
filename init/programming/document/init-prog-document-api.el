;;; init-prog-document-api.el --- init for API
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dash-docs ] -- Offline documentation browser using Dash docsets.

(use-package dash-docs
  :ensure t
  :defer t
  :custom ((dash-docs-docsets-path (expand-file-name "~/.docsets"))
           (dash-docs-min-length 2)
           ;; 'eww-browse-url, 'browse-url, 'browse-url-generic, 'helm-browse-url
           (dash-docs-browser-func 'eaf-open-browser)
           (dash-docs-candidate-format "%d  %n  (%t)")
           (dash-docs-enable-debugging nil)))

;;; [ helm-dash ] -- Offline documentation browser for +150 APIs using Dash docsets.

(use-package helm-dash
  :ensure t
  :bind (:map document-prefix ("d" . helm-dash-at-point) ("M-d" . helm-dash))
  :custom (dash-docs-common-docsets '("Clojure" "ClojureDocs" "Java"))
  :init
  (defun helm-dash--around (orig-func &rest args)
    "Make `helm-dash' ignore case. Useful for SQL docsets."
    (let ((case-fold-search t)
          (helm-case-fold-search t))
      (apply orig-func args)))
  (advice-add 'helm-dash :around #'helm-dash--around)
  
  ;; buffer local docsets
  (defun my/dash-docs-local-docsets (docsets-list &optional append-to)
    (make-local-variable 'dash-docs-common-docsets)
    (if append-to
        (setq dash-docs-common-docsets
              (append dash-docs-common-docsets docsets-list))
      (setq-local dash-docs-common-docsets nil)
      (setq-local dash-docs-common-docsets docsets-list))
    (mapc 'dash-docs-activate-docset dash-docs-common-docsets))
  
  ;; Bash
  (defun helm-dash-buffer-local-shell-docsets ()
    (my/dash-docs-local-docsets '("Bash")))
  (add-hook 'sh-mode-hook 'helm-dash-buffer-local-shell-docsets)
  ;; Ruby
  (defun helm-dash-buffer-local-ruby-docsets ()
    (my/dash-docs-local-docsets '("Ruby")))
  (add-hook 'enh-ruby-mode-hook 'helm-dash-buffer-local-ruby-docsets)
  ;; Ruby on Rails
  (defun helm-dash-buffer-local-rails-docsets ()
    (my/dash-docs-local-docsets '("Ruby on Rails" "Ruby"))
    (my/dash-docs-local-docsets '("HTML" "JavaScript" "CSS" "jQuery") t))
  (add-hook 'projectile-rails-mode-hook 'helm-dash-buffer-local-rails-docsets)
  ;; Python
  (defun helm-dash-buffer-local-python-docsets ()
    (my/dash-docs-local-docsets '("Python_3" "Python_zh_cn"))
    (my/dash-docs-local-docsets '("Qt_5" "Qt_for_Python" "pyside2") t)
    (my/dash-docs-local-docsets '("TensorFlow 2" "PyTorch") t))
  (add-hook 'python-mode-hook 'helm-dash-buffer-local-python-docsets)
  ;; Web
  (with-eval-after-load 'web-mode
    (defun helm-dash-buffer-local-web-docsets ()
      (my/dash-docs-local-docsets '("HTML" "CSS" "JavaScript"))
      (my/dash-docs-local-docsets '("jQuery" "React") t))
    (add-hook 'web-mode-hook 'helm-dash-buffer-local-web-docsets))
  ;; JavaScript
  (defun helm-dash-buffer-local-javascript-docsets ()
    (my/dash-docs-local-docsets '("JavaScript" "NodeJS" "jQuery"))
    (my/dash-docs-local-docsets '("React" "VueJS" "Angular" "BackboneJS" "Express" "Bootstrap_4" "D3JS") t)
    (my/dash-docs-local-docsets '("electron") t)
    (my/dash-docs-local-docsets '("Chrome_Extension_API") t))
  (add-hook 'js-mode-hook 'helm-dash-buffer-local-javascript-docsets)
  ;; HTTP
  (defun helm-dash-buffer-local-http-docsets ()
    (my/dash-docs-local-docsets '("HTTP")))
  (add-hook 'restclient-mode-hook 'helm-dash-buffer-local-http-docsets)
  ;; HTML
  (defun helm-dash-buffer-local-html-docsets ()
    (my/dash-docs-local-docsets '("HTML" "JavaScript" "jQuery"))
    (my/dash-docs-local-docsets '("React" "D3JS") t))
  (add-hook 'html-mode-hook 'helm-dash-buffer-local-html-docsets)
  ;; CSS
  (defun helm-dash-buffer-local-css-docsets ()
    (my/dash-docs-local-docsets '("CSS"))
    (my/dash-docs-local-docsets '("Bootstrap_4") t))
  (add-hook 'css-mode-hook 'helm-dash-buffer-local-css-docsets)
  ;; Common Lisp
  (defun helm-dash-buffer-local-common-lisp-docsets ()
    (my/dash-docs-local-docsets '("Common Lisp")))
  (add-hook 'common-lisp-mode-hook 'helm-dash-buffer-local-common-lisp-docsets)
  ;; Clojure
  (defun helm-dash-buffer-local-clojure-docsets ()
    (my/dash-docs-local-docsets '("Clojure" "Java"))
    (my/dash-docs-local-docsets '("ClojureDocs") t)
    (my/dash-docs-local-docsets '("RxJava") t))
  (add-hook 'clojure-mode-hook 'helm-dash-buffer-local-clojure-docsets)
  ;; ClojureScript
  (defun helm-dash-buffer-local-clojurescript-docsets ()
    (my/dash-docs-local-docsets '("Clojure" "ClojureScript" "JavaScript"))
    (my/dash-docs-local-docsets '("ClojureDocs") t))
  (add-hook 'clojurescript-mode-hook 'helm-dash-buffer-local-clojurescript-docsets)
  ;; CIDER REPL
  (defun helm-dash-buffer-local-cider-docsets ()
    (if (equal cider-repl-type "clj")
        (progn
          (my/dash-docs-local-docsets '("Clojure" "Java"))
          (my/dash-docs-local-docsets '("ClojureDocs") t))
      (my/dash-docs-local-docsets '("Clojure" "ClojureScript" "JavaScript"))
      (my/dash-docs-local-docsets '("ClojureDocs") t)))
  (add-hook 'cider-repl-mode-hook 'helm-dash-buffer-local-cider-docsets)
  ;; Kotlin
  (defun helm-dash-buffer-local-kotlin-docsets ()
    (my/dash-docs-local-docsets '("Kotlin")))
  (add-hook 'kotlin-mode-hook 'helm-dash-buffer-local-kotlin-docsets)
  ;; C
  (defun helm-dash-buffer-local-C-docsets ()
    (my/dash-docs-local-docsets '("C"))
    (my/dash-docs-local-docsets '("GNU Make" "CMake" "LLVM" "Clang" "GLib") t))
  (add-hook 'c-mode-hook 'helm-dash-buffer-local-C-docsets)
  ;; C++
  (defun helm-dash-buffer-local-C++-docsets ()
    (my/dash-docs-local-docsets '("C++"))
    (my/dash-docs-local-docsets '("LLVM" "Clang" "GLib") t)
    (my/dash-docs-local-docsets '("Qt_5") t))
  (add-hook 'c++-mode-hook 'helm-dash-buffer-local-C++-docsets)
  ;; GNU Make
  (defun helm-dash-buffer-local-make-docsets ()
    (my/dash-docs-local-docsets '("GNU Make")))
  (add-hook 'make-mode-hook 'helm-dash-buffer-local-make-docsets)
  ;; CMake
  (defun helm-dash-buffer-local-cmake-docsets ()
    (my/dash-docs-local-docsets '("CMake")))
  (add-hook 'cmake-mode-hook 'helm-dash-buffer-local-cmake-docsets)
  ;; Go
  (defun helm-dash-buffer-local-go-docsets ()
    (my/dash-docs-local-docsets '("Go")))
  (add-hook 'go-mode-hook 'helm-dash-buffer-local-go-docsets)
  ;; Java
  (defun helm-dash-buffer-local-java-docsets ()
    (my/dash-docs-local-docsets '("Java"))
    (my/dash-docs-local-docsets '("RxJava" "JavaFx") t))
  (add-hook 'java-mode-hook 'helm-dash-buffer-local-java-docsets)
  ;; SQL
  (defun helm-dash-buffer-local-sql-docsets ()
    (cl-case sql-product
      ('ansi (my/dash-docs-local-docsets '("SQLite" "PostgreSQL" "MySQL")))
      ('sqlite (my/dash-docs-local-docsets '("SQLite")))
      ('postgresql (my/dash-docs-local-docsets '("PostgreSQL")))
      ('postgres (my/dash-docs-local-docsets '("PostgreSQL")))
      ('mariadb (my/dash-docs-local-docsets '("MySQL")))
      ('mysql (my/dash-docs-local-docsets '("MySQL")))))
  (add-hook 'sql-mode-hook 'helm-dash-buffer-local-sql-docsets)
  (add-hook 'sql-interactive-mode-hook 'helm-dash-buffer-local-sql-docsets)
  ;; MongoDB
  (defun helm-dash-buffer-local-mongodb-docsets ()
    (my/dash-docs-local-docsets '("MongoDB")))
  (add-hook 'inf-mongo-mode-hook 'helm-dash-buffer-local-mongodb-docsets)
  ;; Redis
  (defun helm-dash-buffer-local-redis-docsets ()
    (my/dash-docs-local-docsets '("Redis")))
  (add-hook 'redis-mode-hook 'helm-dash-buffer-local-redis-docsets)
  ;; Neo4j
  (defun helm-dash-buffer-local-neo4j-docsets ()
    (my/dash-docs-local-docsets '("Neo4j")))
  (add-hook 'cypher-mode-hook 'helm-dash-buffer-local-neo4j-docsets)
  ;; LaTeX
  (defun helm-dash-buffer-local-latex-docsets ()
    (my/dash-docs-local-docsets '("LaTeX")))
  (add-hook 'latex-mode-hook 'helm-dash-buffer-local-latex-docsets)
  (add-hook 'LaTeX-mode-hook 'helm-dash-buffer-local-latex-docsets)
  ;; PlantUML
  (defun helm-dash-buffer-local-PlantUML-docsets ()
    (my/dash-docs-local-docsets '("PlantUML")))
  (add-hook 'plantuml-mode-hook 'helm-dash-buffer-local-PlantUML-docsets)
  ;; R
  (defun helm-dash-buffer-local-R-docsets ()
    (my/dash-docs-local-docsets '("R")))
  (add-hook 'ess-r-mode-hook 'helm-dash-buffer-local-R-docsets)
  ;; Julia
  (defun helm-dash-buffer-local-julia-docsets ()
    (my/dash-docs-local-docsets '("Julia")))
  (add-hook 'ess-julia-mode-hook 'helm-dash-buffer-local-julia-docsets)
  (add-hook 'julia-mode-hook 'helm-dash-buffer-local-julia-docsets)
  ;; Docker
  (with-eval-after-load 'dockerfile-mode
    (defun helm-dash-buffer-local-docker-docsets ()
      (my/dash-docs-local-docsets '("Docker")))
    (add-hook 'dockerfile-mode-hook 'helm-dash-buffer-local-docker-docsets))
  ;; Kubernetes
  (with-eval-after-load 'k8s-mode
    (defun helm-dash-buffer-local-kubernetes-docsets ()
      (my/dash-docs-local-docsets '("kubernetes")))
    (add-hook 'k8s-mode-hook 'helm-dash-buffer-local-kubernetes-docsets))
  ;; Vagrant
  ;; GraphQL
  (with-eval-after-load 'graphql-mode
    (defun helm-dash-buffer-local-GraphQL-docsets ()
      (my/dash-docs-local-docsets '("GraphQL Specification")))
    (add-hook 'graphql-mode-hook 'helm-dash-buffer-local-GraphQL-docsets))
  ;; Swift
  (with-eval-after-load 'swift-mode
    (defun helm-dash-buffer-local-swift-docsets ()
      (my/dash-docs-local-docsets '("Swift"))
      (my/dash-docs-local-docsets '("iOS" "OS_X") t))
    (add-hook 'swift-mode-hook 'helm-dash-buffer-local-swift-docsets))
  ;; Android
  (with-eval-after-load 'android-mode
    (defun helm-dash-buffer-local-android-docsets ()
      (my/dash-docs-local-docsets '("Android" "Java")))
    (add-hook 'android-mode-hook 'helm-dash-buffer-local-android-docsets))
  ;; Arduino
  (with-eval-after-load 'arduino-mode
    (defun helm-dash-buffer-local-arduino-docsets ()
      (my/dash-docs-local-docsets '("Arduino"))
      (my/dash-docs-local-docsets '("C") t))
    (add-hook 'arduino-mode-hook 'helm-dash-buffer-local-arduino-docsets))
  ;; Linux
  (with-eval-after-load 'nginx-mode
    (defun helm-dash-buffer-local-nginx-docsets ()
      (my/dash-docs-local-docsets '("Nginx")))
    (add-hook 'nginx-mode-hook 'helm-dash-buffer-local-nginx-docsets))
  (with-eval-after-load 'apache-mode
    (defun helm-dash-buffer-local-nginx-docsets ()
      (my/dash-docs-local-docsets '("Apache_HTTP_Server")))
    (add-hook 'apache-mode-hook 'helm-dash-buffer-local-apache-docsets)))

;;; [ zeal-at-point ]

;; (use-package zeal-at-point
;;   :ensure t
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
