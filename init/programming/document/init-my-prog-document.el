;;; init-my-prog-document.el --- init for Programming Document Look Up.

;;; Commentary:

;;; Code:

;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(require 'eldoc)

(dolist (hook
         '(emacs-lisp-mode-hook
           lisp-interaction-mode-hook
           lisp-mode-hook
           ielm-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

;; (when turn-on-eldoc-mode
;;   (setq (make-local-variable 'eldoc-documentation-function) 'robe-eldoc))

(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "cyan"
                    :weight 'bold)


;;; ElDoc with most paredit command.
;;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
(eldoc-add-command 'paredit-backward-delete
                   'paredit-close-round)

;;; add docstring to ElDoc output.
;;; TODO
;; (defadvice eldoc-get-fnsym-args-string (after add-dacstring (sym)
;;                                               activate compile)
;;   "Add a doc string to ElDoc's modeline information."
;;   (let ((doc (eldoc-docstring-first-line
;;               (cdr (help-split-fundoc (documentation sym t) sym)))))
;;     (when (and doc (not (equal doc "")))
;;       (setq ad-return-value
;;             (concat ad-return-value
;;                     (if (> (+ (length ad-return-value) (length doc) 4)
;;                            (frame-width)) "\n" "    ")
;;                     doc))))
;;   ad-return-value)


;;; [ which-function-mode (which-func) ]

;; ;; (add-to-list 'which-func-modes 'org-mode)
;; (setq which-func-modes t) ; for all modes.
;;
;; (which-function-mode 1)


;;; [ Info ]

(global-set-key (kbd "C-h i") 'info-display-manual)

(defun info-display-manual-in-buffer (topic)
  "Display Info TOPIC in its own buffer."
  (interactive
   (list
    (progn
      (info-initialize)
      (completing-read "Info Manual name: "
                       (info--manual-names)
                       nil t))))
  (let ((bufname (format "*info: %s*" topic)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (info topic bufname))))

;; (global-set-key (kbd "C-h i") 'info-display-manual-in-buffer) ; FIXME: replace default `info'.


;;; [ API docsets ]


;;; [ dash ] -- A modern list api for Emacs. No 'cl required.

(use-package dash
  :ensure t
  :config
  ;; Syntax highlighting of dash functions
  (eval-after-load "dash" '(dash-enable-font-lock))
  )


;;; [ helm-dash ] -- Browse Dash docsets inside emacs.

;;; Usage:
;;
;; - [M-x helm-dash]
;; - [M-x helm-dash-at-point]
;; - [M-x helm-dash-reset-connections] :: fix/reset sqlite db's error
;;
;; - `helm-dash-install-docset' :: install official docset.
;; - `helm-dash-install-user-docset' :: install user contributed docset.
;; - `helm-dash-install-docset-from-file' :: install docset from file.

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-docsets-path (expand-file-name "~/.docsets")
        helm-dash-min-length 3
        ;; 'eww-browse-url, 'browse-url, 'browse-url-generic, 'helm-browse-url
        helm-dash-browser-func 'browse-url-conkeror
        helm-dash-candidate-format "%d  %n  (%t)"
        )
  
  (setq helm-dash-common-docsets
        '("Ruby"
          ;; "Ruby on Rails"
          ;; "HTML" "CSS"
          ;; "JavaScript"
          ;; "Redis" "MongoDB"
          ;; "RubyMotion"
          )
        )

  (setq helm-dash-enable-debugging t)

  (define-key my-prog-help-document-map (kbd "C-d") 'helm-dash-at-point) ; `helm-dash'
  
  ;; buffer local docsets
  (defun my-helm-dash-buffer-local-docsets-add (docsets-list)
    (unless (boundp 'helm-dash-docsets)
      (defvar helm-dash-docsets '()))
    (make-local-variable 'helm-dash-docsets)
    (setq helm-dash-docsets
          (-union docsets-list (-flatten helm-dash-docsets)))
    )
  
  ;; Bash
  (defun helm-dash-buffer-local-shell-docsets ()
    (setq-local helm-dash-docsets '("Bash")))
  (add-hook 'sh-mode-hook 'helm-dash-buffer-local-shell-docsets)
  ;; Ruby
  (defun helm-dash-buffer-local-ruby-docsets ()
    (setq-local helm-dash-docsets '("Ruby")))
  (add-hook 'enh-ruby-mode-hook 'helm-dash-buffer-local-ruby-docsets)
  ;; Ruby on Rails
  (defun helm-dash-buffer-local-rails-docsets ()
    (my-helm-dash-buffer-local-docsets-add '("Ruby on Rails" "Ruby" "HTML" "JavaScript" "CSS" "jQuery"))
    )
  (add-hook 'projectile-rails-mode-hook 'helm-dash-buffer-local-rails-docsets)
  ;; Python
  (defun helm-dash-buffer-local-python-docsets ()
    (setq-local helm-dash-docsets '("Python 3")))
  (add-hook 'python-mode-hook 'helm-dash-buffer-local-python-docsets)
  ;; Web
  (defun helm-dash-buffer-local-web-docsets ()
    (setq-local helm-dash-docsets '("HTML" "CSS" "JavaScript"))
    (my-helm-dash-buffer-local-docsets-add '("jQuery"))
    )
  (add-hook 'web-mode-hook 'helm-dash-buffer-local-web-docsets)
  ;; JavaScript
  (defun helm-dash-buffer-local-javascript-docsets ()
    (setq-local helm-dash-docsets '("JavaScript" "NodeJS"))
    (my-helm-dash-buffer-local-docsets-add '("jQuery")))
  (add-hook 'js2-mode-hook 'helm-dash-buffer-local-javascript-docsets)
  ;; Common Lisp
  (defun helm-dash-buffer-local-common-lisp-docsets ()
    (setq-local helm-dash-docsets '("Common Lisp")))
  (add-hook 'common-lisp-mode-hook 'helm-dash-buffer-local-common-lisp-docsets)
  ;; Clojure
  (defun helm-dash-buffer-local-clojure-docsets ()
    (setq-local helm-dash-docsets '("Clojure")))
  (add-hook 'clojure-mode-hook 'helm-dash-buffer-local-clojure-docsets)
  ;; C
  (defun helm-dash-buffer-local-C-docsets ()
    (setq-local helm-dash-docsets '("C"))
    (my-helm-dash-buffer-local-docsets-add '("CMake")))
  (add-hook 'c-mode-hook 'helm-dash-buffer-local-C-docsets)
  ;; Go
  (defun helm-dash-buffer-local-go-docsets ()
    (setq-local helm-dash-docsets '("Go")))
  (add-hook 'go-mode-hook 'helm-dash-buffer-local-go-docsets)
  ;; Java
  (defun helm-dash-buffer-local-java-docsets ()
    (setq-local helm-dash-docsets '("Java")))
  (add-hook 'java-mode-hook 'helm-dash-buffer-local-java-docsets)
  ;; SQL
  (defun helm-dash-buffer-local-sql-docsets ()
    (setq-local helm-dash-docsets '("SQLite" "PostgreSQL" "MySQL")))
  (add-hook 'sql-mode-hook 'helm-dash-buffer-local-sql-docsets)
  ;; LaTeX
  (defun helm-dash-buffer-local-latex-docsets ()
    (setq-local helm-dash-docsets '("LaTeX")))
  (add-hook 'latex-mode-hook 'helm-dash-buffer-local-latex-docsets)
  (add-hook 'LaTeX-mode-hook 'helm-dash-buffer-local-latex-docsets)
  ;; ESS: Julia, R
  (defun helm-dash-buffer-local-ess-docsets ()
    (setq-local helm-dash-docsets '("Julia"))
    ;; (my-helm-dash-buffer-local-docsets-add '("R"))
    )
  (add-hook 'ess-mode-hook 'helm-dash-buffer-local-ess-docsets)
  ;; Docker
  (with-eval-after-load 'dockerfile-mode
    (defun helm-dash-buffer-local-docker-docsets ()
      (setq-local helm-dash-docsets '("Docker")))
    (add-hook 'dockerfile-mode-hook 'helm-dash-buffer-local-docker-docsets))
  ;; Swift
  (with-eval-after-load 'swift-mode
    (defun helm-dash-buffer-local-swift-docsets ()
      (setq-local helm-dash-docsets '("Swift"))
      (my-helm-dash-buffer-local-docsets-add '("iOS" "OS_X")))
    (add-hook 'swift-mode-hook 'helm-dash-buffer-local-swift-docsets))
  ;; Android
  (with-eval-after-load 'android-mode
    (defun helm-dash-buffer-local-android-docsets ()
      (setq-local helm-dash-docsets '("Android"))
      (my-helm-dash-buffer-local-docsets-add '("Java")))
    (add-hook 'android-mode-hook 'helm-dash-buffer-local-android-docsets))
  ;; Arduino
  (with-eval-after-load 'arduino-mode
    (defun helm-dash-buffer-local-arduino-docsets ()
      (setq-local helm-dash-docsets '("Arduino")))
    (add-hook 'arduino-mode-hook 'helm-dash-buffer-local-arduino-docsets))
  ;; Linux
  (with-eval-after-load 'nginx-mode
    (defun helm-dash-buffer-local-nginx-docsets ()
      (setq-local helm-dash-docsets '("Nginx")))
    (add-hook 'nginx-mode-hook 'helm-dash-buffer-local-nginx-docsets))
  (with-eval-after-load 'apache-mode
    (defun helm-dash-buffer-local-nginx-docsets ()
      (setq-local helm-dash-docsets '("Apache_HTTP_Server")))
    (add-hook 'apache-mode-hook 'helm-dash-buffer-local-apache-docsets))
  
  ;; (eval-after-load "helm-dash"
  ;;   '(defun helm-dash-actions (actions doc-item) `(("Go to doc" . eww))))

  ;; open doc as you type
  ;;
  ;; This works kinda ok, but it's super slow. makes everything sluggish.  We
  ;; should investigate on that, There's also helm-idle-delay worth
  ;; investigating.
  ;;
  ;; (add-hook 'helm-update-hook 'helm-dash-update t)
  ;; (defun helm-dash-update ()
  ;;   (interactive)
  ;;   (with-selected-window
  ;;     (eww (helm-get-selection))))
  )


;;; [ dash-at-point ] -- Search the word at point with Dash.

;;; Usage:
;;
;; - `dash-at-point'
;; - `dash-at-point-with-docset'

(use-package dash-at-point
  :ensure t
  :config
  (define-key my-prog-help-document-map (kbd "M-d") 'dash-at-point)
  (define-key my-prog-help-document-map (kbd "M-e") 'dash-at-point-with-docset)

  ;; (add-to-list 'dash-at-point-mode-alist '(perl-mode . "perl"))

  ;; (add-hook 'projectile-rails-mode-hook
  ;;           (lambda () (setq dash-at-point-docset "rails")))
  ;; (add-hook 'rhtml-mode-hook
  ;;           (lambda () (setq dash-at-point-docset "rails")))
  ;; (add-hook 'ruby-mode-hook
  ;;           (lambda () (setq dash-at-point-docset "ruby")))
  )


;;; [ RFC ]

(require 'init-my-prog-document-rfc)


;;; [ Man/Women ]

(require 'init-my-prog-document-man)


;;; [ Assistant ]

(require 'init-my-prog-document-assistant)



(provide 'init-my-prog-document)

;;; init-my-prog-document.el ends here
