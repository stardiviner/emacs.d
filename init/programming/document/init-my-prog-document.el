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

(if (featurep 'helm)
    (define-key my-prog-help-document-map (kbd "i") 'helm-info-at-point)
  )

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

(require 'dash)

;; Syntax highlighting of dash functions
(eval-after-load "dash" '(dash-enable-font-lock))


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
  :config
  (setq helm-dash-docsets-path (expand-file-name "~/.docsets")
        helm-dash-min-length 1
        ;; 'eww-browse-url, 'browse-url, 'browse-url-generic, 'helm-browse-url
        helm-dash-browser-func 'helm-browse-url-uzbl
        helm-dash-candidate-format "%d  %n  (%t)"
        
        helm-dash-common-docsets
        '("Ruby"
          "Python 3"
          ;; "PHP"
          "Ruby on Rails"
          "HTML" "CSS" "JavaScript"
          ;; "Emmet" "Haml" "Less"
          ;; "CoffeeScript"
          ;; "NodeJS"
          "jQuery" "AngularJS" "React" "D3JS"
          ;; "EmberJS" "ExtJS" "BackboneJS" "KnockoutJS" "MomentJS" "PrototypeJS" "RequireJS" "UnderscoreJS"
          ;; "RFCs"
          "Common Lisp"
          "Clojure"
          "C" ; "C++"
          "Go" ; "Swift" ; "Rust"
          ;; "Java"
          ;; "Elixir"
          ;; "Haskell" ; "Scala"
          ;; "Erlang"
          "SQLite" "PostgreSQL" ; "MySQL"
          ;; "Redis" "MongoDB"
          "Bash"
          ;; "LaTeX"
          "Julia" ; "R"
          ;; "Processing"
          ;; "Unity_3D" "Cocos3D" "Cocos2D"
          ;; "OpenGL_4" "OpenCV_C"
          "Docker" "Vagrant" ; "Nginx"
          ;; "Qt"
          ;; "Android" "iOS" "OS_X"
          ;; "RubyMotion"
          ;; "Arduino"
          )

        )
  
  (setq helm-dash-enable-debugging nil)

  (define-key my-prog-help-document-map (kbd "C-d") 'helm-dash-at-point) ; `helm-dash'
  
  ;; buffer local docsets
  ;; (defun go-doc ()
  ;;   (interactive)
  ;;   (setq-local helm-dash-docsets '("Go")))
  ;; (add-hook 'go-mode-hook 'go-doc)

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

(define-key my-prog-help-document-map (kbd "M-d") 'dash-at-point)
(define-key my-prog-help-document-map (kbd "M-e") 'dash-at-point-with-docset)

;; (add-to-list 'dash-at-point-mode-alist '(perl-mode . "perl"))

(add-hook 'projectile-rails-mode-hook
          (lambda () (setq dash-at-point-docset "rails")))
(add-hook 'rhtml-mode-hook
          (lambda () (setq dash-at-point-docset "rails")))

(add-hook 'ruby-mode-hook
          (lambda () (setq dash-at-point-docset "ruby")))



;;; [ RFC ]

(require 'init-my-prog-document-rfc)


;;; [ Man/Women ]

(require 'init-my-prog-document-man)


;;; [ Assistant ]

(require 'init-my-prog-document-assistant)



(provide 'init-my-prog-document)

;;; init-my-prog-document.el ends here
