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

(setq which-func-modes t)
(add-to-list 'which-func-modes 'org-mode)

(which-function-mode 1)


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
;;
;; - `helm-dash-install-docset' :: install official docset.
;; - `helm-dash-install-user-docset' :: install user contributed docset.
;; - `helm-dash-install-docset-from-file' :: install docset from file.
;;
;; - The command helm-dash-reset-connections will clear the connections to all sqlite db's.
;;   Use it in case of errors when adding new docsets. The next call to helm-dash will recreate them.

;; - https://github.com/Kapeli/Dash-User-Contributions/tree/master/docsets
;; - https://github.com/kidd/dashes-to-dashes

(setq helm-dash-docsets-path (expand-file-name "~/.docsets")
      helm-dash-min-length 2
      ;; helm-dash-completing-read-func 'completing-read ; 'completing-read, 'ido-completing-read
      helm-dash-browser-func 'helm-browse-url ; 'eww, 'browse-url, 'browse-url-generic, 'helm-browse-url
      ;; helm-dash-connections
      helm-dash-common-docsets
      '("Ruby" "Ruby on Rails"
        "Python 3"
        ;; "PHP"
        "HTML" "CSS" "JavaScript"
        "Emmet" "Haml" "Less"
        "CoffeeScript" "NodeJS"
        "jQuery" "AngularJS" "React" "D3JS"
        ;; "EmberJS" "ExtJS" "BackboneJS" "KnockoutJS" "MomentJS" "PrototypeJS" "RequireJS" "UnderscoreJS"
        "Common_Lisp"
        "Clojure"
        "C" "C++" "Go" ; "Swift" ; "Rust"
        ;; "Java"
        "Elixir"
        "Haskell" ; "Scala"
        ;; "Erlang"
        "SQLite" "PostgreSQL" "MySQL" "Redis" "MongoDB"
        ;; "Qt"
        "Bash"
        ;; "LaTeX"
        "Julia" ; "R"
        ;; "Processing"
        ;; "Unity_3D" "Cocos3D" "Cocos2D"
        ;; "OpenGL_4" "OpenCV_C"
        "Docker" "Vagrant" "Nginx"
        ;; "Android" "iOS" "OS_X"
        "RubyMotion"
        "Arduino"
        )
      )

(setq helm-dash-enable-debugging nil)

;;; buffer local docsets
;; (defun go-doc ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Go")))
;; (add-hook 'go-mode-hook 'go-doc)

;;; Only one docset
;; To narrow the search to just one docset, type its name in the beginning of the search followed by a space.
;; If the docset contains spaces, no problemo, we handle it :D.


;; (eval-after-load "helm-dash"
;;   '(defun helm-dash-actions (actions doc-item) `(("Go to doc" . eww))))

;;; open doc as you type
;;
;; This works kinda ok, but it's super slow. makes everything sluggish.  We
;; should investigate on that, There's also helm-idle-delay worth investigating.
;;
;; (add-hook 'helm-update-hook 'helm-dash-update t)
;; (defun helm-dash-update ()
;;   (interactive)
;;   (with-selected-window
;;     (eww (helm-get-selection))))

(define-key my-prog-help-document-map (kbd "C-d") 'helm-dash-at-point)
(define-key my-prog-help-document-map (kbd "M-d") 'helm-dash)

;;; show short doc of helm-dash entry in `helm-M-x' persistent action.
;;
;; (defun helm-dash-actions (actions doc-item)
;;   "Return an alist with the possible actions to execute with DOC-ITEM."
;;   `(("Go to doc" . helm-dash-browse-url)
;;     ("Copy to clipboard" . helm-dash-copy-to-clipboard)))

;; (defun helm-source-dash-search ()
;;   "Return an alist with configuration options for Helm."
;;   `((name . "Dash")
;;     (volatile)
;;     (delayed)
;;     (requires-pattern . ,helm-dash-min-length)
;;     (candidates-process . helm-dash-search)
;;     (action-transformer . helm-dash-actions)))


;; (defun helm-dash ()
;;   "Bring up a Dash search interface in helm."
;;   (interactive)
;;   (helm-dash-create-common-connections)
;;   (helm-dash-create-buffer-connections)
;;   (helm :sources (list (helm-source-dash-search))
;; 	:buffer "*helm-dash*"))


;; (defun helm-dash-at-point ()
;;   "Bring up a Dash search interface in helm using the symbol at
;; point as prefilled search."
;;   (interactive)
;;   (helm-dash-create-common-connections)
;;   (helm-dash-create-buffer-connections)
;;   (helm :sources (list (helm-source-dash-search))
;;         :buffer "*helm-dash*"
;;         :input (thing-at-point 'symbol)))

;; ;; reference `helm-find-files'
;; ;; `helm-source-mu'
;; (defvar helm-source-mu
;;   '((name . "Search email with mu")
;;     (candidates-process . helm-mu-init)
;;     (candidate-transformer . (helm-mu-candidate-parser
;;                               helm-mu-candidates-formatter))
;;     (delayed)
;;     (no-matchplugin)
;;     (nohighlight)
;;     (requires-pattern . 3)
;;     (persistent-action . helm-mu-persistent-action)
;;     (action . (("Display message in mu4e" . helm-mu-display-email)))))


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
