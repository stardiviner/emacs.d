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

(require 'which-func)

;; (setq which-func-modes t)
(add-to-list 'which-func-modes 'org-mode)

(which-function-mode 1)


;;; [ Info ]

(define-key my-prog-help-document-map (kbd "i") 'helm-info-at-point)


;;; [ API docsets ]


;;; [ dash ] -- A modern list api for Emacs. No 'cl required.

;;; Usage:

(require 'dash)

;; Syntax highlighting of dash functions
(eval-after-load "dash" '(dash-enable-font-lock))


;;; [ helm-dash ] -- Browse Dash docsets inside emacs.

;;; Usage:
;; - [M-x helm-dash]
;; - [M-x helm-dash-at-point]
;; - The command helm-dash-reset-connections will clear the connections to all sqlite db's.
;;   Use it in case of errors when adding new docsets. The next call to helm-dash will recreate them.

(require 'helm-dash)

(setq helm-dash-docsets-path (expand-file-name "~/.docsets")
      helm-dash-min-length 3
      ;; helm-dash-completing-read-func 'completing-read ; 'completing-read, 'ido-completing-read
      helm-dash-browser-func 'browse-url ; 'browse-url, 'eww
      ;; helm-dash-connections
      helm-dash-common-docsets '("Ruby" "Ruby on Rails"
                                 "Python 3"
                                 "HTML" "CSS" "JavaScript" "CoffeeScript"
                                 "Common_Lisp" "Clojure"
                                 "C" "Go"
                                 "SQLite" "MySQL" "Redis" "MongoDB"
                                 "Qt"
                                 "Vagrant" "Nginx"
                                 "Android"
                                 "RubyMotion" "AngularJS"
                                 )
      ;; helm-dash-docsets
      )

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



;;; [ RFC ]

(require 'init-my-prog-document-rfc)


;;; [ Man/Women ]

(require 'init-my-prog-document-man)


;;; [ Assistant ]

(require 'init-my-prog-document-assistant)



(provide 'init-my-prog-document)

;;; init-my-prog-document.el ends here
