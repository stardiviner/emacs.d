;;; init-slime.el --- init for SLIME
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SLIME ]

(use-package slime
  :ensure t
  :commands (slime)
  :config
  ;; select the default value from slime-lisp-implementations
  (if (eq system-type 'darwin)
      ;; default to Clozure CL on OS X
      (setq slime-default-lisp 'ccl)
    ;; default to SBCL on Linux and Windows
    (setq slime-default-lisp 'sbcl))

  (add-to-list 'slime-contribs 'slime-fancy)

  ;; a list of alternative Common Lisp implementations that can be
  ;; used with SLIME. Note that their presence render
  ;; inferior-lisp-program useless. This variable holds a list of
  ;; programs and if you invoke SLIME with a negative prefix
  ;; argument, M-- M-x slime, you can select a program from that list.
  ;;
  (setq slime-lisp-implementations
        '((ccl ("ccl"))
          (clisp ("clisp" "-q"))
          (cmucl ("cmucl" "-quiet"))
          (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
  
  (setq slime-completion-at-point-functions '(slime-filename-completion
                                              slime-simple-completion-at-point)
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t)


  ;; auto start SLIME unless it's already running.
  (add-hook 'slime-mode-hook
            (lambda ()
              (unless (slime-connected-p)
                (save-excursion (slime)))))

  ;; (add-hook 'slime-load-hook
  ;;           #'(lambda ()
  ;;               (define-key slime-prefix-map (kbd "M-h")
  ;;                 'slime-documentation-lookup)))
  (eval-after-load 'slime
    `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))
  
  (add-hook 'slime-repl-mode-hook
            '(lambda ()
               (add-hook (make-local-variable 'completion-at-point-functions)
                         'slime-complete-symbol)
               (setq-local tab-always-indent 'complete)
               ))

  ;; notify user after SLIME connected
  (add-hook 'slime-connected-hook
            (lambda ()
              (notifications-notify :title "SLIME subprocess"
                                    :body "SLIME connected.")))
  
  ;; enable SLIME in lisp mode.
  ;; (add-hook 'lisp-mode-hook 'slime-mode)
  )


;;; [ slime-company ] -- slime backend for Company mode.

(use-package slime-company
  :ensure t
  :config
  (setq slime-company-after-completion nil
        slime-company-completion 'fuzzy
        slime-company-complete-in-comments-and-strings t
        slime-company-major-modes '(lisp-mode slime-repl-mode scheme-mode)
        )

  ;; [ enable ]

  ;; (with-eval-after-load 'slime
  ;;   (slime-setup '(slime-company)))

  (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
    (add-hook h 'my-slime-company-maybe-enable))

  (defun my-slime-company-maybe-enable ()
    (when (slime-company-active-p)
      ;; mainly change to my own company-backends adding style.
      (my-company-add-backends-to-mode 'company-slime)
      (unless (slime-find-contrib 'slime-fuzzy)
        (setq slime-company-completion 'simple)))
    )
  
  )


;;; Fontify *SLIME Description* buffer for SBCL

(defun slime-description-fontify ()
  "Fontify sections of SLIME Description."
  (with-current-buffer "*SLIME Description*"
    (highlight-regexp
     (concat "^Function:\\|"
             "^Macro-function:\\|"
             "^Its associated name.+?) is\\|"
             "^The .+'s arguments are:\\|"
             "^Function documentation:$\\|"
             "^Its.+\\(is\\|are\\):\\|"
             "^On.+it was compiled from:$")
     'hi-green-b)))

(advice-add 'slime-show-description :after #'slime-description-fontify)


;;; Improve usability of slime-apropos: slime-apropos-minor-mode

;; (defvar slime-apropos-anchor-regexp "^[^ ]")
;; (defun slime-apropos-next-anchor ()
;;   (interactive)
;;   (let ((pt (point)))
;;     (forward-line 1)
;;     (if (re-search-forward slime-apropos-anchor-regexp nil t)
;;         (goto-char (match-beginning 0))
;;       (goto-char pt)
;;       (error "anchor not found"))))

;; (defun slime-apropos-prev-anchor ()
;;   (interactive)
;;   (let ((p (point)))
;;     (if (re-search-backward slime-apropos-anchor-regexp nil t)
;;         (goto-char (match-beginning 0))
;;       (goto-char p)
;;       (error "anchor not found"))))

;; (defvar slime-apropos-minor-mode-map (make-sparse-keymap))
;; (define-key slime-apropos-minor-mode-map "\C-m" 'slime-describe-symbol)
;; (define-key slime-apropos-minor-mode-map "l" 'slime-describe-symbol)
;; (define-key slime-apropos-minor-mode-map "j" 'slime-apropos-next-anchor)
;; (define-key slime-apropos-minor-mode-map "k" 'slime-apropos-prev-anchor)
;; (define-minor-mode slime-apropos-minor-mode "")


;;; Integrate yas/expend to TAB key

;;; slime-indent-and-complete-symbol is a good function to bind to TAB key, but
;;; if you use Yasnippet, you can let TAB do indent, complete and yas/expand.

;; (defun slime-tab ()
;;   "slime-mode tab dwim, either indent, complete symbol or yas/expand"
;;   (interactive)
;;   (let ((r (slime-indent-and-complete-symbol)))
;;     (unless r
;;       (yas/expand))))

;; (defun my-slime-mode-hook ()
;;   (interactive)
;;   (define-key slime-mode-map (kbd "<tab>")
;;     'slime-tab)
;;   )
;; (add-hook 'slime-mode-hook 'my-slime-mode-hook)



(provide 'init-slime)

;;; init-slime.el ends here
