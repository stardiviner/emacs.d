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

  ;; (add-hook 'slime-load-hook
  ;;           #'(lambda ()
  ;;               (define-key slime-prefix-map (kbd "M-h")
  ;;                 'slime-documentation-lookup)))
  (eval-after-load 'slime
    `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))

  ;; notify user after SLIME connected
  (add-hook 'slime-connected-hook
            (lambda ()
              (notifications-notify :title "SLIME subprocess"
                                    :body "SLIME connected.")))

  ;; disable slime in `lisp-mode-hook'. except other derived modes.
  (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

  (add-hook 'lisp-mode-hook 'slime-mode)
  
  ;; SLIME REPL buffer
  (add-hook 'slime-repl-mode-hook 'slime-mode)
  (add-hook 'slime-repl-mode-hook 'eldoc-mode)

  ;; setup `*inferior-lisp*' buffer (`comint-mode' of SBCL)
  (defun slime-sbcl-inferior-lisp-buffer-setup ()
    (if (equal (buffer-name) "*inferior-lisp*")
        (progn
          (eldoc-mode 1)
          (slime-mode 1)
          (my-company-add-backends-to-mode '(company-slime)))))
  
  (add-hook 'comint-mode-hook 'slime-sbcl-inferior-lisp-buffer-setup)

  ;; auto start SLIME unless it's already running.
  (defun my-slime-connect ()
    ;; only start SLIME on lisp-mode. (except other lisp dialects: `sibilant-mode' etc)
    (if (equal major-mode 'lisp-mode)
        (unless (slime-connected-p)
          (save-excursion (slime)))
      ;; (cond (slime-mode (slime--on))
      ;;       (t (slime--off)))
      )
    )
  
  (add-hook 'lisp-mode-hook 'my-slime-connect)

  ;; bind slime-profile-* commands keybindings.
  (define-key slime-mode-map (kbd "C-c M-e") 'slime-pprint-eval-last-expression)
  (unless (boundp 'slime-profile)
    (define-prefix-command 'slime-profile))
  (define-key slime-mode-map (kbd "C-c C-p") 'slime-profile)
  (define-key slime-profile (kbd "f") 'slime-toggle-profile-fdefinition)
  (define-key slime-profile (kbd "p") 'slime-profile-package)
  (define-key slime-profile (kbd "s") 'slime-profile-by-substring)
  (define-key slime-profile (kbd "u") 'slime-unprofile-all)
  (define-key slime-profile (kbd "R") 'slime-profile-reset)
  (define-key slime-profile (kbd "r") 'slime-profile-report)
  (define-key slime-profile (kbd "l") 'slime-profiled-functions)
  )


;;; [ slime-company ] -- slime backend for Company mode.

(use-package slime-company
  :ensure t
  :init
  (require 'slime-company)
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
      (my-company-add-backends-to-mode '(company-slime))
      (unless (slime-find-contrib 'slime-fuzzy)
        (setq slime-company-completion 'simple)))
    )
  
  )


;;; Fontify *SLIME Description* buffer for SBCL

(defun slime-description-fontify ()
  "Fontify sections of SLIME Description."
  (with-current-buffer "*slime-description*"
    (highlight-regexp
     (concat "^#<FUNCTION\ .*>\\|"
             "^Lambda-list:\\|"
             "^Declared type:\\|"
             "^Derived type:\\|"
             "^Documentation:\\|"
             "Example:\\|"
             "^Source file:\\|"
             ".LISP$"
             )
     'hi-blue-b)))

(defadvice slime-show-description (after slime-description-fontify activate)
  (slime-description-fontify))


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
