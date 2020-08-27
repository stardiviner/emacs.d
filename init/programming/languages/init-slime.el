;;; init-slime.el --- init for SLIME
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SLIME ]

(use-package slime
  :ensure t
  :defer t
  :commands (slime)
  :init
  ;; disable slime in `lisp-mode-hook'. except other derived modes.
  (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

  ;; enable slime-mode for setup to support SLIME.
  (dolist (hook '(lisp-mode-hook
                  lisp-interaction-mode-hook
                  slime-repl-mode-hook))
    (add-hook hook 'slime-mode))

  ;; auto start SLIME unless it's already running.
  (defun my-slime-auto-start ()
    (interactive)
    ;; only start SLIME on lisp-mode. (except other lisp dialects:
    ;; `sibilant-mode' etc)
    (if (equal major-mode 'lisp-mode)
        (unless (slime-connected-p)
          (save-excursion (slime)))
      ;; (cond (slime-mode (slime--on))
      ;;       (t (slime--off)))
      ))
  
  ;; (add-hook 'lisp-mode-hook 'my-slime-auto-start)

  ;; manage SLIME popup buffers
  (add-to-list 'display-buffer-alist '("\\*slime-repl.*\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("\\*slime-events.*\\*" . (display-buffer-below-selected)))
  :config
  ;; select the default value from slime-lisp-implementations
  (if (eq system-type 'darwin)
      ;; default to Clozure CL on OS X
      (setq slime-default-lisp 'ccl)
    ;; default to SBCL on Linux and Windows
    (setq slime-default-lisp 'sbcl))

  (add-to-list 'slime-contribs 'slime-fancy)
  ;; load slime-repl.
  (slime-setup '(slime-repl))

  ;; a list of alternative Common Lisp implementations that can be
  ;; used with SLIME. Note that their presence render
  ;; inferior-lisp-program useless. This variable holds a list of
  ;; programs and if you invoke SLIME with a negative prefix
  ;; argument, M-- M-x slime, you can select a program from that list.
  ;;
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
          (ccl ("ccl"))
          (clisp ("clisp" "-q"))
          (cmucl ("cmucl" "-quiet"))))
  
  ;; (setq slime-completion-at-point-functions
  ;;       '(slime-filename-completion
  ;;         slime-simple-completion-at-point))
  ;; (setq slime-enable-evaluate-in-emacs t)

  ;; (setq slime-auto-start 'always)

  (add-hook 'slime-repl-mode-hook #'my-lisp-repl-common-settings)

  (add-hook 'common-lisp-mode-hook
            (lambda ()
              (local-set-key (kbd "C-h d") 'document-prefix)
              (define-key document-prefix (kbd "d") 'slime-documentation)))
  
  ;; notify user after SLIME connected
  (add-hook 'slime-connected-hook
            (lambda ()
              (notifications-notify :title "SLIME subprocess"
                                    :body "SLIME connected.")))
  
  ;; SLIME REPL buffer
  ;; setup `*inferior-lisp*' buffer (`comint-mode' of SBCL)
  (defun slime-sbcl-inferior-lisp-buffer-setup ()
    (if (equal (buffer-name) "*inferior-lisp*")
        (progn
          (slime-mode 1)
          (eldoc-mode 1)
          (paredit-mode 1)
          (my-company-add-backend-locally 'company-slime))))

  ;; (add-hook 'comint-mode-hook #'slime-sbcl-inferior-lisp-buffer-setup)
  (add-hook 'slime-repl-mode-hook #'slime-sbcl-inferior-lisp-buffer-setup)

  ;; inspect
  (define-key slime-mode-map (kbd "C-c C-i") 'slime-inspect)

  ;; toggle tracing defuns
  (define-key slime-mode-map (kbd "C-c C-t") 'slime-toggle-trace-fdefinition)
  
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

  ;; connect to a remote Lisp machine.
  (setq slime-net-coding-system 'utf-8-unix)

  ;; Fontify *SLIME Description* buffer for SBCL
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
               ".LISP$")
       'hi-blue-b)))

  (advice-add 'slime-show-description :after 'slime-description-fontify)
  
  ;; [ slime-company ] -- slime backend for Company mode.
  (use-package slime-company
    :ensure t
    :config
    (with-eval-after-load 'slime
      (slime-setup '(slime-company)))
    
    (setq slime-company-after-completion 'slime-company-just-one-space
          slime-company-completion 'fuzzy
          slime-company-complete-in-comments-and-strings t
          slime-company-major-modes '(lisp-mode slime-repl-mode scheme-mode))

    (defun my-slime-company-maybe-enable ()
      (when (slime-company-active-p)
        (my-company-add-backend-locally 'company-slime)
        (unless (slime-find-contrib 'slime-fuzzy)
          (setq slime-company-completion 'simple))))
    
    (dolist (hook '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
      (add-hook hook 'my-slime-company-maybe-enable nil 'local))))


;;; Improve usability of slime-apropos: slime-apropos-minor-mode
;; TODO: test whether slime apropos-minor has this by default?
;; if yes, then remove this.

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


(provide 'init-slime)

;;; init-slime.el ends here
