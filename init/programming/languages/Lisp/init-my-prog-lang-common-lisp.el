;;; init-my-prog-lang-common-lisp.el --- init Common Lisp for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Common Lisp ]

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode))

;;; re-define upstream default function 'lispdoc key binding.
(define-key lisp-mode-map (kbd "C-h d") 'lispdoc)

(setq cl-lookup-categories
      '(:hyperspec-index           ; e.g. "", "spec" "CLHS"
        :hyperspec-chapters        ; e.g. [index], [syntax]
        :format-control-characters ; e.g. "~C: Character", "~%: Newline"
        :reader-macro-characters   ; e.g. "(", "#'", "#b", "#+"
        :loop                      ; e.g. loop:with, loop:collect
        :arguments                 ; e.g. :test, :key, :eof-error-p
        :concepts                  ; e.g. "lambda lists:", "character names:"
        "cl-lookup-glossary"       ; e.g. {absolute}, {binding}
        "cl-lookup-mop"            ; e.g. add-dependent, ensure-class

        ;; implementation specific categories
        ;; "cl-lookup-clisp"          ; e.g. ext:cd

        ;; library categories
        "cl-lookup-ppcre"          ; e.g. cl-ppcre:parse-tree-synonym
        ))


;;; [ SBCL ]

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))



;;; [ Quick Lisp ]

;;; Common Lisp support depends on SLIME being installed with Quicklisp
;;
;; (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (message "%s" "SLIME is not installed. Use Quicklisp to install it."))



;;; [ SLIME ]

(use-package slime
  :config
  ;; select the default value from slime-lisp-implementations
  (if (eq system-type 'darwin)
      ;; default to Clozure CL on OS X
      (setq slime-default-lisp 'ccl)
    ;; default to SBCL on Linux and Windows
    (setq slime-default-lisp 'sbcl))

  ;; (require 'slime-autoloads)
  ;; (require 'slime)
  
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
  
  ;; NOTE: currently using slime from el-get installation.
  ;; (require 'slime-autoloads)
  ;; -----------------------------------
  ;; Quicklisp SLIME
  ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; (require 'slime)
  ;; -----------------------------------

  (setq slime-completion-at-point-functions '(slime-filename-completion
                                              slime-simple-completion-at-point)
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t)


  ;; auto start SLIME unless it's already running.
  ;; (add-hook 'slime-mode-hook
  ;;           (lambda ()
  ;;             (unless (slime-connected-p)
  ;;               (save-excursion (slime)))))

  (add-hook 'slime-load-hook
            #'(lambda ()
                (define-key slime-prefix-map (kbd "M-h")
                  'slime-documentation-lookup)))
  ;; (eval-after-load 'slime
  ;;   `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))
  
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
  
  ;; load quicklisp installed SLIME.
  ;; (ql:quickload "quicklisp-slime-helper")

  ;; enable SLIME in lisp mode.
  ;; (add-hook 'lisp-mode-hook 'slime-mode)
  )


;;; [ slime-company ] -- slime backend for Company mode.

(use-package slime-company
  :config
  (with-eval-after-load 'slime
    (slime-setup '(slime-company)))
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

(defadvice slime-show-description (after slime-description-fontify activate)
  "Fontify sections of SLIME Description."
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

;; (defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
;;   ""
;;   (when (get-buffer "*SLIME Apropos*")
;;     (with-current-buffer "*SLIME Apropos*" (slime-apropos-minor-mode 1))))


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


;;; [ Swank ] (cl-swank) --



;;; [ sly ] -- Sylvester the Cat's Common Lisp IDE

(use-package sly
  :init
  ;; (require 'sly-autoloads)
  :commands (sly)
  :config
  (setq sly-lisp-implementations
        '((cmucl ("cmucl" "-quiet"))
          ;; (cmucl ("/opt/cmucl/bin/lisp" "-quiet") :init sly-init-command)
          (acl (\"acl7\") :coding-system emacs-mule)
          (sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

  (setq sly-default-lisp 'sbcl)

  (setq sly-contribs '(sly-fancy sly-retro
                                 sly-scratch
                                 sly-mrepl
                                 sly-autodoc))

  (dolist (hook '(sly-mode-hook
                  sly-mrepl-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  common-lisp-lisp-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (define-key my-prog-help-document-map (kbd "d") 'sly-documentation-lookup)
                )))
  
  (eval-after-load 'sly
    `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

  (eval-after-load 'sly-mrepl
    `(define-key sly-mrepl-mode-map (kbd "C-c C-k")
       'sly-mrepl-clear-recent-output))

  ;; notify user after SLY connected.
  (add-hook 'sly-connected-hook
            (lambda ()
              (notifications-notify :title "SLY subprocess"
                                    :body "SLY connected")))

  ;; auto connect SLY.
  (add-hook 'sly-mode-hook
            (lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))))

  ;; enable `sly-mode' in Lisps
  ;;
  ;; (dolist (hook '(sly-mode-hook
  ;;                 sly-mrepl-mode-hook
  ;;                 lisp-mode-hook
  ;;                 lisp-interaction-mode-hook
  ;;                 common-lisp-lisp-mode-hook
  ;;                 ))
  ;;   (add-hook hook 'sly-mode))
  )


;;; [ company-sly ] -- Company-mode completion backend for SLY.

(add-hook 'sly-mode-hook 'sly-company-mode)

(dolist (hook '(sly-mode-hook
                sly-mrepl-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                common-lisp-lisp-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (my-company-add-backends-to-mode '(sly-company))
              )))



(provide 'init-my-prog-lang-common-lisp)

;;; init-my-prog-lang-common-lisp.el ends here
