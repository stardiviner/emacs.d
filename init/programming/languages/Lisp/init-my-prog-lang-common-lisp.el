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

;;; Usage:
;;
;; - [M-x slime] ::
;; - `slime-mode'

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


  ;; start slime automatically when we open a lisp file
  (defun my-start-slime ()
    "Start SLIME unless it's already running."
    (interactive)
    (unless (slime-connected-p)
      (save-excursion (slime))))

  (add-hook 'slime-mode-hook 'my-start-slime)

  
  (add-hook 'slime-repl-mode-hook
            '(lambda ()
               (add-hook (make-local-variable 'completion-at-point-functions)
                         'slime-complete-symbol)
               (setq-local tab-always-indent 'complete)
               ))

  ;; load quicklisp installed SLIME.
  ;; (ql:quickload "quicklisp-slime-helper")
  )


;;; [ slime-company ] -- slime backend for Company mode.

(require 'slime-company)

(slime-setup '(slime-company))



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

;;; Usage:
;;
;; - [M-x sly] :: fire up SLY and connect to Lisp.
;; - `sly-connect' ::
;; - `sly-mode'

(require 'sly-autoloads)

;; (setq sly-lisp-implementations
;;       '((cmucl ("cmucl" "-quiet"))
;;         ;; (cmucl ("/opt/cmucl/bin/lisp" "-quiet") :init sly-init-command)
;;         (sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

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
              (unless (boundp 'lisp-help-doc-map)
                (define-prefix-command 'lisp-help-doc-map))
              (local-set-key (kbd "C-h d") 'lisp-help-doc-map)
              
              (define-key lisp-help-doc-map (kbd "d") 'sly-documentation-lookup)
              )))

(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(eval-after-load 'sly-mrepl
  `(define-key sly-mrepl-mode-map (kbd "C-c C-k")
     'sly-mrepl-clear-recent-output))


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
