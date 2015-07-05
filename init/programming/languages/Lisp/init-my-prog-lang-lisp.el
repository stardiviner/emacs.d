;;; init-my-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]

;; (setq lisp-dialects-mode-hook '(lisp-mode-hook
;;                                 lisp-interaction-mode-hook
;;                                 emacs-lisp-mode-hook
;;                                 ;; common-lisp-mode-hook
;;                                 scheme-mode-hook
;;                                 clojure-mode-hook
;;                                 cider-repl-mode-hook
;;                                 ))

(defvar lisp-dialects-mode
  '(lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    ;; common-lisp-mode
    scheme-mode
    clojure-mode
    cider-repl-mode
    ))


;; - 'lisp-indent-function
;; - 'common-lisp-indent-function
;; (setq lisp-indent-function 'lisp-indent-function)


;;; [ SLIME ]

;;; Usage:
;;; - [M-x slime] ::

(setq inferior-lisp-program "sbcl")

;; select the default value from slime-lisp-implementations
(if (eq system-type 'darwin)
    ;; default to Clozure CL on OS X
    (setq slime-default-lisp 'ccl)
  ;; default to SBCL on Linux and Windows
  (setq slime-default-lisp 'sbcl))

;; a list of alternative Common Lisp implementations that can be
;; used with SLIME. Note that their presence render
;; inferior-lisp-program useless. This variable holds a list of
;; programs and if you invoke SLIME with a negative prefix
;; argument, M-- M-x slime, you can select a program from that list.
;;
;; (setq slime-lisp-implementations
;;       '((ccl ("ccl"))
;;         (clisp ("clisp" "-q"))
;;         (cmucl ("cmucl" "-quiet"))
;;         (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

;; NOTE: currently using slime from el-get installation.
;; (require 'slime-autoloads)
;;; -----------------------------------
;;; Quicklisp SLIME
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (require 'slime)
;;; -----------------------------------

;; (setq slime-contribs '(slime-fancy))

;;; FIXME: can't connect with SLIME.
;; (defun my-start-slime ()
;;   "Start SLIME unless it's already running."
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))
;;
;; ;; start slime automatically when we open a lisp file
;; (add-hook 'slime-mode-hook 'my-start-slime)
;;
;; (eval-after-load 'slime
;;   '(progn
;;      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol ; 'slime-simple-complete-symbol.
;;            slime-fuzzy-completion-in-place t
;;            slime-enable-evaluate-in-emacs t
;;            slime-autodoc-use-multiline-p t)
;;
;;      (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
;;      (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
;;      (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))


;;; [ ac-slime ] --

;; (require 'ac-slime)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 scheme-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (add-to-list 'ac-sources 'ac-slime))))
;;
;;
;; (set-face-attribute 'ac-slime-menu-face nil
;;                     :foreground "yellow"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-slime-selection-face nil
;;                     )


;;; [ slime-company ] -- slime backend for Company mode.

;; FIXME: (slime-setup '(slime-company))


;; A quick way to jump to the definition of a function given its key binding
;; (global-set-key (kbd "C-h K") 'find-function-on-key)


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





;;; [ Geiser ] -- Geiser is a collection of Emacs major and minor modes that conspire with one or more Scheme interpreters to keep the Lisp Machine Spirit alive.

;;; Geiser is a collection of Emacs major and minor modes that conspire with one
;;; or more Scheme interpreters to keep the Lisp Machine Spirit alive. It draws
;;; inspiration (and a bit more) from environments such as Common Lisp’s Slime,
;;; Factor’s FUEL, Squeak or Emacs itself, and does its best to make Scheme
;;; hacking inside Emacs (even more) fun.

;;; Or, to be precise, what i consider fun. Geiser is thus my humble
;;; contribution to the dynamic school of expression, and a reaction against
;;; what i perceive as a derailment, in modern times, of standard Scheme towards
;;; the static camp. Because i prefer growing and healing to poking at corpses,
;;; the continuously running Scheme interpreter takes the center of the stage in
;;; Geiser. A bundle of Elisp shims orchestrates the dialog between the Scheme
;;; interpreter, Emacs and, ultimately, the schemer, giving her access to live
;;; metadata. Here’s how.

(require 'geiser-install)

;; (run-geiser)


;;; [ ac-geiser ]

;; (require 'ac-geiser)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 scheme-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (ac-geiser-setup)
;;                    (add-to-list 'ac-sources 'ac-source-geiser))))


;;; [ Quack ] -- enhanced Emacs Support for Editing and Running Scheme Code

;;; http://www.neilvandyke.org/quack/


;;; [ hl-sexp ]

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                ))
  (add-hook hook #'hl-sexp-mode))

(set-face-attribute 'hl-sexp-face nil
                    :background (color-darken-name (face-background 'default) 3)
                    )


;;; [ eval-sexp-fu ] -- You can see highlighting the sexps during evaluation in action.

;;; Usage:
;;
;;  `eval-sexp-fu-flash-mode'
;;    Toggle EvalSexpFuFlash mode on or off.
;;    If this mode is on, some `eval-last-sexp'-ish commands will highlight the sexps during evaluation.
;;  `turn-on-eval-sexp-fu-flash-mode'
;;    Unequivocally turn on EvalSexpFuFlash mode
;;  `eval-sexp-fu-eval-sexp-inner-list'
;;    Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.
;;  `eval-sexp-fu-eval-sexp-inner-sexp'
;;    Evaluate the sexp _currently_ pointed; print value in minibuffer.

(require 'eval-sexp-fu)

(set-face-attribute 'eval-sexp-fu-flash nil
                    :foreground nil
                    :background "dark red"
                    )
(set-face-attribute 'eval-sexp-fu-flash-error nil
                    :foreground "red"
                    :weight 'bold)

(setq eval-sexp-fu-flash-duration 0.5
      eval-sexp-fu-flash-error-duration 1.5
      ;; eval-sexp-fu-flash-function
      ;; eval-sexp-fu-flash-doit-function
      )

(eval-sexp-fu-flash-mode 1)


(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
