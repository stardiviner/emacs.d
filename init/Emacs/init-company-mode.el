;;; init-company-mode.el --- init company-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Company Mode ]

;; TODO: https://github.com/company-mode/company-mode/wiki/Third-Party-Packages

;;; Annotations:
;; - <f> :: function
;; - ->  :: snippet (yasnippet)

;;; Usage:
;;
;; - [M-x global-company-mode] ::
;; - [M-:] -> [M-x company-mode] :: enable company-mode in minibuffer.

(require 'company)

(setq company-minimum-prefix-length 3
      company-idle-delay 0.5
      ;; determines when to auto-complete.
      ;; 'company-explicit-action-p, t, nil, 'function
      company-auto-complete 'company-explicit-action-p
      ;; company-auto-complete-chars
      ;; 'company-explicit-action-p, t, nil, 'function
      company-require-match 'company-explicit-action-p
      company-echo-delay 0
      ;; t: show quick-access numbers for the first ten candidates.
      company-show-numbers nil
      ;; align annotations to the right tooltip border.
      company-tooltip-align-annotations t
      company-tooltip-flip-when-above t
      ;; tooltip candidates max limit.
      company-tooltip-limit 10
      ;; minimum candidates height limit.
      company-tooltip-minimum 3
      ;; The minimum width of the tooltip's inner area.
      company-tooltip-minimum-width 0
      ;; This doesn't include the margins and the scroll bar.
      ;; width of margin columns to show around the tooltip
      company-tooltip-margin 1
      ;; 'lines - how to show tooltip unshown candidates number.
      company-tooltip-offset-display 'scrollbar
      ;; loop over candidates
      company-selection-wrap-around t
      )

;; (setq company-begin-commands '(self-insert-command
;;                                org-self-insert-command orgtbl-self-insert-command
;;                                c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash))

;;; quick help document preview & popup
(require 'company-quickhelp)
(setq company-quickhelp-delay nil) ; set to `nil' to trigger popup doc manually.
(company-quickhelp-mode 1)
(add-to-list 'company-frontends 'company-quickhelp-frontend)
;; (setq-default company-frontends (remq 'company-echo-metadata-frontend company-frontends))

;; (setq-default company-frontends
;;               '(company-pseudo-tooltip-unless-just-one-frontend
;;                 company-preview-if-just-one-frontend
;;                 company-echo-metadata-frontend
;;                 company-quickhelp-frontend
;;                 ))

;; Only one back-end is used at a time.  The choice depends on the order of
;; the items in this list, and on the values they return in response to the
;; `prefix' command (see below).  But a back-end can also be a \"grouped\"
;; one (see below).

(setq company-backends
      '((company-files          ; files & directory
         ;; company-gtags company-etags
         company-keywords       ; keywords
         ;; company-dabbrev-code
         ;; company-tempo          ; tempo: flexible template insertion
         company-capf           ; completion-at-point-functions
         ;; :with
         company-yasnippet
         )
        ;; company-elisp ; Emacs Lisp
        ;; company-semantic ; C/C++
        ;; (company-clang company-cmake) ; C/C++
        ;; company-eclim ; Java
        ;; company-nxml company-css ; HTML, CSS, XML
        ;; company-xcode ; for Xcode projects
        (company-abbrev company-dabbrev)
        ;; company-ispell ; Ispell
        ;; company-bbdb           ; BBDB
        ))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'company-backends)
;;                          'company-elisp)))

;;; mode local backends example:

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'company-backends)
;;                          '(company-clang company-cmake))))

;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-dabbrev-code company-yasnippet)))))

;; (setq company-begin-commands '(self-insert-command
;;                                org-self-insert-command orgtbl-self-insert-command
;;                                c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash
;;                                ))

;;; add `company-robe' backend to `company-backends'
;; (add-hook 'inf-ruby-mode-hook
;;           (lambda ()
;;             ;; company-robe
;;             (make-local-variable 'company-backends)
;;             (add-to-list 'company-backends 'company-robe)
;;             ))
;;
;;; remove `company-robe' from `company-backends'
;; (add-hook 'inf-ruby-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  (remq 'company-capf company-backends))))
;; or
;; (add-hook 'robe-mode-on-hook
;;           (lambda ()
;;             (remove-hook 'completion-at-point-functions
;;                          'robe-complete-at-point t)))

;; (add-to-list 'ac-modes 'inf-ruby-mode) ; enable auto-complete (with robe-mode) for inf-ruby completion.


;;; globally
(setq company-global-modes t)
;; (setq company-global-modes '(emacs-lisp-mode
;;                              org-mode
;;                              ruby-mode
;;                              prog-mode))

(add-hook 'after-init-hook 'global-company-mode)

;;; or enabled only in specific modes
;; (dolist (hook '(emacs-lisp-mode-hook
;;                 lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 ))
;;   (add-hook hook 'company-mode))

;; keybindings

;; manually start completion
;; (global-set-key (kbd "<tab>") 'company-indent-or-complete-common)
;; (global-set-key (kbd "<tab>") 'company-complete)
;; (global-set-key (kbd "TAB") 'company-complete)
;; (global-set-key [tab] 'company-complete)

;; snippet
;; TODO: `yas-expand', `yas-expand-from-trigger-key'
(define-key company-active-map [tab] 'yas-expand-from-trigger-key)
;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (let ((yas-fallback-behavior nil))
;;     (unless (yas-expand)
;;       (call-interactively #'company-complete-common))))
;;
;; ;; To make sure that this is called instead of `company-complete-common', use
;; (add-hook 'company-mode-hook
;;           (lambda ()
;;             (substitute-key-definition 'company-complete-common
;;                                        'company-yasnippet-or-completion
;;                                        company-active-map)))

;; navigation
(define-key company-active-map "\t" nil)
(define-key company-active-map [tab] nil)
(define-key company-active-map (kbd "<tab>") nil)
(define-key company-active-map (kbd "<S-tab>") nil)
(define-key company-active-map (kbd "C-n") nil)
(define-key company-active-map (kbd "C-p") nil)
(define-key company-active-map (kbd "C-j") nil)
(define-key company-active-map (kbd "C-g") 'company-abort)
(define-key company-active-map (kbd "M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p") 'company-select-previous)
(define-key company-active-map (kbd "M-j") 'company-complete-selection)
(define-key company-active-map (kbd "M-i") 'company-complete-common)
;; (define-key company-active-map (kbd "M-i") 'company-complete-common-or-cycle)
(define-key company-active-map [mouse-1] 'company-complete-mouse)
(define-key company-active-map [mouse-3] 'company-select-mouse)

;; help
(define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
(if (functionp 'company-quickhelp-manual-begin)
    ;; Default: (define-key company-quickhelp-mode-map (kbd "M-h") 'company-quickhelp-manual-begin)
    (define-key company-active-map (kbd "M-h") 'company-quickhelp-manual-begin) ; 'company-quickhelp--show
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  )

(define-key company-active-map (kbd "M-l") 'company-show-location)

;; search
;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "M-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-M-s") 'company-search-candidates)
(define-key company-search-map (kbd "C-g") 'company-search-abort)
(define-key company-search-map (kbd "M-s") 'company-search-repeat-forward)
(define-key company-search-map (kbd "M-r") 'company-search-repeat-backward)
(define-key company-search-map (kbd "M-n") 'company-search-repeat-forward)
(define-key company-search-map (kbd "M-p") 'company-search-repeat-backward)
(define-key company-search-map (kbd "M-o") 'company-search-kill-others)
(define-key company-search-map (kbd "M-j") 'company-complete-selection)

(defun company-new-line ()
  "insert a literal return new line."
  (interactive)
  ;; (company-abort)
  (company-cancel 'abort)
  (newline-and-indent)
  )

(define-key company-active-map [return] 'company-new-line)
(define-key company-active-map "\r" 'company-new-line)

(define-key company-active-map (kbd "SPC")
  '(lambda ()
     (interactive)
     (company-abort)
     (insert " ")))

;; some keybinding for company backends
(global-set-key (kbd "C-c /") 'company-files)

;; faces
;; tooltip
(set-face-attribute 'company-tooltip nil
                    :inverse-video nil
                    :foreground "black" :background "white"
                    :weight 'normal :slant 'normal
                    :underline nil)
;; selection
(set-face-attribute 'company-tooltip-selection nil
                    :inherit 'company-tooltip
                    :foreground "white" :background "#212121"
                    :underline nil)
(set-face-attribute 'company-tooltip-mouse nil
                    :inherit 'company-tooltip
                    :foreground "sky blue" :background "#333333"
                    :weight 'bold)
;; common
(set-face-attribute 'company-tooltip-common nil
                    :inherit 'company-tooltip
                    :foreground nil :background "light gray"
                    :underline nil)
;; common selection
(set-face-attribute 'company-tooltip-common-selection nil
                    :inverse-video nil
                    :inherit 'company-common
                    :foreground "cyan1" :background "#212121"
                    :underline nil)
;; search
(set-face-attribute 'company-tooltip-search nil
                    :inherit 'company-tooltip
                    :foreground "red" :background nil
                    :weight 'bold
                    :underline "#222222")
;; annotation
(set-face-attribute 'company-tooltip-annotation nil
                    :inherit 'company-tooltip
                    :foreground "#777777" :background nil
                    :slant 'italic)
;; scroll-bar
(set-face-attribute 'company-scrollbar-fg nil
                    :foreground "black" :background "black")
(set-face-attribute 'company-scrollbar-bg nil
                    :foreground "gray" :background "gray")
;; preview
(set-face-attribute 'company-preview nil
                    :foreground "dim gray" :background "black"
                    :weight 'normal)
(set-face-attribute 'company-preview-common nil
                    :inherit 'company-preview
                    :foreground "green yellow" :background "#444444")
(set-face-attribute 'company-preview-search nil
                    :inherit 'company-preview
                    :foreground "cyan" :background nil)
;; echo area
(set-face-attribute 'company-echo nil
                    :foreground "light blue" :background nil)
(set-face-attribute 'company-echo-common nil
                    :inherit 'company-echo
                    :foreground "cyan" :background nil)
;; template
;; TODO:
;; (set-face-attribute 'company-template-field nil
;;                     :foreground "orange" :background nil
;;                     :weight 'bold)


;; color quick hack
;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


;; TODO:
;; company-prefix is for "inline" displayed first matched candidate.
;; - company-preview
;; - company-preview-common
;; - company-preview-search
;; - company-echo
;; - company-echo-common

;;; Yasnippet integration
;;
;; - [C-h f company-yasnippet]

;; 1. * In a buffer-local value of `company-backends', grouped with a back-end or
;; several that provide actual text completions.
;;
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-dabbrev-code company-yasnippet)))))

;; 2. * After keyword `:with', grouped with other back-ends.
;;
;; (push '(company-semantic :with company-yasnippet) company-backends)

;; 3. * Not in `company-backends', just bound to a key.
;; 
;; (global-set-key (kbd "C-c y") 'company-yasnippet)

;;
;; 1. Company interferes with Yasnippet’s native behaviour. Here’s a quick fix: http://gist.github.com/265010

;; (define-key company-active-map "\t" 'company-yasnippet-or-completion)
;; (define-key company-active-map [tab] 'company-yasnippet-or-completion)
;; 
;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (if (yas--template-can-expand-p) ; FIXME: yasnippet internal function to check whether candidate expandable.
;;       (progn (company-abort)
;;              (yas-expand))
;;     (company-complete-common)))


;;; 2. Another code for solving conflicts in Company and Yasnippet.
;;; Another code for solving conflicts in Company and Yasnippet.
;;
;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))
;;
;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))
;;
;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))
;;
;; (global-set-key [tab] 'tab-indent-or-complete)



;; (setq company-dabbrev-downcase 'case-replace) ; disable the down-case feature of the dabbrev back-end?


;;; company-transformers -- Functions to change the list of candidates received from backends.

;;; example:
;; (push (apply-partially #'cl-remove-if
;;                        (lambda (c))
;;                        (or (string-match-p "[^\x00-\x7F]+" c)
;;                           (string-match-p "[0-9]+" c)
;;                           (if (equal major-mode "org")
;;                               (>= (length c) 15))))
;;       company-transformers)
;;
;; with this code:
;; - remove those non-ANSII candidates.
;; - remove any completion containing numbers.
;; - remove any candidate which is no longer than 15 in org-mode.

;; (setq company-transformers '(company-sort-by-backend-importance))


;;; [ company-statistics ] -- Sort completion candidates by previous completion choices.

;;; Company-statistics is a global minor mode built on top of the in-buffer
;;; completion system company-mode. The idea is to keep a log of a certain
;;; number of completions you choose, along with some context information, and
;;; use that to rank candidates the next time you have to choose — hopefully
;;; showing you likelier candidates at the top of the list.

;;; Design
;;;
;;; Company-statistics is an add-on for company-mode, but is only loosely
;;; coupled to it (it works by adding a sorting function to
;;; `company-transformers' as well as a handler to
;;; `company-completion-finished-hook'). It is designed with some flexibility in
;;; mind as for the recorded context information and the way candidates are
;;; scored: the default pair of functions are only examples! The stats are
;;; automatically persistent between sessions.

;;; Usage:

;; (require 'company-statistics)
;; (company-statistics-mode)
;;
;; ;; (add-hook 'after-init-hook 'company-statistics-mode)
;;
;; (setq company-statistics-auto-restore t
;;       company-statistics-auto-save t
;;       company-statistics-file "~/.emacs.d/.company-statistics-cache.el"
;;       ;; company-statistics-score-calc 'company-statistics-score-calc-default
;;       ;; company-statistics-score-change 'company-statistics-score-change-default
;;       company-statistics-size 500
;;       )


;;; company-elisp

(setq company-elisp-detect-function-context t ; offer Lisp functions only in appropriate contexts.
      company-elisp-show-locals-first t
      )


;;; company-dabbrev

(setq company-dabbrev-minimum-length 2)


;;; company-etags


;;; company-gtags

;; (setq company-gtags-modes '(prog-mode jde-mode))


;;; company-bbdb

(setq company-bbdb-modes '(message-mode
                           mu4e-compose-mode org-mu4e-compose-org-mode
                           org-mode))


(provide 'init-company-mode)

;;; init-company-mode.el ends here
