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

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 2
        ;; decrease this delay when you can type code continuously fast.
        company-idle-delay 0.4
        ;; determines when to auto-complete.
        ;; 'company-explicit-action-p, t, nil, 'function
        ;; company-auto-complete nil
        ;; company-auto-complete-chars
        ;; 'company-explicit-action-p, t, nil, 'function
        ;; company-require-match 'company-explicit-action-p
        company-echo-delay .01
        ;; t: show quick-access numbers for the first ten candidates.
        ;; company-show-numbers nil
        ;; align annotations to the right tooltip border.
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; tooltip candidates max limit.
        company-tooltip-limit 10
        ;; minimum candidates height limit.
        company-tooltip-minimum 3
        ;; The minimum width of the tooltip's inner area.
        ;; company-tooltip-minimum-width 0
        ;; This doesn't include the margins and the scroll bar.
        ;; width of margin columns to show around the tooltip
        company-tooltip-margin 1
        ;; 'lines - how to show tooltip unshown candidates number.
        ;; company-tooltip-offset-display 'scrollbar
        ;; loop over candidates
        company-selection-wrap-around t
        company-search-regexp-function #'company-search-flex-regexp
        )

  (setq company-tempo-expand t)
  
  (setq company-semantic-begin-after-member-access t
        company-semantic-insert-arguments t)
  
  ;; for completion.el
  ;; TODO: test this.
  ;; (add-to-list 'company-begin-commands 'completion-separator-self-insert-command)

  
  ;; Only one back-end is used at a time.  The choice depends on the order of
  ;; the items in this list, and on the values they return in response to the
  ;; `prefix' command (see below).  But a back-end can also be a \"grouped\"
  ;; one (see below).
  ;;
  ;; - `:with' :: e.g. (company-ghc :with company-dabbrev)
  ;;
  ;; It affects which backend gets selected for completion: the group in question,
  ;; or some after it. If you have a group (company-ghc company-dabbrev-code) in,
  ;; say, a ruby-mode buffer, it will be used because company-dabbrev-code doesn't
  ;; care about the major mode. If the group was (company-ghc :with
  ;; company-dabbrev-code), though, it would be skipped because company-ghc would
  ;; always return nil to prefix in that major mode.

  (setq-default company-backends
                '((company-files          ; files & directory
                   ;; company-gtags company-etags
                   company-keywords       ; keywords
                   ;; company-tempo          ; tempo: flexible template insertion
                   company-capf                   ; `completion-at-point-functions'
                   ;; :with
                   company-yasnippet
                   company-dabbrev-code
                   company-abbrev ; company-dabbrev
                   )
                  )
                )

  (defun my-company-add-backends-to-mode (backends-list)
    "Add a list of backends to mode local. integrate with default `company-backends'."
    ;; make `company-backends' local.
    (make-local-variable 'company-backends)
    ;; TODO: remove duplicate
    ;; (remove-if (lambda (backend) (find backend backends-list)) company-backends)
    (setq company-backends
          (remove-if (lambda (b)
                       (find b backends-list))
                     company-backends))
    ;; copy backends list
    (setq company-backends (copy-tree company-backends))
    ;; adding
    (setf (car company-backends)
          (append backends-list
                  (car company-backends)))
    )

  ;; (defun my-company-add-backends-to-mode (backends-list)
  ;;   (make-local-variable 'company-backends)
  ;;   (if (listp (car company-backends))
  ;;       (setq-local company-backends
  ;;                   (setf (car company-backends)
  ;;                         (-union backends-list (car company-backends))))
  ;;     (setq-local company-backends
  ;;                 (-union backends-list company-backends)))
  ;;   )

  ;; remove company backend from `company-backends'.
  ;; (setq company-backends
  ;;       (cons 'company-emacs-eclim
  ;;             (remove-if (lambda (b) (find b '(company-nxml company-eclim)))
  ;;                        company-backends)))

  ;; globally
  (setq company-global-modes t)

  (global-company-mode 1)
  ;; (add-hook 'after-init-hook 'global-company-mode)

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
      (progn
        ;; 'company-quickhelp--show
        ;; (define-key company-quickhelp-mode-map (kbd "M-h") 'company-quickhelp-manual-begin)
        (define-key company-active-map (kbd "M-h") 'company-quickhelp-manual-begin)
        )
    (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
    )

  (define-key company-active-map (kbd "M-l") 'company-show-location)

  ;; search
  ;; (setq company-search-regexp-function #regexp-quote)
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
  ;; (global-set-key (kbd "C-c /") 'company-files)

  ;; faces
  ;; tooltip
  (set-face-attribute 'company-tooltip nil
                      :inverse-video nil
                      :foreground "black"
                      :background "white"
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
                      :foreground "dim gray" :background "light gray"
                      :underline nil)
  ;; common selection
  (set-face-attribute 'company-tooltip-common-selection nil
                      :inverse-video nil
                      :inherit 'company-common
                      :foreground "cyan" :background "#212121"
                      :underline nil)
  ;; search
  (set-face-attribute 'company-tooltip-search nil
                      :inherit 'company-tooltip
                      :foreground "white" :background "deep pink")
  ;; annotation
  (set-face-attribute 'company-tooltip-annotation nil
                      :inherit 'company-tooltip
                      :foreground "#777777" :background "white")
  (set-face-attribute 'company-tooltip-annotation-selection nil
                      :inherit 'company-tooltip-selection
                      :foreground "orange")
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
                      :foreground "cyan")
  ;; echo area
  (set-face-attribute 'company-echo nil
                      :foreground "light blue")
  (set-face-attribute 'company-echo-common nil
                      :inherit 'company-echo
                      :foreground "cyan")
  ;; template
  ;; FIXME:
  ;; (set-face-attribute 'company-template-field nil
  ;;                     :foreground "orange"
  ;;                     :weight 'bold)

  ;; [ company-yasnippet ]
  ;;
  ;; TODO: remove this after upstream `company-mode' add this feature.
  ;; use `yas-key-syntaxes-v2' branch of `company-yasnippet'.
  ;; (load-file "~/Code/Emacs/company-mode/company-yasnippet.el")
  ;; (load-file "~/.emacs.d/init/extensions/company-yasnippet.el")
  ;;
  ;; make `company-yasnippet' work for prefix like `%link_to'.
  ;; (setq-default yas-key-syntaxes (list "w_" "w_." "w_.()"
  ;;                                      #'yas-try-key-from-whitespace))

  ;; [ company-abbrev / company-dabbrev ]

  (setq company-dabbrev-other-buffers t)

  ;; [ company-tempo ]
  (setq company-tempo-expand t)

  ;; [ company-etags ]

  (require 'company-etags)

  ;; enable to offer completions in comment and strings.
  ;; (setq company-etags-everywhere t)

  ;; [ company-transformers ]
  ;; (setq company-transformers '(company-sort-by-backend-importance))
  )


;;; [ company-quickhelp ] -- quick help document preview & popup

(use-package company-quickhelp
  :ensure t
  :config
  ;; set to `nil' to trigger popup doc manually.
  (setq company-quickhelp-delay nil)

  (company-quickhelp-mode 1)

  ;; remove echo-area short doc display
  ;; (setq-default company-frontends
  ;;               (remq 'company-echo-metadata-frontend company-frontends))

  (add-to-list 'company-frontends 'company-quickhelp-frontend)

  ;; (add-to-list 'company-frontends 'company-preview-frontend)
  )


;;; [ company-statistics ]

(use-package company-statistics
  :init
  ;; (add-hook 'after-init-hook 'company-statistics-mode)
  :config
  (setq company-statistics-auto-restore t
        company-statistics-auto-save t
        company-statistics-file "~/.emacs.d/.company-statistics-cache.el"
        ;; company-statistics-score-calc 'company-statistics-score-calc-default
        ;; company-statistics-score-change 'company-statistics-score-change-default
        company-statistics-size 500
        )

  (company-statistics-mode)
  )


;;; [ company-try-hard ] -- get all completions from company backends.

;; Offer completions from the first backend in `company-backends' that offers
;; candidates. If called again, use the next backend, and so on.

;; (use-package company-try-hard
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "<tab>") 'company-try-hard)
;;   )


;;; [ company-dict ] -- A backend that emulates ac-source-dictionary.

(use-package company-dict
  :ensure t
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  (add-to-list 'company-backends 'company-dict)
  )


(provide 'init-company-mode)

;;; init-company-mode.el ends here
