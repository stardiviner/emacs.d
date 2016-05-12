;;; init-company-mode.el --- init company-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Company Mode ]

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 3
        ;; decrease this delay when you can type code continuously fast.
        company-idle-delay 0.2
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
        company-tooltip-flip-when-above nil
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

  ;; `company-mode' frontend showing the selection as if it had been inserted.
  ;; (add-to-list 'company-frontends 'company-preview-frontend)
  
  (setq company-backends
        '(company-files          ; files & directory
          ;; company-gtags company-etags
          company-keywords       ; keywords
          ;; company-tempo          ; tempo: flexible template insertion
          company-capf                   ; `completion-at-point-functions'
          :with
          company-yasnippet
          company-dabbrev-code         ; company-dabbrev
          company-abbrev
          )
        )

  (defun my-company-add-backend-locally (backend)
    "Add a backend in my custom way.

\(my-company-add-backend-locally 'company-robe\)
"
    (if (local-variable-if-set-p 'company-backends)
        (add-to-list 'company-backends `(,backend :with company-yasnippet))
      (add-to-list (make-local-variable 'company-backends)
                   `(,backend :with company-yasnippet))
      ))

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
  (global-set-key (kbd "M-<tab>") 'company-complete)
  (global-set-key (kbd "C-M-i") 'company-complete)

  ;; yasnippet
  ;; `yas-expand', `yas-expand-from-trigger-key'
  (define-key company-active-map [tab] 'yas-expand-from-trigger-key)

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
  (define-key company-active-map [mouse-1] 'company-complete-mouse)
  (define-key company-active-map [mouse-3] 'company-select-mouse)

  ;; help
  (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

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

  ;; [ company-yasnippet ]
  ;; make `company-yasnippet' work for prefix like `%link_to'.
  ;; (setq-default yas-key-syntaxes (list "w_" "w_." "w_.()"
  ;;                                      #'yas-try-key-from-whitespace))

  ;; [ company-abbrev / company-dabbrev ]
  (setq company-dabbrev-other-buffers t)

  ;; [ company-tempo ]
  (setq company-tempo-expand t)

  ;; [ company-etags ]
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

  ;; (company-quickhelp-mode 1)
  ;;
  ;; remove echo-area short doc display
  ;; (setq-default company-frontends
  ;;               (remq 'company-echo-metadata-frontend company-frontends))
  ;;
  (add-to-list 'company-frontends 'company-quickhelp-frontend)

  
  (if (functionp 'company-quickhelp-manual-begin)
      (define-key company-active-map (kbd "M-h") 'company-quickhelp-manual-begin)
    (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer))
  )


;;; [ company-statistics ]

;; (use-package company-statistics
;;   ;; :ensure t
;;   :config
;;   (setq company-statistics-auto-restore t
;;         company-statistics-auto-save t
;;         company-statistics-file "~/.emacs.d/.company-statistics-cache.el"
;;         ;; company-statistics-score-calc 'company-statistics-score-calc-default
;;         ;; company-statistics-score-change 'company-statistics-score-change-default
;;         company-statistics-size 500
;;         )
;;
;;   (company-statistics-mode)
;;   )


;;; [ company-mode in minibuffer `M-:' ]

(defun company-mode-minibuffer-setup ()
  "Setup company-mode in minibuffer."
  (company-mode 1)
  (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                  company-preview-if-just-one-frontend
                                  ))
  (setq-local company-tooltip-limit 4)
  (setq-local company-tooltip-minimum 1)
  )

(add-hook 'eval-expression-minibuffer-setup-hook 'company-mode-minibuffer-setup)


(provide 'init-company-mode)

;;; init-company-mode.el ends here
