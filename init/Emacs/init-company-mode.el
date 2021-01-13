;;; init-company-mode.el --- init company-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Company Mode ]

(use-package company
  :ensure t
  :ensure company-emoji
  :defer t
  :delight company-mode
  ;; disable initialize loading all company backends.
  :preface (setq company-backends nil)
  :commands (global-company-mode)
  :custom ((company-etags-modes nil)    ; disable `company-etags'
           (company-minimum-prefix-length 3)
           (completion-ignore-case t) ; complete candidates ignore case-sensitive when typing.
           (company-idle-delay 0.3)
           (company-tooltip-idle-delay 0.3)
           (company-echo-delay 0)
           ;; decide when to auto commit insertion of the selected completion candidate.
           ;; (company-auto-commit t)
           ;; (company-auto-commit-chars '(?\  ?\) ?.))
           (company-selection-wrap-around t) ; loop over candidates
           (company-tooltip-align-annotations t) ; align annotations to the right tooltip border.
           (company-show-numbers nil)
           (company-search-regexp-function #'company-search-flex-regexp)
           (company-frontends '(company-pseudo-tooltip-frontend
                                ;; company-preview-if-just-one-frontend
                                company-echo-metadata-frontend
                                company-preview-common-frontend))
           (company-backends '((company-capf ; `completion-at-point-functions'
                                :separate company-yasnippet
                                ;; :separate company-tempo  ; tempo: flexible template insertion
                                :separate company-keywords
                                :separate company-emoji)
                               (company-dabbrev-code :with company-abbrev)
                               company-files))
           (company-dabbrev-other-buffers t) ; only from same major-mode
           )
  :hook (after-init . global-company-mode)
  :init (add-to-list 'display-buffer-alist '("^\\*company-documentation\\*" . (display-buffer-below-selected)))
  :config  
  (defun my-company-add-backend-locally (backend)
    "Add a backend in my custom way.

(add-hook 'major-mode-hook
            #'(lambda (my-company-add-backend-locally 'company-backend)))"
    (unless (eq (if (listp (car company-backends))
                    (caar company-backends)
                  (car company-backends))
                backend)
      (setq-local company-backends
                  (cons
                   (list backend :with (if (listp (car company-backends))
                                           (caar company-backends)
                                         (car company-backends)))
                   (cdr company-backends)))))

  ;; [ company-ispell ]
  ;; hide `company-ispell' echo message "Starting 'look' process".
  (use-package shut-up
    :ensure t
    :init
    (defun ispell-silent (orig-func &rest args)
      "Silent ispell lookup words message."
      (shut-up (apply orig-func args)))
    (advice-add 'ispell-lookup-words :around 'ispell-silent))
  
  ;; [ keybindings ]

  ;; (global-set-key (kbd "<tab>") 'company-indent-or-complete-common)
  ;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
  ;; (define-key company-mode-map (kbd "TAB") 'company-complete)
  ;; (define-key company-mode-map [tab] 'company-complete)
  (define-key company-mode-map (kbd "C-M-i") 'company-complete)
  (define-key company-mode-map (kbd "M-<tab>") 'company-complete)

  ;; yasnippet
  ;; `yas-expand', `yas-expand-from-trigger-key'
  (define-key company-active-map [tab] 'yas-expand-from-trigger-key)

  ;; navigation
  (define-key company-active-map "\t" nil)
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "<S-tab>") nil)
  (define-key company-active-map (kbd "M-j") 'company-complete-selection)
  (define-key company-active-map (kbd "M-i") 'company-complete-common)
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-l") 'company-show-location)
  ;; (setq company-search-regexp-function #regexp-quote)
  (define-key company-active-map (kbd "M-s") 'company-search-candidates)
  (define-key company-active-map (kbd "C-M-s") 'company-filter-candidates)
  ;; nested searching map.
  (define-key company-search-map (kbd "C-g") 'company-search-abort)
  (define-key company-search-map (kbd "M-g") 'company-search-toggle-filtering)
  (define-key company-search-map (kbd "M-s") 'company-search-repeat-forward)
  (define-key company-search-map (kbd "M-r") 'company-search-repeat-backward)
  (define-key company-search-map (kbd "M-n") 'company-search-repeat-forward)
  (define-key company-search-map (kbd "M-p") 'company-search-repeat-backward)
  (define-key company-search-map (kbd "M-o") 'company-search-kill-others)
  (define-key company-search-map (kbd "M-j") 'company-complete-selection)

  ;; [ company-yasnippet ]
  ;; make `company-yasnippet' work for prefix like `%link_to'.
  ;; (setq-default yas-key-syntaxes (list "w_" "w_." "w_.()"
  ;;                                      #'yas-try-key-from-whitespace))

  ;; [ company-transformers ]
  ;; NOTE: disable customize `company-transformers' to fix python-mode company candidates
  ;; sorting.
  ;; (setq company-transformers '(company-sort-by-backend-importance
  ;;                              company-sort-prefer-same-case-prefix
  ;;                              ;; company-sort-by-occurrence
  ;;                              ))
  
  ;; [ company-mode in minibuffer `M-:' ]
  (defun company-mode-minibuffer-setup ()
    "Setup company-mode in minibuffer."
    (company-mode 1)
    (setq-local company-tooltip-limit 4)
    (setq-local company-tooltip-minimum 1))
  (add-hook 'eval-expression-minibuffer-setup-hook 'company-mode-minibuffer-setup))

;;; [ company-box ] -- A company front-end with icons.

(use-package company-box
  :ensure t
  :delight company-box-mode
  :hook (company-mode . company-box-mode)
  :custom (;; (company-idle-delay 0.5) ; increase delay to avoid fast input slow down company speed.
           (company-box-doc-delay 0.5)
           ;; (company-box-doc-enable nil) ; disable auto `company-box-doc' timer.
           ;; (company-box-icons-alist 'company-box-icons-all-the-icons)
           )
  ;; :config (add-to-list 'company-box-frame-parameters '(font . "DejaVu Sans Mono-10"))
  )

;;; [ company-prescient ] -- prescient.el + company.

(use-package company-prescient
  :ensure t
  :config (company-prescient-mode 1))

;; [ company-tabnine ] -- A company-mode backend for TabNine, the all-language autocompleter.

(use-package company-tabnine
  :ensure t
  :defer t
  :config
  ;; The free version of TabNine is good enough,
  ;; and below code is recommended that TabNine not always
  ;; prompt me to purchase a paid version in a large project.
  (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
    (let ((company-message-func (ad-get-arg 0)))
      (when (and company-message-func
                 (stringp (funcall company-message-func)))
        (unless (string-match "The free version of TabNine only indexes up to"
                              (funcall company-message-func))
          ad-do-it))))

  ;; only enable `company-tabnine' in some modes.
  ;; (defun company-tabnine-enable ()
  ;;   (make-local-variable 'company-backends)
  ;;   (add-to-list 'company-backends 'company-tabnine))
  ;; (dolist (hook '(c-mode-hook c++-mode-hook java-mode-hook))
  ;;   (add-hook hook #'company-tabnine-enable))
  )


(provide 'init-company-mode)

;;; init-company-mode.el ends here
