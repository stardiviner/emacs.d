;;; init-company-mode.el --- init company-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Company Mode ]

(use-package company
  :ensure t
  :defer t
  ;; disable `company-mode' in `org-mode' for performance.
  :preface (setq company-global-modes '(not org-mode))
  :commands (global-company-mode)
  :init (global-company-mode 1)
  :config
  (setq company-minimum-prefix-length 3
        ;; decrease this delay when you can type code continuously fast.
        company-idle-delay .1
        company-tooltip-idle-delay 0.1
        ;; determine which characters trigger auto-completion the selected candidate.
        company-auto-complete nil ; nil: don't auto select the first candidate when input `company-auto-complete-chars'.
        ;; '(?_ ?\( ?w ?. ?\" ?$ ?\' ?/ ?| ?! ?#)
        company-auto-complete-chars '(?\  ?\) ?. ?#)
        ;; company-require-match 'company-explicit-action-p ; 'never
        company-tooltip-align-annotations t ; align annotations to the right tooltip border.
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10 ; tooltip candidates max limit
        company-tooltip-minimum 3 ; minimum candidates height limit
        ;; company-tooltip-minimum-width 0 ; the minimum width of the tooltip's inner area
        company-tooltip-margin 1 ; width of margin columns to show around the tooltip
        company-selection-wrap-around t ; loop over candidates
        company-search-regexp-function #'company-search-flex-regexp
        )

  ;; `company-mode' frontend showing the selection as if it had been inserted.
  ;; (add-to-list 'company-frontends 'company-preview-frontend)

  (setq-default company-backends
                '(company-files         ; files & directory
                  ;; company-gtags company-etags
                  (company-capf         ; `completion-at-point-functions'
                   :with company-yasnippet
                   :with company-tempo  ; tempo: flexible template insertion
                   :with company-dabbrev-code
                   ;; :separate company-semantic
                   ;; :separate company-ispell ; for word completion in comment.
                   )
                  (company-keywords
                   :with company-abbrev)
                  )
                )
  
  (defun my-company-add-backend-locally (backend)
    "Add a backend in my custom way.

\\(my-company-add-backend-locally 'company-robe\\)"
    (if (local-variable-if-set-p 'company-backends)
        (add-to-list 'company-backends `(,backend :with company-yasnippet))
      (add-to-list (make-local-variable 'company-backends)
                   `(,backend :with company-yasnippet))
      ))

  ;; keybindings

  ;; manually start completion (don't globally set, conflict with auto-complete
  ;; in some modes which use auto-complete)
  ;;
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
  ;; nested searching map.
  (define-key company-search-map (kbd "M-g") 'company-search-toggle-filtering)
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

  (defun company-space ()
    "insert a literal return new line."
    (interactive)
    ;; (company-abort)
    (company-cancel 'abort)
    (insert " ")
    )
  
  (define-key company-active-map (kbd "SPC") 'company-space)

  ;; [ company-yasnippet ]
  ;; make `company-yasnippet' work for prefix like `%link_to'.
  ;; (setq-default yas-key-syntaxes (list "w_" "w_." "w_.()"
  ;;                                      #'yas-try-key-from-whitespace))

  ;; [ company-abbrev / company-dabbrev ]
  (setq company-dabbrev-other-buffers t)

  ;; [ company-tempo ]

  ;; [ company-etags ]
  ;; enable to offer completions in comment and strings.
  ;; (setq company-etags-everywhere t)

  ;; [ company-transformers ]
  ;; - `company-sort-prefer-same-case-prefix'
  ;; - `company-sort-by-backend-importance'
  ;; - `company-sort-by-occurrence'
  (setq company-transformers '(company-sort-prefer-same-case-prefix
                               company-sort-by-backend-importance
                               company-sort-by-occurrence))

  ;; animation effect on company completion
  ;; - `beacon-blink', `beacon--shine'
  (defun my-company-start-animation (backend)
    ;; beacon
    (let ((beacon-size 20)
          (beacon-color "cyan"))
      (beacon-blink)))
  (defun my-company-success-animation (backend)
    ;; beacon
    (let ((beacon-size 20)
          (beacon-color "green"))
      (beacon-blink)))
  (defun my-company-fail-animation (backend)
    ;; beacon
    (let ((beacon-size 20)
          (beacon-color "dark red"))
      (beacon-blink)))
  ;; (add-hook 'company-completion-started-hook #'my-company-start-animation)
  (add-hook 'company-completion-finished-hook #'my-company-success-animation)
  (add-hook 'company-completion-cancelled-hook #'my-company-fail-animation)
  )



;;; [ company-childframe ] -- use a child-frame as company candidate menu.

;; (use-package company-childframe
;;   :ensure t
;;   :config
;;   (company-childframe-mode 1)
;;   ;; (add-to-list 'company-frontends 'company-childframe-frontend)
;;   ;; fix `desktop-save-mode' record and enable `company-childframe' on all buffers.
;;   (require 'desktop)
;;   (push '(company-childframe-mode . nil) desktop-minor-mode-table)
;;   )

;;; [ company-quickhelp ] -- quick help document preview & popup

;; (use-package company-quickhelp
;;   :ensure t
;;   :config
;;   (setq company-quickhelp-use-propertized-text t)
;;
;;   ;; automatic
;;   ;; (company-quickhelp-mode 1)
;;   ;;
;;   ;; remove echo-area short doc display
;;   ;; (setq-default company-frontends
;;   ;;               (remq 'company-echo-metadata-frontend company-frontends))
;;   ;;
;;   ;; (add-to-list 'company-frontends 'company-preview-common-frontend) ; NOTE: this caused company-mode tooltip offset.
;;
;;   ;; manually
;;   (setq company-quickhelp-delay nil) ; set to `nil' to trigger popup doc manually.
;;   ;; (add-to-list 'company-frontends 'company-quickhelp-frontend)
;;   (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
;;   (with-eval-after-load 'company-quickhelp
;;     (define-key company-active-map (kbd "M-h") 'company-quickhelp-manual-begin))
;;   )

;;; [ company-mode in minibuffer `M-:' ]

(defun company-mode-minibuffer-setup ()
  "Setup company-mode in minibuffer."
  (company-mode 1)
  (company-box-mode -1)
  (setq-local company-frontends '(company-pseudo-tooltip-frontend))
  (setq-local company-tooltip-limit 4)
  (setq-local company-tooltip-minimum 1)
  )

(add-hook 'eval-expression-minibuffer-setup-hook 'company-mode-minibuffer-setup)

;;; [ company-box ] -- A company front-end with icons.

(use-package company-box
  :ensure t
  :ensure all-the-icons
  :hook (company-mode . company-box-mode)
  :init (require 'all-the-icons)
  :config
  (setq company-box-doc-delay 0.3)
  (define-key company-active-map (kbd "M-h") nil)
  (define-key company-box-mode-map (kbd "M-h") 'company-box-doc)

  (defun my:company-box-faces-setup (theme)
    "Reload company-box faces on `circadian' `THEME' toggling."
    (set-face-attribute 'company-box-candidate nil
                        :inherit nil
                        :family (face-attribute 'default :family)
                        :foreground (face-foreground 'default))
    (set-face-attribute 'company-box-selection nil
                        :inherit 'company-tooltip-selection)
    (set-face-attribute 'company-box-background nil
                        :background (face-background 'company-tooltip)))
  (add-hook 'circadian-after-load-theme-hook #'my:company-box-faces-setup)

  (setq company-box-backends-colors
        '((company-capf . (:icon "LightSeaGreen"))
          (company-keywords . (:all "tomato"))
          (company-files . (:all "CornflowerBlue"))
          (company-yasnippet . (:icon "#7C4Dff"
                                      :candidate "purple" :annotation "gray"
                                      :selected (:background "purple" :foreground "white")))
          (company-tempo . (:all "chocolate"))
          (company-dabbrev . (:all "khaki"))
          (company-dabbrev-code . (:all "dark khaki"))
          ;; extra backends
          (company-elisp . (:icon "firebrick"))
          (sly-company . (:icon "RoyalBlue"))
          (company-slime . (:icon "RoyalBlue"))
          (geiser-company-backend . (:icon "SlateBlue"))
          (elpy-company-backend . (:icon "orange"))
          (company-robe . (:icon "red1"))
          (company-c-headers . (:icon "DarkGoldenrod"))
          (company-irony . (:icon "DodgerBlue"))
          (company-irony-c-headers . (:icon "DarkGoldenrod"))
          (company-go . (:icon "SandyBrown"))
          (company-racer . (:icon "SteelBlue"))
          (company-tern . (:icon "yellow3"))
          (company-lua . (:icon "LightBlue"))
          (company-edbi . (:icon "DarkGreen"))
          (company-restclient . (:icon "DarkTurquoise"))
          ))

  (setq company-box-icons-unknown (all-the-icons-faicon "code" :height 0.9 :v-adjust -0.05))
  (setq company-box-icons-yasnippet (all-the-icons-faicon "file-code-o" :height 0.8 :v-adjust -0.05))
  (setq company-box-icons-elisp (list
                                 ;; "λ" ; function/method
                                 (all-the-icons-material "functions" :v-adjust -0.15 :height 0.9)
                                 ;; (all-the-icons-faicon "hashtag")
                                 "v" ; variable
                                 (all-the-icons-octicon "package" :height 0.9 :v-adjust -0.05) ; library
                                 (all-the-icons-faicon "font" :height 0.8 :v-adjust -0.05) ; face
                                 ))
  )


(provide 'init-company-mode)

;;; init-company-mode.el ends here
