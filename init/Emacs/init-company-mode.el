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
;;;

(require 'company)

(setq-default company-minimum-prefix-length 3   ; minimum prefix character number for auto complete.
              company-idle-delay 0.5
              company-tooltip-align-annotations t ; align annotations to the right tooltip border.
              company-tooltip-limit 10          ; tooltip candidates max limit.
              company-tooltip-minimum 6         ; minimum candidates limit.
              company-tooltip-minimum-width 0   ; The minimum width of the tooltip's inner area.
                                        ; This doesn't include the margins and the scroll bar.
              company-tooltip-margin 1          ; width of margin columns to show around the tooltip
              company-tooltip-offset-display 'scrollbar ; 'lines - how to show tooltip unshown candidates number.
              company-show-numbers nil ; t: show quick-access numbers for the first ten candidates.
              company-selection-wrap-around t ; loop over candidates
              ;; company-async-wait 0.03
              ;; company-async-timeout 2
              )


;;; help document preview & popup
;; (require 'company-quickhelp)
;; (company-quickhelp-mode t)
;; (setq company-quickhelp--delay 0.1)

(setq-default company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                  company-preview-if-just-one-frontend
                                  company-echo-metadata-frontend
                                  ;; company-quickhelp-frontend
                                  ))


(setq-default company-backends '(company-elisp ; Emacs Lisp
                                 company-capf
                                 company-yasnippet
                                 company-semantic company-clang  company-cmake ; C/C++
                                 ;; company-eclim ; Java
                                 ;; company-ropemacs ; Python
                                 company-nxml company-css ; HTML, CSS, XML
                                 ;; company-xcode ; for Xcode projects
                                 company-bbdb ; BBDB
                                 (company-dabbrev-code company-yasnippet company-gtags company-etags company-keywords)
                                 company-files ; files & directory
                                 company-dabbrev company-abbrev ; abbrev
                                 ;; company-oddmuse ; wiki
                                 ;; company-ispell
                                 ))


;;; mode local backends example:
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-dabbrev-code company-yasnippet)))))

;; (setq company-begin-commands '(self-insert-command
;;                                org-self-insert-command orgtbl-self-insert-command
;;                                c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash
;;                                ))


;;; globally
(add-hook 'after-init-hook 'global-company-mode)
(diminish 'company-mode)
;;
;;; To use company-mode in all buffers, add the following line to your init file:
;; (unless (featurep 'auto-complete)
;;   (message "auto-complete isn't enabled, active company-mode instead.")
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (after 'global-company-mode
;;     (diminish 'company-mode))
;;   )
;;
;;; or enabled only in specific modes
;; (dolist (hook '(emacs-lisp-mode-hook
;;                 lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 scheme-mode-hook
;;                 clojure-mode-hook
;;                 ruby-mode-hook
;;                 enh-ruby-mode-hook
;;                 inf-ruby-mode-hook
;;                 python-mode-hook
;;                 sh-mode-hook
;;                 c-mode-hook
;;                 c++-mode-hook
;;                 java-mode-hook
;;                 asm-mode-hook
;;                 haskell-mode-hook
;;                 web-mode-hook
;;                 js2-mode-hook
;;                 js3-mode-hook
;;                 html-mode-hook
;;                 css-mode-hook
;;                 ))
;;   (add-hook hook 'company-mode))

(setq company-global-modes t)
;; (setq company-global-modes '(emacs-lisp-mode
;;                              org-mode
;;                              ruby-mode
;;                              prog-mode))

;; keybindings
;; (global-set-key (kbd "<tab>") 'company-complete)

;; manually start completion
;; (global-set-key (kbd "TAB") 'company-complete)

;; snippet
(define-key company-active-map [tab] 'yas-expand)

;; navigation
(defun company-new-line ()
  "insert a literal return new line."
  (interactive)
  (company-abort)
  (newline-and-indent)
  )

(define-key company-active-map "\t" nil)
(define-key company-active-map [tab] nil)
(define-key company-active-map (kbd "<tab>") nil)
(define-key company-active-map (kbd "<S-tab>") nil)
(define-key company-active-map [return] 'company-new-line)
(define-key company-active-map "\r" 'company-new-line)
(define-key company-active-map (kbd "C-n") nil)
(define-key company-active-map (kbd "C-p") nil)
(define-key company-active-map (kbd "C-j") nil)
(define-key company-active-map (kbd "C-g") '(lambda ()
                                              (interactive)
                                              (company-abort)))
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
(define-key company-active-map (kbd "M-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-M-s") 'company-search-candidates)
(define-key company-search-map (kbd "C-g") 'company-search-abort)
(define-key company-search-map (kbd "M-s") 'company-search-repeat-forward)
(define-key company-search-map (kbd "M-r") 'company-search-repeat-backward)
(define-key company-search-map (kbd "M-n") 'company-search-repeat-forward)
(define-key company-search-map (kbd "M-p") 'company-search-repeat-backward)
(define-key company-search-map (kbd "M-o") 'company-search-kill-others)
(define-key company-search-map (kbd "M-j") 'company-complete-selection)


(define-key company-active-map (kbd "SPC")
  '(lambda ()
     (interactive)
     (company-abort)
     (insert " ")))

(define-key company-active-map (kbd "C-g")
  '(lambda ()
     (interactive)
     (company-abort)))


;; faces
(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "white"
                    :weight 'normal :slant 'normal)
(set-face-attribute 'company-tooltip-selection nil
                    :inherit 'company-tooltip
                    :foreground "white" :background "#212121")
(set-face-attribute 'company-tooltip-mouse nil
                    :inherit 'company-tooltip
                    :foreground "cyan" :background "black"
                    :weight 'bold)
(set-face-attribute 'company-tooltip-common nil
                    :inherit 'company-tooltip
                    :foreground "dark gray")
(set-face-attribute 'company-tooltip-common-selection nil
                    :inherit 'company-tooltip-common
                    :inverse-video nil
                    :foreground "white" :background " ")
(set-face-attribute 'company-tooltip-search nil
                    :inherit 'company-tooltip
                    :foreground "red")
(set-face-attribute 'company-tooltip-annotation nil
                    :inherit 'company-tooltip
                    :foreground "dark red")
(set-face-attribute 'company-scrollbar-fg nil
                    :foreground "black" :background "black")
(set-face-attribute 'company-scrollbar-bg nil
                    :foreground " " :background "gray")
(set-face-attribute 'company-preview nil
                    :foreground "black" :background "dark gray"
                    )

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
;;; 1. Company interferes with Yasnippet’s native behaviour. Here’s a quick fix: http://gist.github.com/265010

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


(provide 'init-company-mode)

;;; init-company-mode.el ends here
