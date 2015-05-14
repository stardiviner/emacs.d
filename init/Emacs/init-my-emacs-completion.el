;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ complete ]

;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
;; (setq completion-cycle-threshold 5)
;; (setq completion-at-point-functions '(;; semantic-completion-at-point-function ; NOTE: require `semantic'.
;;                                       elisp-completion-at-point)
;;       ;; completion-styles
;;       )


;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(load-library "pcomplete")


;;; [ Icomplete (icomplete/ido/iswitchb) ] -- enhance the default minibuffer completion.

;;; Usage:
;;
;; - [M-x icomplete-mode] :: toggle `icomplete-mode'.
;; - See `icomplete-completions' docstring for a description of the icomplete
;;   display format.


;; (require 'icomplete)
;;
;; ;; (add-hook 'icomplete-minibuffer-setup-hook
;; ;;           ())
;;
;; (icomplete-mode 1)
;;
;; (setq icomplete-compute-delay 0.3
;;       icomplete-max-delay-chars 2
;;       icomplete-in-buffer nil ; also use Icomplete when completing in non-mini buffers.
;;       icomplete-separator " | "
;;       icomplete-hide-common-prefix t
;;       icomplete-show-matches-on-no-input nil
;;       )
;;
;; (set-face-attribute 'icomplete-first-match nil
;;                     :weight 'bold)


;;; [ Hippie ] --- hippie complete.

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-all-abbrevs
;;         try-expand-list
;;         try-expand-line
;;         try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))


;;; Press [TAB] in minibuffer to show completions in popup window buffer.




(require 'init-helm)
;; (require 'init-ido)

(require 'init-company-mode)
;; (require 'init-auto-complete)

;;; make auto-complete work with company-mode
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (company-mode -1)
;;               (auto-complete-mode 1)
;;               )))



(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
