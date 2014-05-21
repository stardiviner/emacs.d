;;; init-my-prog-lang-R.el --- init R

;;; Commentary:


;;; Code:

;;; [ Emacs Speaks Statistics (ESS) ]
;; It is designed to support editing of scripts and interaction with various
;; statistical analysis programs such as R, S-Plus, SAS, Stata and JAGS.

;; (require 'ess)
(require 'ess-site)



;;; [ R-mode ]

(autoload 'R-mode "ess" "ESS" t)

;; automatically get the correct mode
(add-to-list 'auto-mode-alist
             '("\\.R$" . R-mode)
             '("\\.S$" . S-mode)
             '("\\.Rd\\" . Rd-mode) ; R documentation
             )
;; comment out the following if you are not using R/S-Plus on ACPUB
;; add a ";" in front of each line
;; (load "/usr/pkg/ess/lisp/ess-site")
;; (setq-default inferior-S+6-program-name "Splus")

(setq-default inferior-R-program-name "R") ;; /path/to/R

(setq ess-eval-visibly-p t)
;; (setq ess-ask-for-ess-directory t)


;;; [ ess-eldoc ]
(require 'ess-eldoc)


;;; [ auto-complete-acr ]

;;; auto-complete-mode extension for GNU R(statistics)
;; https://github.com/myuhe/auto-complete-acr.el






(provide 'init-my-prog-lang-R)

;;; init-my-prog-lang-R.el ends here
