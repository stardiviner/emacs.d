;;; init-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ completion ] -- *Completion* buffer

;;; Usage:
;;
;; - `completion-at-point-functions' is a special hook.
;;    add a completion command into it with mode locally.
;;    (add-hook 'completion-at-point-functions 'completion-function nil t)

;;; [ compdef ] -- A stupid Emacs completion definer for `pcomplete' and `company-mode'.

(use-package compdef
  :ensure t
  :defer t
  :init (compdef
         :modes #'org-mode
         :company '(company-files company-yasnippet company-dabbrev company-capf)
         :capf #'pcomplete-completions-at-point))



(provide 'init-emacs-completion)

;;; init-emacs-completion.el ends here
