;;; init-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ completion ] -- *Completion* buffer

;;; Usage:
;;
;; - `completion-at-point-functions' is a special hook.
;;    add a completion command into it with mode locally.
;;    (add-hook 'completion-at-point-functions 'completion-function nil t)

;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library


(require 'init-helm)
(require 'init-ivy)



(provide 'init-emacs-completion)

;;; init-emacs-completion.el ends here
