;;; init-emacs-edit.el --- init Emacs editing

;;; Commentary:

;;; Code:
(unless (boundp 'editing-prefix)
  (define-prefix-command 'editing-prefix))
(global-set-key (kbd "C-x e") 'editing-prefix)

(require 'init-emacs-kill-ring)
(require 'init-emacs-register)
(require 'init-emacs-region)
(require 'init-emacs-typography)
(require 'init-emacs-edit-electric)
(require 'init-emacs-edit-rectangle)
(require 'init-emacs-edit-narrow)
(require 'init-emacs-edit-tabulate)
(require 'init-emacs-edit-multiple-cursors)
(require 'init-emacs-edit-indirect)
(require 'init-emacs-edit-sudo)
(require 'init-emacs-edit-server)
;; (require 'init-emacs-edit-collaborate)

(provide 'init-emacs-edit)

;;; init-emacs-edit.el ends here
