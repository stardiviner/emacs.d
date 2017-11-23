;;; init-my-emacs-edit.el --- init Emacs editing

;;; Commentary:

;;; Code:

(unless (boundp 'editing-prefix)
  (define-prefix-command 'editing-prefix))
(global-set-key (kbd "C-x e") 'editing-prefix)

(require 'init-my-emacs-kill-ring)
(require 'init-my-emacs-region)
(require 'init-my-emacs-edit-electric)
(require 'init-my-emacs-edit-rectangle)
(require 'init-my-emacs-edit-narrow)
(require 'init-my-emacs-edit-tabulate)
(require 'init-my-emacs-edit-multiple-cursors)
(require 'init-my-emacs-edit-indirect)
(require 'init-my-emacs-edit-sudo)
(require 'init-my-emacs-edit-server)



(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
