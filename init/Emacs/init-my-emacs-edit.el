;;; init-my-emacs-edit.el --- init Emacs editing

;;; Commentary:

;;; Code:

(unless (boundp 'my-edit-prefix)
  (define-prefix-command 'my-edit-prefix))
(global-set-key (kbd "C-x e") 'my-edit-prefix)


(require 'init-my-emacs-kill-ring)
(require 'init-my-emacs-region)
(require 'init-my-emacs-macro)
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
