;;; init-my-emacs-edit.el --- init Emacs editing

;;; Commentary:

;;; Code:


(unless (boundp 'my-edit-prefix)
  (define-prefix-command 'my-edit-prefix))
(global-set-key (kbd "C-x e") 'my-edit-prefix)


;;;_ Edit

;; typed text replaces the active selection
(delete-selection-mode t)

;;;_ Rectangle

;;; - [C-x SPC] / [C-x r r m] (custom keybinding) :: `rectangle-mark-mode'



;;; [ 0xc ] -- Easy base conversion made easy in Emacs.

(use-package 0xc
  :ensure t
  :defer t)


(require 'init-my-emacs-kill-ring)
(require 'init-my-emacs-region)
(require 'init-my-emacs-macro)
(require 'init-my-emacs-edit-electric)
(require 'init-my-emacs-edit-narrow)
(require 'init-my-emacs-edit-tabulate)
(require 'init-my-emacs-edit-multiple-cursors)
(require 'init-my-emacs-edit-sudo)
(require 'init-my-emacs-edit-server)

(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
