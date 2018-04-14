;;; init-emacs-edit.el --- init Emacs editing

;;; Commentary:

;;; Code:

(unless (boundp 'editing-prefix)
  (define-prefix-command 'editing-prefix))
(global-set-key (kbd "C-x e") 'editing-prefix)

(require 'init-emacs-kill-ring)
(require 'init-emacs-register)
(require 'init-emacs-region)
(require 'init-emacs-edit-electric)
(require 'init-emacs-edit-rectangle)
(require 'init-emacs-edit-narrow)
(require 'init-emacs-edit-tabulate)
(require 'init-emacs-edit-multiple-cursors)
(require 'init-emacs-edit-indirect)
(require 'init-emacs-edit-sudo)
(require 'init-emacs-edit-server)

;;; [ clipboard ]

;; - select-enable-primary - default nil; set this to t if you want the Emacs commands C-w and C-y to use the primary selection.
;; - select-enable-clipboard - default t; set this to nil if you want the Emacs commands C-w and C-y to use the clipboard selection.
;; - Yes, you can have Emacs use both at the same time.
;; - `clipboard-yank'
;; - `clipboard-kill-ring-save'

(setq select-enable-clipboard t
      select-enable-primary t
      select-enable-clipboard-manager t
      )



(provide 'init-emacs-edit)

;;; init-emacs-edit.el ends here
