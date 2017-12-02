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


;;; [ register ]

(global-unset-key (kbd "C-x r j"))
(global-unset-key (kbd "C-x r +"))
(global-unset-key (kbd "C-x r c"))
(global-unset-key (kbd "C-x r i"))
(global-unset-key (kbd "C-x r u"))
(global-unset-key (kbd "C-x r U"))
(global-unset-key (kbd "C-x r s"))
(global-unset-key (kbd "C-x r b"))
(global-unset-key (kbd "C-x r f"))
(global-unset-key (kbd "C-x r g"))
(global-unset-key (kbd "C-x r w"))
(global-unset-key (kbd "C-x r f"))
(global-unset-key (kbd "C-x r SPC"))
(global-unset-key (kbd "C-x r C-SPC"))
(global-unset-key (kbd "C-x r C-@"))

(unless (boundp 'register-prefix)
  (define-prefix-command 'register-prefix))
(global-set-key (kbd "C-x r x") 'register-prefix)

(define-key register-prefix (kbd "j") 'jump-to-register)
(define-key register-prefix (kbd "+") 'increment-register)
(define-key register-prefix (kbd "c") 'copy-to-register)
(define-key register-prefix (kbd "i") 'insert-register)
(define-key register-prefix (kbd "SPC") 'point-to-register)
(define-key register-prefix (kbd "C-SPC") 'point-to-register)
(define-key register-prefix (kbd "C-@") 'point-to-register)
(define-key register-prefix (kbd "p") 'point-to-register)
(define-key register-prefix (kbd "f") 'frameset-to-register)
(define-key register-prefix (kbd "w") 'window-configuration-to-register)


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



(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
