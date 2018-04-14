;;; init-emacs-register.el --- init for Emacs registers.

;;; Commentary:



;;; Code:

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



(provide 'init-emacs-register)

;;; init-emacs-register.el ends here
