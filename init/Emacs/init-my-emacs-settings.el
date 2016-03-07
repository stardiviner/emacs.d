;;; init-my-emacs-settings.el --- init my Emacs settings

;;; Commentary:

;;; Code:

;;; [ Disabled Commands ]

;; To enable all disabled commands in one fell swoop, put this in .emacs (not
;; recommended for newbies):
;;
;; NOTE: uncomment this when you are very familiar with Emacs.

;; (setq disabled-command-function nil)

;;; ----------------------------------------------------------
;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)     ; [C-x n n]
(put 'narrow-to-page 'disabled nil)       ; [C-x n p]
(put 'narrow-to-defun 'disabled nil)      ; [C-x n d]
(put 'upcase-region 'disabled nil)        ; [C-x C-u]
(put 'downcase-region 'disabled nil)      ; [C-x C-l]



(fset 'yes-or-no-p 'y-or-n-p) ; treat 'y' as yes, 'n' as no.

(setq inhibit-startup-message t) ; no splash screen

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") "!\n\n"))


;;; [ Time ]

;;; time-stamp
;; (add-hook 'before-save-hook 'time-stamp)
;; (setq time-stamp-pattern nil)


;; [ Bell ]

(setq visible-bell nil)                   ; use bell beep instead of flash frame.


;;; [ mode ]

;; (setq default-major-mode 'org-mode) ; use org-mode for any unspecified mode.


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

(unless (boundp 'my-register-map)
  (define-prefix-command 'my-register-map))
(global-set-key (kbd "C-x r x") 'my-register-map)

(define-key my-register-map (kbd "j") 'jump-to-register)
(define-key my-register-map (kbd "+") 'increment-register)
(define-key my-register-map (kbd "c") 'copy-to-register)
(define-key my-register-map (kbd "i") 'insert-register)
(define-key my-register-map (kbd "SPC") 'point-to-register)
(define-key my-register-map (kbd "C-SPC") 'point-to-register)
(define-key my-register-map (kbd "C-@") 'point-to-register)
(define-key my-register-map (kbd "p") 'point-to-register)
(define-key my-register-map (kbd "f") 'frameset-to-register)
(define-key my-register-map (kbd "w") 'window-configuration-to-register)


;;; [ clipboard ]

;; - x-select-enable-primary - default nil; set this to t if you want the Emacs commands C-w and C-y to use the primary selection.
;; - x-select-enable-clipboard - default t; set this to nil if you want the Emacs commands C-w and C-y to use the clipboard selection.
;; - Yes, you can have Emacs use both at the same time.
;; - `x-clipboard-yank'
;; - `clipboard-kill-ring-save'

(setq x-select-enable-clipboard t
      x-select-enable-clipboard-manager t
      x-select-enable-primary t
      )



(provide 'init-my-emacs-settings)

;;; init-my-emacs-settings.el ends here
