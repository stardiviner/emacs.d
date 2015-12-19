;;; init-my-emacs-settings.el --- init my Emacs settings

;;; Commentary:

;;; Code:

;;; [ Disabled Commands ]
;; To enable all disabled commands in one fell swoop, put this in .emacs (not recommended for newbies):
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


;;; [ concurrent in Emacs (JIT) ]

(setq jit-lock-stealth-verbose t)


;;; [ Time ]
(setq system-time-locale "C") ; make timestamps in org-mode appear in English.
;;; time-stamp
;; (add-hook 'before-save-hook 'time-stamp)
;; (setq time-stamp-pattern nil)


;; [ Bell ]
(setq visible-bell nil)                   ; use bell beep instead of flash frame.


;;; [ User Information ]
(setq user-full-name "stardiviner")
(setq user-mail-address "numbchild@gmail.com")
;; (setq user-login-name "stardiviner")


;;; [ mode ]
;; (setq default-major-mode 'org-mode) ; use org-mode for any unspecified mode.


;;; [ session ]

;;; save-desktop
;;; save-place


;;; [ auto save ]
;; (auto-save-mode t)
(setq auto-save-default t               ; create #autosave# files
      auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-"
      auto-save-interval 1500)


;; places
(require 'saveplace)
(setq save-place t                      ; save point place
      save-place-file "~/.emacs.d/.emacs-places")


;;; [ backup ]
(setq make-backup-files t
      backup-by-copying t
      backup-by-copying-when-mismatch t
      backup-by-copying-when-privileged-mismatch t
      backup-by-copying-when-linked t
      version-control t ; use versioned backups.
      vc-make-backup-files nil ; do not backup files in vc.
      ;; backup-inhibited ; do not generate backup
      delete-old-versions t             ; auto delete old versions.
      kept-new-versions 3               ; number of new versions.
      kept-old-versions 3               ; number of old versions.
      version-control t                 ; multiple versions backup.
      )

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/.backups")))
      ;; `((".*" . ,temporary-file-directory)) ; put all under directory /tmp.
      )


;;; [ password ]

(setq password-cache-expiry nil) ; don't expire password cache.


;;; [ register ]

;; Usage:
;;
;; Default keybinds
;; - [C-x r] -- `r' means register prefix.
;;   - [C-x r SPC] -- `SPC' means mark
;;     - [C-x r SPC + register_name] -- save current position to register
;;   - [C-x r j] -- `j' means jump
;;     - [C-x r j + register_name] -- jump to register position
;;   - [C-x r s] -- (copy-to-register)
;;   - [C-x r i] -- (insert-register)
;;   - [C-x r r] -- (copy-rectangle-to-register)
;; - [M-x view-register] -- check out a register
;; - [M-x list-registers] -- list out all registers
;; - [M-x append-to-register RET r]
;; - [M-x prepend-to-register RET r]

(unless (boundp 'my-register-map)
  (define-prefix-command 'my-register-map))
(global-set-key (kbd "C-x r x") 'my-register-map)

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




;;; [ macro ]
;; file to save macros:
;; ~/.emacs.d/macros/macros
(load-file "~/.emacs.d/init/macros/macros")



;;; [ movement ]
;; set sentence-end to recognize chinese punctuation.
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; (setq sentence-end-double-space nil)

(setq track-eol t) ; always track end of line when moving at end of line.


;;; mouse
;;; disable all mouse click events
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

(mouse-avoidance-mode 'animation)

;; make pointer invisible while typing.
;; The pointer becomes visible again when the mouse is moved.
(setq make-pointer-invisible t)


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


;;; [ Browser ]

(setq browse-url-browser-function 'browse-url-generic ; 'browse-url-generic, 'browse-url-default-browser, 'eww-browse-url (EWW)
      ;; "conkeror" "firefox", "chromium-browser", "uzbl-tabbed", "luakit", "jumanji", "elinks",
      browse-url-generic-program (executable-find "firefox")
      )


;;; [ Search ]
(setq-default  case-fold-search t ; whether toggle case sensitive search depend on your input has mixture of upcase and downcase.
               case-replace t ; preserve case in replacements.
               )


;;; [ guru-mode ] --- Learn to use Emacs the way it was meant to be used (the Emacs guru way)

;;; Guru mode disables some common keybindings and suggests the use of the
;;; established Emacs alternatives instead.

;; (require 'guru-mode)

;; (guru-global-mode +1)
;; ;; (add-hook 'prog-mode-hook 'turn-on-guru-mode)




(provide 'init-my-emacs-settings)

;;; init-my-emacs-settings.el ends here
