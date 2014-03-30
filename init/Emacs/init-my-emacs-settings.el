;;; init-my-emacs-settings.el --- init my Emacs settings

;;; Commentary:

;;; Code:

;;; [ Disabled Commands ]
;; To enable all disabled commands in one fell swoop, put this in .emacs (not recommended for newbies):
;; TODO uncomment this when you are very familiar with Emacs.
;; (setq disabled-command-function nil)


(fset 'yes-or-no-p 'y-or-n-p) ; treat 'y' as yes, 'n' as no.

(setq inhibit-startup-message t) ; no splash screen


;;; [ Time ]
(setq system-time-locale "C") ; make timestamps in org-mode appear in English.
;;; time-stamp
;; (add-hook 'before-save-hook 'time-stamp)
;; (setq time-stamp-pattern nil)


;; [ Bell ]
(setq visible-bell t)


;;; [ User Information ]
(setq user-full-name "stardiviner")
(setq user-mail-address "numbchild@gmail.com")
;; (setq user-login-name "chris")


;;; [ mode ]
(setq default-major-mode 'org-mode) ; use org-mode for any unspecified mode.


;;; [ session ]

;;; [ auto save ]
;; (auto-save-mode t)
(setq auto-save-default t               ; create #autosave# files
      auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-"
      auto-save-interval 200)

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
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 3
      version-control t
      )

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))


;;; [ version control ] (vc)



;;; [ register ]
;; Usage:
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


;; [ Bookmark ]
;; Usage:
;; - [C-x r m] -- mark as a bookmark
;; - [C-x r b] -- jump to a bookmark
;; - [C-x r l] -- list your bookmarks
;;   - a -- show annotation for current bookmark
;;   - A -- show all annotations
;;   - d -- mark as delete
;;   - e -- edit the annotation for current bookmark
;;   - m -- mark various entries for display and other operations
;;   - o -- visit the current bookmark in another window, keeping the bookmark list open
;;   - C-o -- switch to the current bookmark in another window
;;   - r -- rename the current bookmark
;;   - x -- execute marked status actions
;; - [M-x bookmark-set] -- add current page into bookmark
(require 'bookmark)
(setq bookmark-default-file "~/.emacs.d/my-init/bookmarks.bmk")
(setq bookmark-save-flag 1)


;;; [ macro ]
;; file to save macros:
;; ~/.emacs.d/macros/macros
(load-file "~/.emacs.d/init/macros/macros")






(provide 'init-my-emacs-settings)

;;; init-my-emacs-settings.el ends here
