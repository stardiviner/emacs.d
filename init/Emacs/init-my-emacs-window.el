;;; init-my-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

;;; [ switch-window ] -- show a number on window instead of modeline.
;; (require 'switch-window)
;; (global-set-key (kbd "C-x o") 'switch-window)


;;; [ window-number ] --
(require 'window-number)

(window-number-mode)

(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according
  to numbers with the C-x C-j prefix. Another mode,
  `window-number-meta-mode' enables the use of the M- prefix."
  t)

;; (autoload 'window-number-meta-mode "window-number"
;; "A global minor mode that enables use of the M- prefix to select
;; windows, use `window-number-mode' to display the window numbers in
;; the mode-line."
;; t)

;; (push (cons 'my-window-number-meta-mode my-window-number-mode-map) minor-mode-map-alist)

;;; window-number face
(set-face-attribute 'window-number-face nil
                    :background "red" :foreground "black"
                    :box '(:color "dark red" :line-width 1 :style nil)
                    :bold 'normal)


;;; [ window-numbering ] --
;; (unless (package-installed-p 'window-numbering)
;;   (package-install 'window-numbering))
;; (require 'window-numbering)


;;; [ workgroups2 ] --

;;; Usage
;;; Workgroups is a session manager for Emacs.
;;;
;;;     It saves all your opened buffers, their location and sizes on disk to restore later
;;;     You can create several workspaces
;;;
;;; You can also restore such buffers as: org-agenda, shell, magit-status, help.

;;; Key Bindings
;; Most commands are bound to both <prefix> <key> and <prefix> C-<key>.

;; By default prefix is: "C-c z" (To change it - see settings below)
;; Type <prefix> ? (Eval (wg-help)) for more help.
;; <prefix> <key>
;; <prefix> c    - create workgroup
;; <prefix> A    - rename workgroup
;; <prefix> k    - kill workgroup
;; <prefix> v    - switch to workgroup
;; <prefix> C-s  - save session
;; <prefix> C-f  - load session


(require 'workgroups2)

;; Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c z"))
;; Change workgroups session file
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")

;; FIXME: remove modeline workgroups-mode dynamic part. ( (First workgroup)296, 6)
(setq wg-mode-line-display-on nil   ; toggle workgroups' mode-line display.
      wg-mode-line-disable t      ; do not modify mode-line.
      wg-mode-line-only-name nil    ; only show workgroup name.
      wg-mode-line-decor-divider ":"
      wg-mode-line-decor-left-brace "("
      wg-mode-line-decor-right-brace ")"
      )

;; (workgroups-mode 1)        ; put this one at the bottom of .emacs



;;; [ E2WM ] --- Equilibrium Emacs Window Manager

;;; Usage:
;; The current implementation has following perspectives:
;; * code      : main coding layout
;; * two       : side by side layout
;; * doc       : reading documentation layout
;; * dashboard : showing plug-ins like dashboard in Mac OSX
;; * array     : selecting buffers like expose in Mac OSX

(unless (package-installed-p 'e2wm)
  (package-install 'e2wm))
(require 'e2wm)

(global-set-key (kbd "C-c +") 'e2wm:start-management)
(global-set-key (kbd "C-c -") 'e2wm:stop-management)

;;; Customization
;; (setq e2wm:c-my-org-repice
;;       '(| (:left-max-size 35)
;;           (- (:upper-size-ratio 0.7)
;;              files history)
;;           (- (:upper-size-ratio 0.7)
;;              (| (:right-max-size 30)
;;                 main imenu)
;;              sub)))

;; (setq e2wm:c-my-org-winfo
;;   '((:name main)
;;     (:name files :plugin files)
;;     (:name history :plugin history-list)
;;     (:name sub :buffer "*info*" :default-hide t)
;;     (:name imenu :plugin imenu :default-hide nil))
;;   )





(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
