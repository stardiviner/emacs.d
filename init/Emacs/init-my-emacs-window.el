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
;; (set-face-attribute 'window-number-face nil
;;                     :background "red" :foreground "black"
;;                     :box '(:color "dark red" :line-width 1 :style nil)
;;                     :bold 'normal)


;;; [ window-numbering ] --
;; (unless (package-installed-p 'window-numbering)
;;   (package-install 'window-numbering))
;; (require 'window-numbering)


;; popup current window to another new frame.

(defun my-turn-current-window-into-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(if (boundp 'window-number-mode-map)
    (define-key window-number-mode-map (kbd "C-x C-j p") 'my-turn-current-window-into-new-frame)
  (global-set-key (kbd "C-x C-j p") 'my-turn-current-window-into-new-frame))


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
(setq wg-prefix-key (kbd "C-c w"))
;; Change workgroups session file
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")

(setq wg-mode-line-display-on nil         ; toggle workgroups' mode-line display.
      wg-mode-line-disable t            ; do not modify mode-line.
      wg-mode-line-only-name nil          ; only show workgroup name.
      wg-mode-line-use-faces t          ;
      wg-mode-line-decor-divider ":"
      wg-mode-line-decor-left-brace "("
      wg-mode-line-decor-right-brace ")"
      wg-mode-line-decor-workgroup-unmodified #("-" 0 1
                                                (help-echo "The current workgroup is unmodified"))
      wg-mode-line-decor-workgroup-modified #("*" 0 1
                                              (help-echo "The current workgroup is modified"))
      wg-mode-line-decor-session-modified #("*" 0 1
                                            (help-echo "The session is modified"))
      wg-mode-line-decor-session-unmodified #("-" 0 1
                                              (help-echo "The session is unmodified"))
      wg-mode-line-decor-window-dedicated #("#" 0 1
                                            (help-echo "This window is dedicated to its buffer."))
      wg-mode-line-decor-window-undedicated #("-" 0 1
                                              (help-echo "This window is not dedicated to its buffer."))
      )

;; (set-face-attribute 'wg-mode-line-face nil
;;                     :foreground "dark cyan")

;; (workgroups-mode 1)        ; put this one at the bottom of .emacs



;;; [ E2WM ] --- Equilibrium Emacs Window Manager

;;; Usage:
;; The current implementation has following perspectives:
;; * code      : main coding layout
;; * two       : side by side layout
;; * doc       : reading documentation layout
;; * dashboard : showing plug-ins like dashboard in Mac OSX
;; * array     : selecting buffers like expose in Mac OSX

;; (require 'e2wm)

;; (global-set-key (kbd "C-c +") 'e2wm:start-management)
;; (global-set-key (kbd "C-c -") 'e2wm:stop-management)

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



;;; [ popwin ] -- Popup Window Manager for Emacs (*always* shows upon minibuffer)

;;; https://github.com/m2ym/popwin-el

;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
;;
;; Take an example. When you complete file names during find-file, the
;; (annoying) *Completions* buffer will appear in a newly splitted window. You
;; might understand the necessity of the window, but you may wonder why the
;; window still remains after completion...
;;
;; popwin resolves there problems. Windows of such temporary buffers will be
;; shown as a popup window, and you can close them smoothly by typing C-g in
;; anytime.

;;; Popup windows:
;; - *Help*
;; - *Completions*
;; - *Compilation*
;; - *Occur*

;;; NOTE:
;;; popwin can work for popup function like `pop-to-buffer' etc. If some
;;; extension uses split window and switch buffer function, not popup function,
;;; popwin may not work,

;;; Config examples:
;;;
;;; 1. buffer name regexp pattern.
;;;
;;; M-! shell command output
;;; (push '("*Shell Command Output*" :position bottom :height 15) popwin:special-display-config)
;;;
;;; 2. major-mode name.
;;;
;;; You can specify major-mode name as pattern like following.
;;; (push '(erc-mode) popwin:special-display-config)
;;;
;;; 3. combine upper two matches.
;;; regexp match + major mode match to capture a exact buffer more exactly.
;;;
;;; (defun my/popwin-func (buffer)
;;;   (let ((mode (with-current-buffer buffer
;;;                 major-mode)))
;;;     (and (string-match "REGEXP" (buffer-name buffer))
;;;          (eq mode 'SOME-MODE))))
;;;
;;; (push '(my/popwin-func :height 15 :position bottom) popwin:special-display-config)

;;; Usage:
;; - [C-g] :: close popup window.
;;
;; popwin provides a default keymap named `popwin:keymap'.
;; (global-set-key (kbd "C-z") popwin:keymap)
;; | Key    | Command                               |
;; |--------+---------------------------------------|
;; | b      | popwin:popup-buffer                   |
;; | l      | popwin:popup-last-buffer              |
;; | o      | popwin:display-buffer                 |
;; | C-b    | popwin:switch-to-last-buffer          |
;; | C-p    | popwin:original-pop-to-last-buffer    |
;; | C-o    | popwin:original-display-last-buffer   |
;; | SPC    | popwin:select-popup-window            |
;; | s      | popwin:stick-popup-window             |
;; | 0      | popwin:close-popup-window             |
;; | f, C-f | popwin:find-file                      |
;; | e      | popwin:messages                       |
;; | C-u    | popwin:universal-display              |
;; | 1      | popwin:one-window                     |


(require 'popwin)

(popwin-mode 1)

(global-set-key (kbd "C-z") popwin:keymap)

(setq popwin:close-popup-window-timer-interval 0.1
      popwin:reuse-window t ; t, 'current,
      )

;;; `popwin:special-display-config'
;;; push popwin:special-display-config `flags': [C-h v popwin:special-display-config]
;;; - :position [bottom|top]
;;; - :height 10
;;; - :width 100
;;; - :noselect t
;;; - :stick t
;;; - :regexp t
;;; - :dedicated t
;;; - :tail t

;; TODO:
(push '("*scratch*" :height 30 :position bottom) popwin:special-display-config)

;;; Debugger mode, *Backtrace*
(push '("*Backtrace*" :position bottom :height 15) popwin:special-display-config)

;; M-! shell command output
(push '("*Shell Command Output*" :position bottom :height 15) popwin:special-display-config)

;;; Org-mode
;; FIXME: this does not work.
;; (push '("*Org todo" :position bottom) popwin:special-display-config)
;; (push '("*Org Note" :position bottom :height 15) popwin:special-display-config)
;; (push '("*Org tags*" :position bottom) popwin:special-display-config)
;; (push '("*Agenda Commands*" :position bottom) popwin:special-display-config)
;; (push '("*Org Agenda*" :position bottom :height 20) popwin:special-display-config)
(push '("*Org-Babel Error Output*" :position bottom :height 10) popwin:special-display-config)

;;; Completion List (completion-list-mode)
;; FIXME: popwin can't capture this popup window.
(push '(completion-list-mode :position bottom :height 15) popwin:special-display-config)

;;; Eshell
(push '(eshell-mode :position bottom :height 15) popwin:special-display-config)
(push '("*eshell*" :position bottom :height 15) popwin:special-display-config)

;;; Occur Mode
(push '("*Occur*" :position bottom :height 10) popwin:special-display-config)

;;; Man/Women
(push '(Man-mode :position bottom :height 15) popwin:special-display-config)

;;; Ediff
(push '("*Ediff Control Panel*" :position bottom :height 15) popwin:special-display-config)

;;; Compilation
(push '(compilation-mode :position bottom :height 15) popwin:special-display-config)
;; (push '("*compilation*" :position bottom :height 15) popwin:special-display-config)

;;; Magit
;; TODO: create one for commit message buffer.
;; git-modes
(push '("\\*git-" :regexp t :position top) popwin:special-display-config)

;;; ERC
;; TODO: This does not work. Because ERC does not use `pop-to-buffer' for private message buffer.
(defun my/popwin-func-for-erc-private-message (buffer)
  "Match private messages which except channel buffers that start with a #.

The `BUFFER' is the popwin catch pop private message buffer."
  (let ((mode (with-current-buffer buffer
                major-mode)))
    ;; TODO or string match `erc-pals' variables list.
    (and (string-match "^[^#]*" (buffer-name buffer))
       (eq mode 'erc-mode))))

(push '(my/popwin-func-for-erc-private-message :height 15 :position bottom) popwin:special-display-config)

;;; Flycheck
(push '("*Flycheck errors*" :position bottom :height 10) popwin:special-display-config)
(push '("*Compile-Log*" :position bottom :height 15) popwin:special-display-config)

;;; *Pp Eval Output*
;; TODO: this will make this buffer does not show up.
(push '("*Pp Eval Output*" :position bottom :height 15) popwin:special-display-config)

;;; sdcv
(push '("*SDCV*" :position bottom :height 15) popwin:special-display-config)

;; bm.el
(push '("*bm-bookmarks*" :position bottom :height 15) popwin:special-display-config)
(push '(bm-show-mode :position bottom :height 15) popwin:special-display-config)

;; display-time-world
(push '("*wclock*" :position bottom :height 10) popwin:special-display-config)

;; process list
(push '("*Process List*" :position bottom :height 10) popwin:special-display-config)
(push '(process-menu-mode :position bottom :height 10) popwin:special-display-config)

;; BBDB
(push '(bbdb-mode :position bottom :height 15) popwin:special-display-config)
(push '("*BBDB*" :position bottom :height 15) popwin:special-display-config)

;; Festival
(push '("*festival*" :position bottom :height 15) popwin:special-display-config)

;; Helm (all helm complete candidates popup)
; (push '("^\\*helm.*\\*$" :regexp t :position bottom :height 10) popwin:special-display-config)

;; TeX/LaTeX (AUCTeX)
;; (push '(TeX-output-mode :position bottom :height 15) popwin:special-display-config)

;; ack-and-a-half
(push '(ack-and-a-half-mode :position bottom :height 15) popwin:special-display-config)

;; ag
(push '(ag-mode :position bottom :height 15) popwin:special-display-config)

;;; IELM
;; TODO:
(push '("*ielm*" :position bottom :height 15) popwin:special-display-config)
(push '(inferior-emacs-lisp-mode :position bottom :height 15) popwin:special-display-config)

;;; yari Ruby document lookup
(push '(yari-mode :position bottom :height 15) popwin:special-display-config)
;;; rub-ruby - inf-ruby
(push '(inf-ruby-mode :position bottom :height 15) popwin:special-display-config)
;; (push '("*ruby*" :position bottom :height 15) popwin:special-display-config)
;; (push '("*rails*" :position bottom :height 15) popwin:special-display-config)
(push '(projectile-rails-generate-mode :position bottom :height 15) popwin:special-display-config)
(push '(projectile-rails-compilation-mode :position bottom :height 15) popwin:special-display-config)

;;; ruby-compilation-mode (RubyComp)
;; FIXME: popwin can't capture this popup window. dive in ruby-compilation-mode source, it use Emacs built-in function window.el.gz -> `pop-to-buffer'.
(push '(ruby-compilation-mode :position bottom :height 15) popwin:special-display-config)

;; octave help mode
(push '(octave-help-mode :position bottom :height 15) popwin:special-display-config)

;; calc -- Calculator
(push '(calc-mode :position bottom :height 10) popwin:special-display-config)
;; (push '("*Calculator*" :position bottom :height 10) popwin:special-display-config)

;;; checkdoc
;; FIXME:
(push '("*Checkdoc Status*" :position bottom :height 7) popwin:special-display-config)

;;; howdoi
(push '(howdoi-mode :position bottom :height 7) popwin:special-display-config)
(push '("*How do I*" :position bottom :height 7) popwin:special-display-config)


(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here





