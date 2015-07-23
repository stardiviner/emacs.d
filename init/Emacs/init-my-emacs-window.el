;;; init-my-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

;;; [ switch-window ] -- show a number on window instead of modeline.

;; (require 'switch-window)
;; (global-set-key (kbd "C-x o") 'switch-window)

;;; switch to new splitted window after split.
;;
;; 1. this will break the default action, and affect other window behaviors.
;;
;; (defadvice split-window-below (after switch-to-new-split-below-window activate)
;;   "Switch to new splitted window."
;;   (other-window 1))
;; 2. bind to a function is a better solution.
;;
(define-key global-map (kbd "C-x 2")
  '(lambda ()
     (interactive)
     (split-window-vertically)
     (other-window 1)))

(define-key global-map (kbd "C-x 3")
  '(lambda ()
     (interactive)
     (split-window-horizontally)
     (other-window 1)))


;;; [ display-buffer-alist ]

;;; Example:
;; save/restore window configuration when creating/killing poporg buffer.
;;
;; (add-to-list 'display-buffer-alist
;;              '("\\*poporg.*?\\*" . ((display-buffer-reuse-window
;;                                      display-buffer-pop-up-window)
;;                                     . ((inhibit-same-window . t)))))


;;; [ winner ]

;;; Usage
;;
;; - [C-c] :: prefix
;; - [C-c Left] :: undo
;; - [C-c Right] :: redo

(winner-mode)


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

;; save/restore frame positions
(setq wg-control-frames t
      wg-restore-frame-position t
      wg-remember-frame-for-each-wg t
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
;;
;; (global-set-key (kbd "C-c +") 'e2wm:start-management)
;; (global-set-key (kbd "C-c -") 'e2wm:stop-management)
;;
;; (setq e2wm:c-my-org-repice
;;       '(| (:left-max-size 35)
;;           (- (:upper-size-ratio 0.7)
;;              files history)
;;           (- (:upper-size-ratio 0.7)
;;              (| (:right-max-size 30)
;;                 main imenu)
;;              sub)))
;;
;; (setq e2wm:c-my-org-winfo
;;       '((:name main)
;;         (:name files :plugin files)
;;         (:name history :plugin history-list)
;;         (:name sub :buffer "*info*" :default-hide t)
;;         (:name imenu :plugin imenu :default-hide nil))
;;       )


;;; [ ne2wm ]

;; (require 'ne2wm-setup)


;;; [ perspective ] -- Perspectives for Emacs.

;; This package provides tagged workspaces in Emacs, similar to workspaces in
;; windows managers such as Awesome and XMonad (and somewhat similar to multiple
;; desktops in Gnome or Spaces in OS X).

;;; Usage:
;;
;; - `persp-mode' :: activate perspective mode.
;; - [C-x x] :: prefix
;;
;; Key -- Command
;;
;; s -- persp-switch: Query a perspective to switch or create
;; k -- persp-remove-buffer: Query a buffer to remove from current perspective
;; c -- persp-kill : Query a perspective to kill
;; r -- persp-rename: Rename current perspective
;; a -- persp-add-buffer: Querry an open buffer to add to current perspective
;; A -- persp-set-buffer: Add buffer to current perspective and remove it from all others
;; i -- persp-import: Import a given perspective from another frame.
;; n, <right> -- persp-next : Switch to next perspective
;; p, <left> -- persp-prev: Switch to previous perspective


;; (require 'perspective)


;;; [ window-purpose ] -- Organize Windows and Buffers According to Purposes.

;;; Usage:
;;
;; - [C-c ,] :: prefix
;;
;;; Steps:
;;
;; 1. configuration
;;
;;   (add-to-list 'purpose-user-mode-purposes '(python-mode . py))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
;;   (purpose-compile-user-configuration)
;;
;;   - configurations
;;    
;;     Manually: M-x customize-group purpose. Look at:
;;    
;;     - "Purpose User Mode Purposes": recognize purpose according to major mode
;;     - "Purpose User Name Purposes": recognize purpose according to buffer name (for exact names)
;;     - "Purpose User Regexp Purposes": recognize purpose according to buffer name (for name patterns)
;;     - "Purpose Use Default Configuration": toggle default configuration on/off
;;    
;;     (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;;     (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;;     (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;;     (setq purpose-use-default-configuration t) ; not really necessary, default is t
;;     (purpose-compile-user-configuration) ; activates your changes
;;
;;
;; 2. change window layout
;;
;; If you have a previously saved layout, you can load it with
;; purpose-load-window-layout and skip step 2 and step 3.
;;
;;   0. open a Python file
;;   1. C-c , d (purpose-toggle-window-purpose-dedicated) so window is dedicated ("[py]" in the status bar will change to "[py!]")
;;   2. C-x 1 (delete-other-windows)
;;   3. C-x 2 (split-window-below)
;;   4. C-c C-z (python-shell-switch-to-shell)
;;   5. C-c , d so window is dedicated
;;   6. C-x o (other-window) to select the python file's window
;;   7. C-x ^ (enlarge-window) until you like the sizes of the windows
;;
;; 3. save window layout
;;
;;   [M-x purpose-save-window-layout]


;; (require 'window-purpose)
;;
;; (setq purpose-preferred-prompt 'helm
;;       ;; purpose-x-*
;;       purpose-x-popwin-position 'bottom
;;       purpose-x-popwin-height 0.5
;;       purpose-x-popwin-width 0.5)
;;
;; (add-to-list 'purpose-user-mode-purposes '(popwin-mode . popup-window))
;; (add-to-list 'purpose-user-mode-purposes '(compilation-mode . popup-window))
;; (add-to-list 'purpose-user-mode-purposes '(help-mode . popup-window))
;; (add-to-list 'purpose-user-mode-purposes '(ack-and-a-half-mode . popup-window))
;; (add-to-list 'purpose-user-mode-purposes '(dired-mode . sidebar-window))
;;
;; (purpose-compile-user-configuration)
;;
;; (purpose-mode)


;;; [ golden-ratio ] -- Automatic resizing of Emacs windows to the golden ratio.

;;; golden-ratio helps on this issue by resizing automatically the windows you
;;; are working on to the size specified in the "Golden Ratio". The window that
;;; has the main focus will have the perfect size for editing, while the ones
;;; that are not being actively edited will be re-sized to a smaller size that
;;; doesn't get in the way, but at the same time will be readable enough to know
;;; it's content.

;;; Usage:
;;
;; - [M-x golden-ratio] :: manually invoke `golden-ratio'.
;; - [M-x golden-ratio-mode] :: toggle `golden-ratio-mode'.
;; - [M-x golden-ratio-toggle-widescreen] :: toggle between widescreen and regular width window.

;; (require 'golden-ratio)
;;
;; (golden-ratio-mode 1)
;;
;; (setq golden-ratio-auto-scale t ; for wide screens
;;       golden-ratio-adjust-factor .8 ; adjust factor
;;       golden-ratio-wide-adjust-factor .8)


;;; [ popwin ] -- Popup Window Manager for Emacs (*always* shows upon minibuffer)

;;; https://github.com/m2ym/popwin-el

;; popup window which from `display-buffer'.

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

;; (push `(,special-buffer-regexp :regexp t :noselect nil)
;;       popwin:special-display-config)

;; TODO:
(push '("*scratch*" :height 30 :position bottom) popwin:special-display-config)

;;; Debugger mode, *Backtrace*
(push '("*Backtrace*" :position bottom :height 15 :noselect t) popwin:special-display-config)

;; M-! shell command output
(push '("*Shell Command Output*" :position bottom :height 15 :noselect t) popwin:special-display-config)

;;; Info
(push '(apropos-mode :position bottom :height 15) popwin:special-display-config)

;;; help-mode, *Help*, *Metahelp* (from mode C-h ?)
(push '(help-mode :position bottom :height 15) popwin:special-display-config)

;;; Org-mode
;; FIXME: this does not work.
;; (push '("*Org todo" :position bottom) popwin:special-display-config)
;; (push '("*Org Note" :position bottom :height 15) popwin:special-display-config)
;; (push '("*Org tags*" :position bottom) popwin:special-display-config)
;; (push '("*Agenda Commands*" :position bottom) popwin:special-display-config)
;; (push '("*Org Agenda*" :position bottom :height 20) popwin:special-display-config)
(push '("*Org-Babel Error Output*" :position bottom :height 10 :noselect t) popwin:special-display-config)

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
(push '("*Man *" :position bottom :height 15) popwin:special-display-config)

;;; Ediff
(push '("*Ediff Control Panel*" :position bottom :height 15) popwin:special-display-config)

;;; Compilation
(push '(compilation-mode :position bottom :height 15) popwin:special-display-config)
;; (push '("*compilation*" :position bottom :height 15) popwin:special-display-config)

;;; Tags
;; cscope
;; ascope
(push '(ascope-list-entry-mode :position bottom :height 15) popwin:special-display-config)

;;; Git
;;; git-modes
(push '("\\*git-" :regexp t :position top) popwin:special-display-config)
;;; Magit
(push '(magit-commit-mode :position bottom :height 20) popwin:special-display-config)
;; (push '("*magit-commit" :position bottom :height 15) popwin:special-display-config)
(push '(magit-process-mode :position bottom :height 15 :noselect t) popwin:special-display-config)
;; (push '("*magit-process*" :position bottom :height 15) popwin:special-display-config)

;;; git-gutter[+]
;; FIXME:
;; (push '("*git-gutter+-diff*" :position bottom) popwin:special-display-config)
;; (push '(git-gutter+-commit-mode :position bottom) popwin:special-display-config)


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

;;; comint-mode
(push '(comint-mode :position bottom :height 15) popwin:special-display-config)
;; (push '("*compilation*" :position bottom :height 15) popwin:special-display-config)

;;; Flycheck
(push '("*Flycheck errors*" :position bottom :height 10) popwin:special-display-config)
(push '("*Compile-Log*" :position bottom :height 15) popwin:special-display-config)

;;; quickrun "*quickrun*"
(push '(quickrun/mode :position bottom :height 10) popwin:special-display-config)
(push '("*quickrun*" :position bottom :height 10) popwin:special-display-config)

;;; *Pp Eval Output*
;; TODO: this will make this buffer does not show up.
(push '("*Pp Eval Output*" :position bottom :height 15) popwin:special-display-config)

;;; sdcv
(push '("*SDCV*" :position bottom :height 15 :noselect t) popwin:special-display-config)

;;; shelldoc
(push '("*Shelldoc*" :position top :height 15) popwin:special-display-config)

;; bm.el
;;; TODO: modify source code.
(push '(bm-show-mode :position bottom :height 15) popwin:special-display-config)
(push '("*bm-bookmarks*" :position bottom :height 15) popwin:special-display-config)

;; display-time-world
(push '("*wclock*" :position bottom :height 10 :noselect t) popwin:special-display-config)

;; process list
(push '("*Process List*" :position bottom :height 10) popwin:special-display-config)
(push '(process-menu-mode :position bottom :height 10) popwin:special-display-config)

;; BBDB
(push '(bbdb-mode :position bottom :height 15) popwin:special-display-config)
(push '("*BBDB*" :position bottom :height 15) popwin:special-display-config)

;; pdf-tools
(push '(pdf-occur-buffer-mode :position bottom :height 10) popwin:special-display-config)
(push '(pdf-outline-buffer-mode :position bottom :height 10) popwin:special-display-config)
(push '("*PDF-Metadata*" :position bottom :height 15) popwin:special-display-config)

;; Festival
(push '("*festival*" :position bottom :height 15) popwin:special-display-config)

;; Helm (all helm complete candidates popup)
;; (push '("^\\*helm.*\\*$" :regexp t :position bottom :height 10) popwin:special-display-config)

;; TeX/LaTeX (AUCTeX)
;; (push '(TeX-output-mode :position bottom :height 15) popwin:special-display-config)

;; ack-and-a-half
(push '(ack-and-a-half-mode :position bottom :height 15) popwin:special-display-config)

;; ag
(push '(ag-mode :position bottom :height 20) popwin:special-display-config)

;;; IELM
;; TODO:
(push '("*ielm*" :position bottom :height 15) popwin:special-display-config)
(push '(inferior-emacs-lisp-mode :position bottom :height 15) popwin:special-display-config)

;;; Clojure, CIDER
(push '(inf-clojure-mode :position bottom :height 15) popwin:special-display-config)
(push '(cider-clojure-interaction-mode :position bottom :height 15) popwin:special-display-config)

;;; ESS
;; *julia*
;; (push '(inferior-ess-mode :position bottom :height 15) popwin:special-display-config)
;; *Julia*
;; (push '(inferior-julia-mode :position bottom :height 15) popwin:special-display-config)

;;; yari Ruby document lookup
(push '(yari-mode :position bottom :height 15) popwin:special-display-config)
;;; rub-ruby - inf-ruby
(push '(inf-ruby-mode :position bottom :height 15) popwin:special-display-config)
;; (push '("*ruby*" :position bottom :height 15) popwin:special-display-config)
;; (push '("*rails*" :position bottom :height 15) popwin:special-display-config)

;;; Python
;; *Python*
;; (push '(inferior-python-mode :position bottom :height 15) popwin:special-display-config)

;; projectile-rails
(push '(projectile-rails-generate-mode :position bottom :height 15) popwin:special-display-config)
(push '(projectile-rails-compilation-mode :position bottom :height 15) popwin:special-display-config)
(push '(projectile-rails-server-mode :position bottom :height 10) popwin:special-display-config)

;;; ruby-compilation-mode (RubyComp)
;; FIXME: popwin can't capture this popup window. dive in ruby-compilation-mode source, it use Emacs built-in function window.el.gz -> `pop-to-buffer'.
(push '(ruby-compilation-mode :position bottom :height 15) popwin:special-display-config)

;;; jedi doc help
(push '("*jedi:doc" :position bottom :height 15) popwin:special-display-config)
(push '(rst-mode :position bottom :height 15) popwin:special-display-config)

;; octave help mode
(push '(octave-help-mode :position bottom :height 15) popwin:special-display-config)

;; calc -- Calculator
(push '(calc-mode :position bottom :height 10) popwin:special-display-config)
;; (push '("*Calculator*" :position bottom :height 10) popwin:special-display-config)

;; eww
(push '(eww-bookmark-mode :position bottom :height 15) popwin:special-display-config)

;;; checkdoc
;; FIXME:
(push '("*Checkdoc Status*" :position bottom :height 7) popwin:special-display-config)


;; TeX & AUCTeX
;; (push '(special-mode :position bottom :height 15) popwin:special-display-config)
(push '("*TeX Help*" :position bottom :height 15) popwin:special-display-config)

;;; howdoi
(push '(howdoi-mode :position bottom :height 7) popwin:special-display-config)
(push '("*How do I*" :position bottom :height 7) popwin:special-display-config)

;; elfeed
(push '(elfeed-search-mode :position top :height 20) popwin:special-display-config)
(push '("*elfeed-search*" :position top :height 20) popwin:special-display-config)

;; restclient
(push '("*HTTP Response*" :position bottom :height 20) popwin:special-display-config)

;; poporg
;; FIXME: not work
;; (push '("*poporg:*" :position bottom :height 20) popwin:special-display-config)

(defun my/popwin-func-for-poporg-edit-window (buffer)
  "Match poporg popup edit buffer.

The `BUFFER' is the popwin catch poporg edit popup buffer"
  (let ((mode (with-current-buffer buffer
                major-mode)))
    (and (string-match "\*poporg:\ .*\*" (buffer-name buffer))
       (eq mode 'org-mode))))

(push '(my/popwin-func-for-erc-private-message :height 15 :position bottom) popwin:special-display-config)


;;; [ shackle ] -- Enforce rules for popup windows.

;;; This package is heavily inspired by popwin and was hacked together after
;;; discovering it being hard to debug, creating overly many timers and exposing
;;; rather baffling bugs. shackle being intentionally simpler and easier to
;;; understand is considered a debugging-friendly feature, not a bug. However if
;;; you prefer less rough edges, a sensible default configuration and having
;;; more options for customizing, give popwin a try.

;;; `shackle-rules'
;; The condition can be either a symbol, a string, a list of either or t. A
;; symbol is interpreted as the major mode of the buffer to match, a string as
;; the name of the buffer (which can be turned into regexp matching by using the
;; :regexp key with a value of t in the key-value part), a list groups either
;; symbols or strings (as described earlier) while requiring at least one
;; element to match and t as the fallback rule to follow when no other match
;; succeeds. If you set up a fallback rule, make sure it's the last rule in
;; shackle-rules, otherwise it will always be used.

;; (require 'shackle)
;;
;; (setq shackle-default-alignment 'below
;;       shackle-default-ratio 0.4
;;       shackle-lighter " ⛓")
;;
;; (setq shackle-rules '((t :same t)
;;                       ;; enables the rather radical behaviour of always reusing
;;                       ;; the current window in order to avoid unwanted window
;;                       ;; splitting.
;;
;;                       ;; use popup for all buffers
;;                       (t :popup t)
;;                      
;;                       ;; provides a less intrusive user experience to select all
;;                       ;; windows by default unless they are spawned by
;;                       ;; compilation-mode and demonstrates how to use
;;                       ;; exceptions.
;;                       (compilation-mode :noselect t)
;;                       (t :select t)
;;
;;                       ;; tames helm windows by aligning them at the bottom with
;;                       ;; a ratio of 40%
;;                       ("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)
;;                       ))
;;
;; (shackle-mode)


;;; [ zoom-window ] -- zoom/un-zoom window like tmux.

;;; Usage:
;;
;; - `zoom-window' :: Toggle between zooming current window and unzooming.

(require 'zoom-window)

(setq zoom-window-mode-line-color "dark red"
      zoom-window-use-elscreen nil ; whether use extension elscreen.
      )

(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)


(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
