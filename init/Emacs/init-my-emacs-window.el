;;; init-my-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

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

;; popup current window to another new frame.

(defun my-turn-current-window-into-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))


;;; [ display-buffer-alist ]

;; Learn about display actions, see [[info:elisp#Display Action Functions]].

;; display poporg popup buffer below the selected window with 0.3 height.
;; (add-to-list 'display-buffer-alist
;;              '("\\*poporg:\ .*?\\*" ; *poporg: init-my-emacs-window.el*
;;                (display-buffer-reuse-window
;;                 display-buffer-below-selected)
;;                (window-height . 0.3)
;;                ))


;;; [ winner ]

;;; Usage
;;
;; - [C-c] :: prefix
;; - [C-c Left] :: undo
;; - [C-c Right] :: redo

(winner-mode 1)


;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
  :config
  ;; display window number in mode-line.
  ;; (ace-window-display-mode -1)
  
  (global-set-key (kbd "C-x C-j") 'ace-window)

  (set-face-attribute 'aw-leading-char-face nil
                      :background "#004A5D" :foreground "white"
                      :box '(:color "cyan" :line-width 1)
                      :height 200)
  (set-face-attribute 'aw-background-face nil
                      :foreground "#333333")
  (set-face-attribute 'aw-mode-line-face nil
                      :foreground "orange red")

  (setq aw-background nil)
  )


;;; [ resize-window ]

(use-package resize-window
  :ensure t
  :config
  (setq resize-window-allow-backgrounds nil)
  (global-set-key (kbd "C-x w") 'resize-window)
  )


;;; [ E2WM ] --- Equilibrium Emacs Window Manager

;;; Usage:
;; The current implementation has following perspectives:
;; * code      : main coding layout
;; * two       : side by side layout
;; * doc       : reading documentation layout
;; * dashboard : showing plug-ins like dashboard in Mac OSX
;; * array     : selecting buffers like expose in Mac OSX

(use-package e2wm
  ;; :ensure t
  ;; :config
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
  )


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


;; (use-package perspective
;;   :ensure t)


;;; [ window-purpose ] -- Organize Windows and Buffers According to Purposes.

;; (use-package window-purpose
;;   :ensure t
;;   :config
;;   (setq purpose-preferred-prompt 'ivy
;;         purpose-layout-dirs '(locate-user-emacs-file ".purpose/layouts/")
;;         )
;;
;;   (setq pop-up-frames t)
;;
;;   ;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;;   ;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;;   ;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;;
;;   (add-to-list 'purpose-user-mode-purposes '(popwin-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(compilation-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(help-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(ack-and-a-half-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(dired-mode . sidebar-window))
;;
;;   (add-to-list 'purpose-special-action-sequences
;;                '(popup-frame
;;                  purpose-display-reuse-window-buffer
;;                  purpose-display-reuse-window-purpose
;;                  purpose-display-pop-up-frame))
;;
;;   (setq purpose-use-default-configuration t)
;;   (purpose-compile-user-configuration)
;;
;;   (require 'window-purpose-x)
;;   (purpose-x-kill-setup)
;;
;;   (purpose-mode)
;;   )


;;; [ golden-radio ] -- automatic resizing of Emacs windows to the golden ratio.

(use-package golden-ratio
  ;; :ensure t
  :config
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-adjust-factor 1.0
        golden-ratio-wide-adjust-factor 0.8)
  (setq golden-ratio-recenter t)

  ;; exclude
  (setq golden-ratio-exclude-modes
        (append golden-ratio-exclude-modes
                '(ediff-mode
                  calendar-mode calc-mode dired-mode
                  speedbar-mode project-explorer-mode
                  gnus-summary-mode gnus-article-mode
                  mu4e-headers-mode mu4e-compose-mode
                  restclient-mode
                  )))

  ;; "\\`\\*[Hh]elm.*\\*\\'"
  (setq golden-ratio-exclude-buffer-regexp '("\\`\\*.*?\\*\\'")) ; *...* buffers
  
  (setq golden-ratio-exclude-buffer-names '(" *Org todo*" " *Org tags*"))
  (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")
  
  ;; for popwin.
  ;; FIXME:
  ;; (setq golden-ratio-inhibit-functions '(pop-to-buffer))

  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(window-number-select
                  )))

  ;; manually re-fit ratio.
  (global-set-key (kbd "C-x j") 'golden-ratio)

  ;; (progn
  ;;   (add-hook 'ediff-before-setup-windows-hook #'(lambda () (golden-ratio-mode -1)))
  ;;   (add-hook 'ediff-quit-hook #'(lambda () (golden-ratio-mode 1))))
  
  (golden-ratio-mode 1)
  )


;;; [ popwin ] -- Popup Window Manager for Emacs (*always* shows upon minibuffer)

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)

  (global-set-key (kbd "C-z") popwin:keymap)

  (setq popwin:close-popup-window-timer-interval 0.1
        popwin:reuse-window t ; t, 'current,
        )

  ;; `popwin:special-display-config'
  ;; push popwin:special-display-config `flags': [C-h v popwin:special-display-config]
  ;; - :position [bottom|top|left|right]
  ;; - :height 10
  ;; - :width 100
  ;; - :noselect t
  ;; - :stick t
  ;; - :regexp t
  ;; - :dedicated t
  ;; - :tail t

  ;; (push `(,special-buffer-regexp :regexp t :noselect nil)
  ;;       popwin:special-display-config)

  (push '("*scratch*" :position bottom :height 10) popwin:special-display-config)

  ;; Debugger mode, *Backtrace*
  (push '("*Backtrace*" :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; M-! shell command output
  (push '("*Shell Command Output*" :position bottom :height 6 :noselect t :tail t) popwin:special-display-config)
  (push '("*Async Shell Command*" :position bottom :height 6 :noselect t :tail t) popwin:special-display-config)

  ;; Info
  (push '(apropos-mode :position bottom :height 10) popwin:special-display-config)

  ;; help-mode, *Help*, *Metahelp* (from mode C-h ?)
  (push '(help-mode :position bottom :height 10) popwin:special-display-config)

  ;; Org-mode
  ;; FIXME: this does not work.
  ;; (push '("*Org todo" :position bottom) popwin:special-display-config)
  ;; (push '("*Org Note" :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*Org tags*" :position bottom) popwin:special-display-config)
  ;; (push '("*Agenda Commands*" :position bottom) popwin:special-display-config)
  ;; (push '("*Org Agenda*" :position bottom :height 20) popwin:special-display-config)
  (push '("*Org-Babel Error Output*" :position bottom :height 6 :noselect t) popwin:special-display-config)
  (push '("*Org-Babel Results*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; Completion List (completion-list-mode)
  ;; FIXME: popwin can't capture this popup window.
  (push '(completion-list-mode :position bottom :height 6) popwin:special-display-config)

  ;; Eshell
  ;; (push '(eshell-mode :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*eshell*" :position bottom :height 10) popwin:special-display-config)

  ;; Occur Mode
  (push '("*Occur*" :position bottom :height 6) popwin:special-display-config)

  ;; Man/Women
  (push '(Man-mode :position bottom :height 10) popwin:special-display-config)
  (push '("*Man *" :position bottom :height 10) popwin:special-display-config)

  ;; Ediff
  ;; (push '("*Ediff Control Panel*" :position bottom :height 10) popwin:special-display-config)

  ;; Compilation
  ;; (push '(compilation-mode :position bottom :height 10 :tail t) popwin:special-display-config)
  (push '(compilation-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*Compile-Log*" :position bottom :height 6) popwin:special-display-config)

  ;; quickrun "*quickrun*"
  (push '(quickrun/mode :position bottom :height 6 :noselect t) popwin:special-display-config)
  (push '("*quickrun*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; *Pp Eval Output*
  ;; TODO: this will make this buffer does not show up.
  (push '("*Pp Eval Output*" :position bottom :height 6) popwin:special-display-config)

  ;; File Explorer
  (push '(project-explorer-mode :position left :stick yes) popwin:special-display-config)
  
  ;; Tags
  ;; cscope
  ;; ascope
  (push '(ascope-list-entry-mode :position bottom :height 6) popwin:special-display-config)

  ;; Git
  ;; git-modes
  ;; (push '("\\*git-" :regexp t :position top) popwin:special-display-config)
  ;; Magit
  ;; (push '(magit-commit-mode :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*magit-commit" :position bottom :height 10) popwin:special-display-config)
  ;; cd
  ;; (push '(magit-process-mode :position bottom :height 10 :noselect t) popwin:special-display-config)
  ;; (push '("*magit-process*" :position bottom :height 10) popwin:special-display-config)

  ;; git-gutter[+]
  ;; FIXME:
  ;; (push '("*git-gutter+-diff*" :position bottom) popwin:special-display-config)
  ;; (push '(git-gutter+-commit-mode :position bottom) popwin:special-display-config)


  ;; ERC
  ;; TODO: This does not work. Because ERC does not use `pop-to-buffer' for private message buffer.
  (defun my/popwin-func-for-erc-private-message (buffer)
    "Match private messages which except channel buffers that start with a #.

The `BUFFER' is the popwin catch pop private message buffer."
    (let ((mode (with-current-buffer buffer
                  major-mode)))
      ;; TODO or string match `erc-pals' variables list.
      (and (string-match "^[^#]*" (buffer-name buffer))
           (eq mode 'erc-mode))))

  (push '(my/popwin-func-for-erc-private-message :height 10 :position bottom) popwin:special-display-config)

  ;; comint-mode
  (push '(comint-mode :position bottom :height 6) popwin:special-display-config)
  ;; (push '("*compilation*" :position bottom :height 10) popwin:special-display-config)

  ;; Flycheck
  (push '("*Flycheck errors*" :position bottom :height 6) popwin:special-display-config)

  ;; sdcv
  (push '("*SDCV*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; shelldoc
  (push '("*Shelldoc*" :position top :height 6) popwin:special-display-config)

  ;; bm.el
  ;; TODO: modify source code.
  (push '(bm-show-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*bm-bookmarks*" :position bottom :height 6) popwin:special-display-config)

  ;; display-time-world
  (push '("*wclock*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; command-log-mode
  (push '("*command-log*" :position bottom :height 6 :noselect t :tail t :stick t)
        popwin:special-display-config)
  
  ;; process list
  (push '("*Process List*" :position bottom :height 6) popwin:special-display-config)
  (push '(process-menu-mode :position bottom :height 6) popwin:special-display-config)

  ;; BBDB
  (push '(bbdb-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*BBDB*" :position bottom :height 6) popwin:special-display-config)

  ;; pdf-tools
  (push '(pdf-occur-buffer-mode :position bottom :height 6) popwin:special-display-config)
  (push '(pdf-outline-buffer-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*PDF-Metadata*" :position bottom :height 6) popwin:special-display-config)

  ;; Festival
  (push '("*festival*" :position bottom :height 6) popwin:special-display-config)

  ;; ack-and-a-half
  (push '(ack-and-a-half-mode :position bottom :height 6) popwin:special-display-config)

  ;; ag
  (push '(ag-mode :position bottom :height 6) popwin:special-display-config)
  ;; pt
  (push '(pt-search-mode :position bottom :height 6) popwin:special-display-config)

  ;; pcre2el
  (push '(rxt-help-mode :position bottom :height 6) popwin:special-display-config)

  ;; IELM
  ;; (push '(inferior-emacs-lisp-mode :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*ielm*" :position bottom :height 10) popwin:special-display-config)

  ;; Lisp
  (push '(inferior-lisp-mode :position bottom :height 6) popwin:special-display-config)
  (push '(sly-mrepl-mode :position bottom :height 6) popwin:special-display-config)
  (push '(slime-repl-mode :position bottom :height 6) popwin:special-display-config)
  (push '(slime-inspector-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*slime-description*" :position bottom :height 6) popwin:special-display-config)
  (push '("*slime-macroexpansion*" :position bottom :height 6) popwin:special-display-config)

  ;; Clojure, CIDER
  (push '(inf-clojure-mode :position bottom :height 6) popwin:special-display-config)
  (push '(cider-clojure-interaction-mode :position bottom :height 6) popwin:special-display-config)
  (push '(cider-inspector-mode :position bottom :height 6) popwin:special-display-config)

  ;; ESS
  (push '(inferior-ess-mode :position bottom :height 6) popwin:special-display-config)
  ;; *julia*
  (push '(inferior-julia-mode :position bottom :height 6) popwin:special-display-config)

  ;; yari Ruby document lookup
  (push '(yari-mode :position bottom :height 6) popwin:special-display-config)
  ;; rub-ruby - inf-ruby
  (push '(inf-ruby-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*ruby*" :position bottom :height 6) popwin:special-display-config)
  (push '("*pry*" :position bottom :height 6) popwin:special-display-config)
  (push '("*rails*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; projectile-rails
  (push '(projectile-rails-generate-mode :position bottom :height 6) popwin:special-display-config)
  (push '(projectile-rails-compilation-mode :position bottom :height 6) popwin:special-display-config)
  (push '(projectile-rails-server-mode :position bottom :height 6) popwin:special-display-config)

  ;; ruby-compilation-mode (RubyComp)
  ;; FIXME: popwin can't capture this popup window. dive in ruby-compilation-mode source, it use Emacs built-in function window.el.gz -> `pop-to-buffer'.
  (push '(ruby-compilation-mode :position bottom :height 6) popwin:special-display-config)

  ;; bundler
  (push '("*Bundler*" :position bottom :height 6) popwin:special-display-config)

  ;; Python
  ;; *Python*
  (push '(inferior-python-mode :position bottom :height 6) popwin:special-display-config)
  ;; *Anaconda*
  (push '(anaconda-mode-view-mode :position bottom :height 6) popwin:special-display-config)

  ;; jedi doc help
  (push '("*jedi:doc" :position bottom :height 6) popwin:special-display-config)
  (push '(rst-mode :position bottom :height 6) popwin:special-display-config)

  ;; JavaScript
  ;; js-comint: *js*
  (push '(inferior-js-mode :position bottom :height 6) popwin:special-display-config)

  ;; Go
  (push '(godoc-mode :position bottom :height 6) popwin:special-display-config)

  ;; Swift

  ;; Haskell
  (push '(inferior-haskell-mode :position bottom :height 6) popwin:special-display-config)
  (push '(haskell-interactive-mode :position bottom :height 6) popwin:special-display-config)

  ;; gnuplot
  (push '(gnuplot-comint-mode :position bottom :height 6) popwin:special-display-config)
  
  ;; octave help mode
  (push '(octave-help-mode :position bottom :height 6) popwin:special-display-config)

  ;; calc -- Calculator
  (push '(calc-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*Calculator*" :position bottom :height 6) popwin:special-display-config)

  ;; eww
  (push '(eww-bookmark-mode :position bottom :height 6) popwin:special-display-config)

  ;; checkdoc
  ;; FIXME:
  (push '("*Checkdoc Status*" :position bottom :height 6) popwin:special-display-config)


  ;; TeX/LaTeX (AUCTeX)
  ;; (push '(TeX-output-mode :position bottom :height 10) popwin:special-display-config)
  (push '("*TeX Help*" :position bottom :height 6) popwin:special-display-config)

  ;; Database: edbi
  (push '("*edbi-dialog-ds*" :position bottom :height 6) popwin:special-display-config)
  (push '(ctbl:table-mode :position bottom :height 6 :noselect t) popwin:special-display-config)
  (push '("\\*edbi:query-result .*" :regexp t :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; howdoi
  (push '(howdoi-mode :position bottom :height 6) popwin:special-display-config)
  (push '("*How do I*" :position bottom :height 6) popwin:special-display-config)

  ;; restclient
  (push '("*rest-client*" :position bottom :height 6) popwin:special-display-config)
  (push '("*HTTP Response*" :position bottom :height 6) popwin:special-display-config)

  ;; elfeed
  (push '(elfeed-search-mode :position top :height 6) popwin:special-display-config)
  (push '("*elfeed-search*" :position top :height 6) popwin:special-display-config)

  ;; mingus
  (push '(mingus-playlist-mode :position left) popwin:special-display-config)

  ;; vagrant
  (push '("*Vagrant*" :position bottom :height 6) popwin:special-display-config)

  ;; poporg
  ;; FIXME: not work
  ;; (push '("*poporg:*" :position bottom :height 10) popwin:special-display-config)

  (defun my/popwin-func-for-poporg-edit-window (buffer)
    "Match poporg popup edit buffer.

The `BUFFER' is the popwin catch poporg edit popup buffer"
    (let ((mode (with-current-buffer buffer
                  major-mode)))
      (and (string-match "\*poporg:\ .*\*" (buffer-name buffer))
           (eq mode 'org-mode))))

  (push '(my/popwin-func-for-erc-private-message :height 10 :position bottom) popwin:special-display-config)
  )


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

(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color "dark red"
        zoom-window-use-elscreen nil ; whether use extension elscreen.
        )

  (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
  )


(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
