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

(add-to-list 'display-buffer-alist
             '("^*Async Shell Command*" . (display-buffer-no-window)))

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
  :defer t
  :bind ("C-x C-j" . ace-window)
  :init
  ;; to re-override `dired-x''s `dired-bind-jump'. bind again after Emacs start.
  (with-eval-after-load 'dired-x
    (global-set-key (kbd "C-x C-j") 'ace-window))
  :config
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
  :defer t
  :init
  (global-set-key (kbd "C-x w") 'resize-window)
  :config
  (setq resize-window-allow-backgrounds nil)
  )


;;; [ popwin ] -- Popup Window Manager for Emacs (*always* shows upon minibuffer)

(use-package popwin
  :ensure t
  :config
  (setq popwin:close-popup-window-timer-interval 0.1
        popwin:reuse-window t ; t, 'current,
        )

  (popwin-mode 1)
  (global-set-key (kbd "C-z") popwin:keymap)

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

  ;; [M-!], [M-&] shell command output
  (push '("*Shell Command Output*" :position bottom :height 8 :noselect t :tail t) popwin:special-display-config)
  (push '("*Async Shell Command*" :position bottom :height 8 :noselect t :tail t) popwin:special-display-config)

  ;; Info
  (push '(apropos-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; help-mode, *Help*, *Metahelp* (from mode C-h ?)
  (push '(help-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; xref
  (push '(xref--xref-buffer-mode :position bottom :height 5 :noselect nil) popwin:special-display-config)
  
  ;; Org-mode
  ;; (push '("*Org todo" :position bottom) popwin:special-display-config)
  ;; (push '("*Org Note" :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*Org tags*" :position bottom) popwin:special-display-config)
  ;; (push '("*Agenda Commands*" :position bottom) popwin:special-display-config)

  ;; (push '("*Org Agenda*" :width 50 :position left) popwin:special-display-config)
  ;; (push '("*Org Agenda*" :height 0.2 :position bottom) popwin:special-display-config)
  (push '("*Org Select*" :height 0.2 :noselect nil :stick t) popwin:special-display-config)
  (push '("^CAPTURE-.+\*.org$" :position left :regexp t) popwin:special-display-config)
  
  (push '("*Org-Babel Error Output*" :position bottom :height 8 :noselect t) popwin:special-display-config)
  (push '("*Org-Babel Results*" :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; Completion List (completion-list-mode)
  (push '(completion-list-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Eshell
  ;; (push '(eshell-mode :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*eshell*" :position bottom :height 10) popwin:special-display-config)

  ;; Occur Mode
  (push '("*Occur*" :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Man/Women
  (push '(Man-mode :position bottom :height 10 :noselect nil) popwin:special-display-config)
  (push '("*Man *" :position bottom :height 10 :noselect nil) popwin:special-display-config)

  ;; Ediff
  ;; (push '("*Ediff Control Panel*" :position bottom :height 10) popwin:special-display-config)

  ;; Compilation
  ;; (push '(compilation-mode :position bottom :height 10 :tail t) popwin:special-display-config)
  (push '(compilation-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '("*Compile-Log*" :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; quickrun "*quickrun*"
  (push '(quickrun/mode :position bottom :height 6 :noselect t) popwin:special-display-config)
  (push '("*quickrun*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; *Pp Eval Output*
  ;; this will make this buffer does not show up.
  (push '("*Pp Eval Output*" :position bottom :height 8) popwin:special-display-config)

  ;; File Explorer
  (push '(project-explorer-mode :position left :stick yes) popwin:special-display-config)
  
  ;; Tags
  ;; gtags
  (push '(ggtags-global-mode :position bottom :height 8) popwin:special-display-config)
  ;; cscope
  ;; ascope
  (push '(ascope-list-entry-mode :position bottom :height 8) popwin:special-display-config)

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
  ;; (push '("*git-gutter+-diff*" :position bottom) popwin:special-display-config)
  ;; (push '(git-gutter+-commit-mode :position bottom) popwin:special-display-config)


  ;; comint-mode
  (push '(comint-mode :position bottom :height 8) popwin:special-display-config)
  ;; (push '("*compilation*" :position bottom :height 10) popwin:special-display-config)

  ;; Flycheck
  (push '("*Flycheck errors*" :position bottom :height 8) popwin:special-display-config)

  ;; sdcv
  (push '("*SDCV*" :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; shelldoc
  (push '("*Shelldoc*" :position top :height 8) popwin:special-display-config)

  ;; bm.el
  (push '(bm-show-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*bm-bookmarks*" :position bottom :height 8) popwin:special-display-config)

  ;; display-time-world
  (push '("*wclock*" :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; command-log-mode
  (push '("*command-log*" :position bottom :height 8 :noselect t :tail t :stick t)
        popwin:special-display-config)
  
  ;; process list
  (push '("*Process List*" :position bottom :height 8) popwin:special-display-config)
  (push '(process-menu-mode :position bottom :height 8) popwin:special-display-config)

  ;; BBDB
  (push '(bbdb-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*BBDB*" :position bottom :height 8) popwin:special-display-config)

  ;; pdf-tools
  (push '(pdf-occur-buffer-mode :position bottom :height 8) popwin:special-display-config)
  (push '(pdf-outline-buffer-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*PDF-Metadata*" :position bottom :height 8) popwin:special-display-config)

  ;; Festival
  (push '("*festival*" :position bottom :height 8) popwin:special-display-config)

  ;; ack-and-a-half
  (push '(ack-and-a-half-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; ag
  (push '(ag-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  ;; pt
  (push '(pt-search-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; pcre2el
  (push '(rxt-help-mode :position bottom :height 8) popwin:special-display-config)

  ;; IELM
  ;; (push '(inferior-emacs-lisp-mode :position bottom :height 10) popwin:special-display-config)
  ;; (push '("*ielm*" :position bottom :height 10) popwin:special-display-config)

  ;; Lisp
  (push '(inferior-lisp-mode :position bottom :height 8) popwin:special-display-config)
  (push '(sly-mrepl-mode :position bottom :height 8) popwin:special-display-config)
  (push '(slime-repl-mode :position bottom :height 8) popwin:special-display-config)
  (push '(slime-inspector-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '("*slime-description*" :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '("*slime-macroexpansion*" :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Clojure, CIDER
  (push '(inf-clojure-mode :position bottom :height 8) popwin:special-display-config)
  (push '(cider-clojure-interaction-mode :position bottom :height 8) popwin:special-display-config)
  (push '(cider-docvqiew-mode :position bottom :height 10 :noselect nil) popwin:special-display-config)
  (push '(cider-inspector-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; ESS
  (push '(inferior-ess-mode :position bottom :height 8) popwin:special-display-config)
  ;; *julia*
  (push '(inferior-julia-mode :position bottom :height 8) popwin:special-display-config)

  ;; yari Ruby document lookup
  (push '(yari-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  ;; rub-ruby - inf-ruby
  (push '(inf-ruby-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*ruby*" :position bottom :height 8) popwin:special-display-config)
  (push '("*pry*" :position bottom :height 8) popwin:special-display-config)
  (push '("*rails*" :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; projectile-rails
  (push '(projectile-rails-generate-mode :position bottom :height 8) popwin:special-display-config)
  (push '(projectile-rails-compilation-mode :position bottom :height 8) popwin:special-display-config)
  (push '(projectile-rails-server-mode :position bottom :height 8) popwin:special-display-config)

  ;; ruby-compilation-mode (RubyComp)
  (push '(ruby-compilation-mode :position bottom :height 8) popwin:special-display-config)

  ;; bundler
  (push '("*Bundler*" :position bottom :height 8) popwin:special-display-config)

  ;; Python
  ;; *Python*
  (push '(inferior-python-mode :position bottom :height 8) popwin:special-display-config)
  ;; *Anaconda*
  (push '(anaconda-mode-view-mode :position bottom :height 8) popwin:special-display-config)

  ;; jedi doc help
  (push '("*jedi:doc" :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '(rst-mode :position bottom :height 8) popwin:special-display-config)

  ;; JavaScript
  ;; js-comint: *js*
  (push '(inferior-js-mode :position bottom :height 8) popwin:special-display-config)

  ;; Go
  (push '(godoc-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Swift

  ;; Haskell
  (push '(inferior-haskell-mode :position bottom :height 8) popwin:special-display-config)
  (push '(haskell-interactive-mode :position bottom :height 8) popwin:special-display-config)

  ;; Lua
  (push '("*lua*" :position bottom :height 8) popwin:special-display-config)

  ;; Scala
  (push '(sbt-mode :position bottom :height 8) popwin:special-display-config)
  
  ;; gnuplot
  ;; (push '(gnuplot-comint-mode :position bottom :height 8) popwin:special-display-config)
  
  ;; octave help mode
  (push '(octave-help-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; SuperCollider
  ;; post buffer
  (push '("*SCLang:PostBuffer*" :position bottom :height 6 :noselect t) popwin:special-display-config)
  ;; workspace
  (push '("*SCLang:Workspace*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; calc -- Calculator
  (push '(calc-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '("*Calculator*" :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; eww
  (push '(eww-bookmark-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; checkdoc
  (push '("*Checkdoc Status*" :position bottom :height 8 :noselect nil) popwin:special-display-config)


  ;; TeX/LaTeX (AUCTeX)
  ;; (push '(TeX-output-mode :position bottom :height 10) popwin:special-display-config)
  (push '("*TeX Help*" :position bottom :height 8 :noselect nil) popwin:special-display-config)
  ;; auctex [C-c =] outline view.
  (push '(reftex-toc-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '("*toc*" :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Database: edbi
  ;; (push '("*edbi-dialog-ds*" :position bottom :height 8) popwin:special-display-config)
  (push '(ctbl:table-mode :position bottom :height 8 :noselect t) popwin:special-display-config)
  (push '("\\*edbi:query-result .*" :regexp t :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; howdoi
  (push '(howdoi-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*How do I*" :position bottom :height 8) popwin:special-display-config)

  ;; restclient
  (push '("*rest-client*" :position bottom :height 8) popwin:special-display-config)
  (push '("*HTTP Response*" :position bottom :height 8) popwin:special-display-config)

  ;; elfeed
  (push '(elfeed-search-mode :position top :height 8) popwin:special-display-config)
  (push '("*elfeed-search*" :position top :height 8) popwin:special-display-config)

  ;; mingus
  (push '(mingus-playlist-mode :position left) popwin:special-display-config)

  ;; vagrant
  (push '("*Vagrant*" :position bottom :height 6) popwin:special-display-config)

  ;; poporg
  (push '("*poporg:*" :position bottom :height 10 :noselect nil) popwin:special-display-config)

  (defun my/popwin-func-for-poporg-edit-window (buffer)
    "Match poporg popup edit buffer.

The `BUFFER' is the popwin catch poporg edit popup buffer"
    (let ((mode (with-current-buffer buffer
                  major-mode)))
      (and (string-match "\*poporg:\ .*\*" (buffer-name buffer))
           (eq mode 'org-mode))))

  (push '(my/popwin-func-for-poporg-edit-window :height 10 :position bottom :noselect nil) popwin:special-display-config)

  ;; ERC
  ;; This does not work. Because ERC does not use `pop-to-buffer' for private message buffer.
  (defun my/popwin-func-for-erc-private-message (buffer)
    "Match private messages which except channel buffers that start with a #.

The `BUFFER' is the popwin catch pop private message buffer."
    (let ((mode (with-current-buffer buffer
                  major-mode)))
      ;; string match `erc-pals' variables list.
      (and (string-match "^[^#]*" (buffer-name buffer))
           (eq mode 'erc-mode))))

  (push '(my/popwin-func-for-erc-private-message :height 10 :position bottom :noselect nil) popwin:special-display-config)
  )


;;; [ zoom-window ] -- zoom/un-zoom window like tmux.

(use-package zoom-window
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)  
  :config
  (setq zoom-window-mode-line-color "dark red"
        zoom-window-use-elscreen nil ; whether use extension elscreen.
        )
  )


(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
