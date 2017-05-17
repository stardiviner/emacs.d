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

;;; [ windmove ]

(use-package windmove
  :ensure t
  :config
  ;; (windmove-default-keybindings) ; default modifier is [Shift]
  (windmove-default-keybindings 'hyper) ; change modifier [H-]
  ;; (windmove-default-keybindings 'super) ; use [Command] key on Mac
  (setq windmove-wrap-around t) ; wrap around at edges
  )

;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
  :bind ("C-x C-j" . ace-window)
  :init
  ;; to re-override `dired-x''s `dired-bind-jump'. bind again after Emacs start.
  (with-eval-after-load 'dired-x
    (global-set-key (kbd "C-x C-j") 'ace-window))
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :background (color-darken-name (face-background 'default) 5)
                      :box "dark gray"
                      :height 2.5)
  
  (setq aw-background nil)
  (if aw-background
      (set-face-attribute 'aw-background-face nil
                          :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                        ('light
                                         (color-lighten-name (face-background 'default) 5))
                                        ('dark
                                         (color-darken-name (face-background 'default) 5)))
                          ))
  )


;;; [ resize-window ]

(use-package resize-window
  :ensure t
  :bind ("C-x w" . resize-window)
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

  (global-set-key (kbd "C-c C-b") popwin:keymap)

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

  ;; bm
  (push '(bm-show-mode :position bottom :height 5 :noselect nil) popwin:special-display-config)
  
  ;; Org-mode
  (push '("*Org-Babel Error Output*" :position bottom :height 8 :noselect t) popwin:special-display-config)
  (push '("*Org-Babel Results*" :position bottom :height 8 :noselect t) popwin:special-display-config)

  ;; Completion List (completion-list-mode)
  (push '(completion-list-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Occur Mode
  (push '("*Occur*" :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Compilation
  (push '(compilation-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  ;; quickrun "*quickrun*"
  (push '(quickrun/mode :position bottom :height 6 :noselect t) popwin:special-display-config)

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

  ;; Flycheck
  (push '("*Flycheck errors*" :position bottom :height 8) popwin:special-display-config)

  ;; process list
  (push '(process-menu-mode :position bottom :height 8) popwin:special-display-config)

  ;; pdf-tools
  (push '(pdf-occur-buffer-mode :position bottom :height 8) popwin:special-display-config)
  (push '(pdf-outline-buffer-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*PDF-Metadata*" :position bottom :height 8) popwin:special-display-config)

  ;; socyl
  (push '(socyl-search-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  ;; ag
  (push '(ag-mode :position bottom :height 10 :noselect nil) popwin:special-display-config)
  ;; pt
  (push '(pt-search-mode :position bottom :height 10 :noselect nil) popwin:special-display-config)

  ;; pcre2el
  (push '(rxt-help-mode :position bottom :height 8) popwin:special-display-config)

  ;; Lisp

  ;; Clojure, CIDER
  (push '(cider-clojure-interaction-mode :position bottom :height 8) popwin:special-display-config)
  (push '(cider-repl-mode :position bottom :height 8) popwin:special-display-config)
  (push '(cider-inspector-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; ESS
  (push '(inferior-ess-mode :position bottom :height 8) popwin:special-display-config)
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
  (push '(inferior-python-mode :position bottom :height 8) popwin:special-display-config)
  (push '("*jedi:doc" :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '(rst-mode :position bottom :height 8) popwin:special-display-config)

  ;; JavaScript
  (push '(inferior-js-mode :position bottom :height 8) popwin:special-display-config)

  ;; Go
  (push '(godoc-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)
  (push '(go-guru-output-mode :position bottom :height 8 :noselect nil) popwin:special-display-config)

  ;; Swift

  ;; Haskell
  (push '(inferior-haskell-mode :position bottom :height 8) popwin:special-display-config)
  (push '(haskell-interactive-mode :position bottom :height 8) popwin:special-display-config)
  (push '(haskell-compilation-mode :position bottom :height 8) popwin:special-display-config)

  ;; SuperCollider
  (push '("*SCLang:PostBuffer*" :position bottom :height 6 :noselect t) popwin:special-display-config)
  (push '("*SCLang:Workspace*" :position bottom :height 6 :noselect t) popwin:special-display-config)

  ;; TeX/LaTeX (AUCTeX)
  (push '(TeX-output-mode :position bottom :height 10) popwin:special-display-config)
  (push '("*TeX Help*" :position bottom :height 8 :noselect nil) popwin:special-display-config)
  )

;;; [ golden-ratio ] -- automatically resizes your windows so that the window containing the point is the largest (size determined by the mathematical golden ratio.

;; (use-package golden-ratio
;;   :ensure t
;;   :diminish golden-ratio-mode
;;   :config
;;   (golden-ratio-mode 1))

;;; [ zoom-window ] -- zoom/un-zoom window like tmux.

(use-package zoom-window
  :ensure t
  :bind ("C-x C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "dark red"
        zoom-window-use-elscreen nil ; whether use extension elscreen.
        )
  )


(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
