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

;;; [ window-purpose ] -- Purpose-based window management for Emacs.

;; (use-package window-purpose
;;   :ensure t
;;   :config
;;   (setq purpose-preferred-prompt 'ivy
;;         purpose-layout-dirs (locate-user-emacs-file ".purpose/layouts/")
;;         )
;;
;;   ;; (setq pop-up-frames t)
;;
;;   ;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;;   ;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;;   ;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;;   ;;
;;   ;; - popup-window
;;   ;; - sidebar-window
;;   ;; - help-window
;;   ;; - search-window
;;   ;; - compilation-window
;;   ;; - repl-window
;;   ;; - utility-window
;;
;;   (add-to-list 'purpose-user-mode-purposes '(popwin-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(compilation-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(comint-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(help-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(apropos-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(xref--xref-buffer-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(Man-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(ag-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(pt-search-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(dired-mode . sidebar-window))
;;   (add-to-list 'purpose-user-mode-purposes '(project-explorer-mode . sidebar-window))
;;   (add-to-list 'purpose-user-mode-purposes '(bm-show-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(process-menu-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(quickrun/mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(pdf-occur-buffer-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(pdf-outline-buffer-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-lisp-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(sly-mrepl-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(slime-repl-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(slime-inspector-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inf-clojure-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(cider-clojure-interaction-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(cider-docview-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(cider-inspector-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-ess-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-julia-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(yari-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inf-ruby-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(ruby-compilation-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(projectile-rails-generate-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(projectile-rails-compilation-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(projectile-rails-server-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-js-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-haskell-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(haskell-interactive-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(sbt-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(calc-mode . utility-window))
;;   (add-to-list 'purpose-user-mode-purposes '(godoc-mode . help-window))
;;
;;
;;   ;; (setq purpose-special-action-sequences '(purpose-display-reuse-window-buffer
;;   ;;                                          purpose-display-reuse-window-purpose
;;   ;;                                          purpose-display-pop-up-frame
;;   ;;                                          popup-frame)
;;   ;;       )
;;
;;   (setq purpose-use-default-configuration t)
;;
;;   (purpose-compile-user-configuration)
;;
;;   (purpose-mode)
;;   )



(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
