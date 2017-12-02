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


;;; [ display-buffer-alist ]

;; Learn about display actions, see [[info:elisp#Display Action Functions]].

(add-to-list 'display-buffer-alist
             '("^*Async Shell Command*" . (display-buffer-no-window)))


;;; [ winner ]

(use-package winner
  :config
  (winner-mode 1))

;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
  :bind ("C-x C-j" . ace-window)
  :init
  (with-eval-after-load 'dired-x
    (global-set-key (kbd "C-x C-j") 'ace-window))
  :config
  (setq aw-background nil)
  (set-face-attribute 'aw-leading-char-face nil
                      :height 2.5 :weight 'bold)
  )


;;; [ zoom-window ] -- zoom/un-zoom window like tmux.

;; (use-package zoom-window
;;   :ensure t
;;   :bind ("C-x C-z" . zoom-window-zoom)
;;   :config
;;   (setq zoom-window-mode-line-color "dark red"
;;         zoom-window-use-elscreen nil ; whether use extension elscreen.
;;         )
;;   )

;;; [ zoom ] -- like `golden-ratio' style managing the window sizes by enforcing a fixed and automatic balanced layout.

;; (use-package zoom
;;   :ensure t
;;   ;; Override the key binding of `balance-windows':
;;   ;; enable this keybinding when `zoom-mode' is disabled.
;;   ;; :bind ("C-x +" . zoom)
;;   :config
;;   ;; Resize the selected window using the golden ratio:
;;   ;; (setq zoom-size '(0.618 . 0.618))
;;
;;   ;; Resize the selected window according to the frame width:
;;   (setq zoom-size
;;         (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
;;               (t                            '(0.5 . 0.5))))
;;
;;   (setq zoom-ignored-major-modes '(which-key-mode))
;;   (setq zoom-ignored-buffer-names '("zoom.el" "init.el"))
;;   (setq zoom-ignored-buffer-name-regexps '("^*calc"))
;;   (setq zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
;;
;;   (zoom-mode t)
;;   )



(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
