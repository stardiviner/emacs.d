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

;; Actions:
;; display-buffer actions:
;; - (display-buffer-same-window)
;; - (display-buffer-in-side-window) :: like which-key popup window upon bottom minibuffer.
;; - (display-buffer-no-window)
;; - (display-buffer-in-child-frame) :: don't use it!!!
;; - (display-buffer-reuse-window (window-height . 0.3))
;; - (display-buffer-reuse-window display-buffer-same-window)
(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Pp Eval Output\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Backtrace\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Process List\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Process List\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Animation\\*" (display-buffer-below-selected)))

;;; [ winner ]

(use-package winner
  :ensure t
  :defer t
  :init
  (winner-mode 1))

;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
  :defer t
  :bind ("C-x C-j" . ace-window)
  :config
  (setq aw-background nil)
  (set-face-attribute 'aw-leading-char-face nil
                      :height 2.5 :weight 'bold)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "cyan")
  )

;;; [ follow-mode ] -- [C-c .] same buffer different windows auto following in large screen.

(require 'follow)
(add-hook 'follow-mode-hook #'split-window-horizontally)



(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
