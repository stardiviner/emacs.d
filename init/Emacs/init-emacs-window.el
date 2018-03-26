;;; init-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

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
  (setq aw-dispatch-always t))

;;; [ follow-mode ] -- [C-c .] same buffer different windows auto following in large screen.

(require 'follow)
(add-hook 'follow-mode-hook #'split-window-horizontally)



(provide 'init-emacs-window)

;;; init-emacs-window.el ends here
