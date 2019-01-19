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
;; - (display-buffer-in-side-window . ((side . bottom) (window-height . 4)))
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
  :init (winner-mode 1))

;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
  :defer t
  :delight ace-window-mode
  :bind ("C-x C-j" . ace-window)
  :config
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  (set-face-attribute 'aw-leading-char-face nil
                      :height 200))

;;; manipulate windows

(use-package hydra
  :ensure t
  :ensure ace-window
  :config
  (global-set-key
   (kbd "C-x C-z")
   (defhydra hydra-window-menu ()
     "window"
     ("h" windmove-left   :color black)
     ("j" windmove-down   :color black)
     ("k" windmove-up     :color black)
     ("l" windmove-right  :color black)
     ("s" split-window-vertically   "split |" :color cyan)
     ("v" split-window-horizontally "split --" :color cyan)
     ("^" enlarge-window "enlarge" :color blue)
     ("V" shrink-window "shrink" :color blue)
     ("}" enlarge-window-horizontally "enlarge" :color blue)
     ("{" shrink-window-horizontally "shrink" :color blue)
     ("b" balance-windows "balance" :color blue)
     ("f" my-turn-current-window-into-new-frame "new-frame" :color blue)
     ("o" delete-other-windows "only-one" :color red)
     ("d" delete-window "delete" :color red)
     ("s" ace-swap-window "swap" :color yellow)
     ("q" nil "quit"))))

;;; [ follow-mode ] -- [C-c .] same buffer different windows auto following in large screen.

(require 'follow)
(add-hook 'follow-mode-hook #'split-window-horizontally)



(provide 'init-emacs-window)

;;; init-emacs-window.el ends here
