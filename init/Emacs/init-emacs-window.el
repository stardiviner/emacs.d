;;; init-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

;;; [ display-buffer-alist ] ;; apply actions on `display-buffer'

;; Learn about display actions, see [[info:elisp#Display Action Functions]].

;; `display-buffer' actions, reference `display-buffer--action-function-custom-type'.
;;
;; - (display-buffer-below-selected)
;; - (display-buffer-same-window)
;; - (display-buffer-at-bottom)
;; - (display-buffer-in-side-window) :: like which-key popup window upon bottom minibuffer.
;; - (display-buffer-no-window)
;; - (display-buffer-in-child-frame) :: don't use it!!!
;; - (display-buffer-reuse-window (window-height . 0.3))
;; - (display-buffer-reuse-window display-buffer-same-window)
;; - (display-buffer-in-side-window ((side . bottom) (window-height . 4)))

;; (add-to-list 'display-buffer-alist
;;              '("^ \\*undo-tree\\*"
;;                display-buffer-reuse-window display-buffer-in-side-window
;;                (reusable-frames . visible)
;;                (side . right)
;;                (slot . 1)
;;                (window-sides-vertical . t)
;;                (window-width . 0.5)
;;                (window-height . 0.15)))
;;
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*Flycheck errors*" eos)
;;                (display-buffer-reuse-window
;;                 display-buffer-in-side-window)
;;                (side            . bottom)
;;                (reusable-frames . visible)
;;                (window-height   . 0.15)))

(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Pp Eval Output\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Backtrace\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Process List\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Shell Command Output\\*" (display-buffer-no-window)))
(add-to-list 'display-buffer-alist
             '("^\\*Async Shell Command\\*" (display-buffer-no-window)))
(add-to-list 'display-buffer-alist
             '("^\\*Animation\\*" (display-buffer-below-selected)))
;; (add-to-list 'display-buffer-alist
;;              '("^\\*Org Src" . (display-buffer-at-bottom)))

;;; [ winner ] -- Restore old window configurations.

;; (use-package winner
;;   :ensure t
;;   :defer t
;;   :init (winner-mode 1))

;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
  :defer t
  :delight ace-window-mode
  :bind ("C-x C-j" . ace-window)
  :init (setq aw-background nil)
  :config (with-eval-after-load 'eldoc ; refresh eldoc after ace-window selecting window.
            (eldoc-add-command 'ace-window)))

;;; manipulate windows

(defun my-turn-current-window-into-new-frame ()
  "Popup current window to another new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(global-set-key (kbd "C-x 5 5") 'my-turn-current-window-into-new-frame)

(use-package hydra
  :ensure t
  :ensure ace-window
  :defer t
  :config
  ;; `hydra-frame-window' is designed from `ace-window' and
  ;; matches aw-dispatch-alist with a few extra
  (defhydra hydra-frame-window (:color red :hint nil)
    "
^Frame^                 ^Window^      Window Size^^^^^^    ^Text Zoom^               (__)
_0_: delete             _t_oggle        ^ ^ _k_ ^ ^            _=_                   (oo)
_1_: delete others      _s_wap          _h_ ^+^ _l_            ^+^             /------\\/
_2_: new                _d_elete        ^ ^ _j_ ^ ^            _-_            / |    ||
_F_ullscreen            _f_rame         _b_alance^^^^          ^ ^        *  /\\---/\\  ~~  C-c w/C-x o w
"
    ("0" delete-frame :exit t)
    ("1" delete-other-frames :exit t)
    ("2" make-frame  :exit t)
    ("b" balance-windows)
    ("s" ace-swap-window)
    ("F" toggle-frame-fullscreen)
    ("t" toggle-window-split)
    ("d" delete-window :exit t)
    ("f" my:turn-current-window-into-new-frame :exit t)
    ("-" text-scale-decrease)
    ("=" text-scale-increase)
    ("h" shrink-window-horizontally)
    ("k" shrink-window)
    ("j" enlarge-window)
    ("l" enlarge-window-horizontally)
    ("q" nil "quit"))
  (global-set-key (kbd "C-x C-z") #'hydra-frame-window/body)
  (with-eval-after-load 'ace-window
    (add-to-list 'aw-dispatch-alist '(?w hydra-frame-window/body) t)))

;;; [ zoom ] -- Fixed and automatic balanced window layout.

(use-package zoom
  :ensure t
  :hook (after-init . zoom-mode)
  :config (setq zoom-size '(0.618 . 0.618))
  (add-to-list 'zoom-ignored-major-modes 'helm-major-mode))

;;; [ follow-mode ] -- [C-c .] same buffer different windows auto following in large screen.

(use-package follow
  :defer t
  :config (add-hook 'follow-mode-hook #'split-window-horizontally))



(provide 'init-emacs-window)

;;; init-emacs-window.el ends here
