;;; init-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

;;; [ display-buffer-alist ] ;; apply actions on `display-buffer'

;; Learn about display actions, see [[info:elisp#Display Action Functions]].

;; `display-buffer' actions:
;;
;; - (display-buffer-same-window)
;; - (display-buffer-in-side-window) :: like which-key popup window upon bottom minibuffer.
;; - (display-buffer-no-window)
;; - (display-buffer-in-child-frame) :: don't use it!!!
;; - (display-buffer-reuse-window (window-height . 0.3))
;; - (display-buffer-reuse-window display-buffer-same-window)
;; - (display-buffer-in-side-window ((side . bottom) (window-height . 4)))
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
  :init (setq aw-background nil))

;;; manipulate windows

(defun my:turn-current-window-into-new-frame ()
  "Popup current window to another new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(global-set-key (kbd "C-x 5 5") 'my:turn-current-window-into-new-frame)

(use-package hydra
  :ensure t
  :ensure ace-window
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

;;; [ golden-ratio ] -- Automatic resizing of Emacs windows to the golden ratio.

(use-package golden-ratio
  :ensure t
  :defer t
  :commands (golden-ratio-mode golden-ratio)
  :init (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale nil
        golden-ratio-recenter t)
  :config
  ;; Exclude following pattern buffers.
  (setq golden-ratio-exclude-modes
        (append golden-ratio-exclude-modes
                '(ediff-mode
                  calendar-mode calc-mode dired-mode
                  speedbar-mode project-explorer-mode
                  gnus-summary-mode gnus-article-mode
                  mu4e-headers-mode mu4e-compose-mode
                  restclient-mode)))

  (setq golden-ratio-exclude-buffer-regexp '("\\`\\*.*?\\*\\'")) ; *...* buffers
  (add-to-list 'golden-ratio-exclude-buffer-names "*rg*")
  (add-to-list 'golden-ratio-exclude-buffer-names " *Org todo*")
  (add-to-list 'golden-ratio-exclude-buffer-names " *Org tags*")
  (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")

  ;; for `popwin'.
  ;; (setq golden-ratio-inhibit-functions '(pop-to-buffer))
  ;; add `window-number' and `ace-window' commands to trigger list.
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands '(window-number-select ace-window)))

  ;; disable in ediff session.
  (add-hook 'ediff-before-setup-windows-hook #'(lambda () (golden-ratio-mode -1)))
  (add-hook 'ediff-quit-hook #'(lambda () (golden-ratio-mode 1)))

  ;; manually re-fit ratio.
  ;; (global-set-key (kbd "C-C C-j") 'golden-ratio)

  ;; fix golden-ratio conflict with eyebrowse.
  (defun golden-ratio-eyebrowse-workaround--advice (orig-fun &rest args)
    (golden-ratio-mode -1)
    (apply orig-fun args)
    (golden-ratio-mode 1))
  (advice-add 'eyebrowse-last-window-config :around #'golden-ratio-eyebrowse-workaround--advice)
  (advice-add 'eyebrowse-switch-to-window-config :around #'golden-ratio-eyebrowse-workaround--advice))


;;; [ follow-mode ] -- [C-c .] same buffer different windows auto following in large screen.

(use-package follow
  :defer t
  :config (add-hook 'follow-mode-hook #'split-window-horizontally))



(provide 'init-emacs-window)

;;; init-emacs-window.el ends here
