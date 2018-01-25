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
;; - (display-buffer-in-side-window)
;; - (display-buffer-no-window)
;; - (display-buffer-reuse-window display-buffer-below-selected) (window-height . 0.3)
;; - (display-buffer-reuse-window display-buffer-same-window)
(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Pp Eval Output\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Backtrace\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Process List\\*" (display-buffer-below-selected)))

;;; [ winner ]

(use-package winner
  :config
  (winner-mode 1))

;;; [ golden-ratio ] -- automatic resizing of Emacs windows to the golden ratio.

(use-package golden-ratio
  :ensure t
  :config
  (setq golden-ratio-auto-scale t
        golden-ratio-adjust-factor 1.0
        golden-ratio-wide-adjust-factor 0.8
        golden-ratio-recenter t)

  ;; add window navigation commands to `golden-ratio' triggers list.
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(window-number-select ace-window)))

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

  (setq golden-ratio-exclude-buffer-regexp '("\\`\\*.*?\\*\\'")) ; *...* buffers
  ;; "\\`\\*[Hh]elm.*\\*\\'"
  (setq golden-ratio-exclude-buffer-names '(" *Org todo*" " *Org tags*"))
  (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")
  
  ;; manually re-fit ratio.
  ;; (global-set-key (kbd "C-C C-j") 'golden-ratio)

  ;; fix `ediff' buffers alignment issue.
  (add-hook 'ediff-before-setup-windows-hook #'(lambda () (golden-ratio-mode -1)))
  (add-hook 'ediff-quit-hook #'(lambda () (golden-ratio-mode 1)))
  
  (golden-ratio-mode 1)
  )

;;; [ ace-window ] -- Quickly switch windows in Emacs.

(use-package ace-window
  :ensure t
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
