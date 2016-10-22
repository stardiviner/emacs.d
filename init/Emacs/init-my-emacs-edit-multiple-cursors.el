;;; init-my-emacs-edit-multiple-cursors.el --- init for multiple cursors
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ [ iedit ] -- Edit multiple regions simultaneously in a buffer or a region

(use-package iedit
  :ensure t
  :defer t
  :init
  ;; (global-set-key (kbd "C-;") 'iedit-mode)
  (define-key my-edit-prefix (kbd "e") 'iedit-dwim)

  :config
  (setq iedit-occurrence-face 'isearch) ; 'highlight

  (defun iedit-dwim (arg)
    "If ARG, start iedit but use \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max)))))))
  )

;;;_ [ multiple-cursors ]

(use-package multiple-cursors
  :ensure t
  :defer t
  :init
  (unless (boundp 'my-mc-prefix)
    (define-prefix-command 'my-mc-prefix))
  (define-key my-edit-prefix (kbd "c") 'my-mc-prefix)

  (define-key my-mc-prefix (kbd "c") 'mc/mark-all-dwim)
  (define-key my-mc-prefix (kbd "r") 'set-rectangular-region-anchor)
  (define-key my-mc-prefix (kbd "l") 'mc/edit-lines)
  (define-key my-mc-prefix (kbd "a") 'mc/edit-beginnings-of-lines)
  (define-key my-mc-prefix (kbd "e") 'mc/edit-ends-of-lines)
  (define-key my-mc-prefix (kbd "n") 'mc/insert-numbers)
  (define-key my-mc-prefix (kbd "s") 'mc/sort-regions)
  (define-key my-mc-prefix (kbd "R") 'mc/reverse-regions)

  (unless (boundp 'my-mc/mark-prefix)
    (define-prefix-command 'my-mc/mark-prefix))
  (define-key my-mc-prefix (kbd "m") 'my-mc/mark-prefix)

  (define-key my-mc/mark-prefix (kbd "a a") 'mc/mark-all-like-this-dwim)
  (define-key my-mc/mark-prefix (kbd "a l") 'mc/mark-all-like-this)
  (define-key my-mc/mark-prefix (kbd "a w") 'mc/mark-all-words-like-this)
  (define-key my-mc/mark-prefix (kbd "a s") 'mc/mark-all-symbols-like-this)
  (define-key my-mc/mark-prefix (kbd "a r") 'mc/mark-all-in-region)
  (define-key my-mc/mark-prefix (kbd "a f") 'mc/mark-all-like-this-in-defun)
  (define-key my-mc/mark-prefix (kbd "a F") 'mc/mark-all-words-like-this-in-defun)
  (define-key my-mc/mark-prefix (kbd "a S") 'mc/mark-all-symbols-like-this-in-defun)
  (define-key my-mc/mark-prefix (kbd "t") 'mc/mark-sgml-tag-pair)

  (define-key my-mc/mark-prefix (kbd "n n") 'mc/mark-next-like-this)
  (define-key my-mc/mark-prefix (kbd "n w") 'mc/mark-next-word-like-this)
  (define-key my-mc/mark-prefix (kbd "n s") 'mc/mark-next-symbol-like-this)
  (define-key my-mc/mark-prefix (kbd "p p") 'mc/mark-previous-like-this)
  (define-key my-mc/mark-prefix (kbd "p w") 'mc/mark-previous-word-like-this)
  (define-key my-mc/mark-prefix (kbd "p s") 'mc/mark-previous-symbol-like-this)

  (if (featurep 'visual-regexp)
      (define-key my-mc/mark-prefix (kbd "v") 'vr/mc-mark))
  ;; `vr/select-mc-mark', `vr/select-replace', `vr/select-query-replace' etc.

  ;; click on multiple-cursors
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  
  :config
  ;; (setq mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory))

  ;; (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode)
  
  ;; First mark the word, then add more cursors.

  ;; To get out of multiple-cursors-mode, press <return> or C-g. The latter will
  ;; first disable multiple regions before disabling multiple cursors. If you want
  ;; to insert a newline in multiple-cursors-mode, use [C-j].

  ;; (setq mc/mode-line '("mc:"
  ;;                      (:eval
  ;;                       (format
  ;;                        #("%d" 0 2
  ;;                          (face font-lock-warning-face))
  ;;                        (mc/num-cursors)))))

  (set-face-attribute 'mc/cursor-face nil
                      :inverse-video nil
                      :background "dark red")
  (set-face-attribute 'mc/region-face nil
                      :inverse-video nil
                      :background (color-darken-name (face-background 'default) 4))

  ;; (setq mc/keymap "C-c c")
  (define-key mc/keymap (kbd "C-'") 'mc-hide-unmatched-lines-mode)
  )



(provide 'init-my-emacs-edit-multiple-cursors)

;;; init-my-emacs-edit-multiple-cursors.el ends here
