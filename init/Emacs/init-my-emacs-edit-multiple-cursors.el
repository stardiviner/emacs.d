;;; init-my-emacs-edit-multiple-cursors.el --- init for multiple cursors
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'mc-prefix)
  (define-prefix-command 'mc-prefix))
(define-key my-edit-prefix (kbd "c") 'mc-prefix)

;;;_ [ iedit ] -- Edit multiple regions simultaneously in a buffer or a region

(use-package iedit
  :ensure t
  :bind (:map my-edit-prefix
	      ("e" . iedit-dwim)
	      ("C-;" . iedit-mode)
	      )
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
  :bind (
         ;; ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ;; ("M-<mouse-1>" . mc/add-cursor-on-click)
         :map mc-prefix
              ("a" . mc/mark-all-dwim)
              ("r" . set-rectangular-region-anchor)
              ("l" . mc/edit-lines)
              ("a" . mc/edit-beginnings-of-lines)
              ("e" . mc/edit-ends-of-lines)
              ("n" . mc/insert-numbers)
              ("s" . mc/sort-regions)
              ("R" . mc/reverse-regions)
              )
  :init
  (unless (boundp 'mc/mark-prefix)
    (define-prefix-command 'mc/mark-prefix))
  (define-key mc-prefix (kbd "m") 'mc/mark-prefix)

  (define-key mc/mark-prefix (kbd "a a") 'mc/mark-all-like-this-dwim)
  (define-key mc/mark-prefix (kbd "a l") 'mc/mark-all-like-this)
  (define-key mc/mark-prefix (kbd "a w") 'mc/mark-all-words-like-this)
  (define-key mc/mark-prefix (kbd "a s") 'mc/mark-all-symbols-like-this)
  (define-key mc/mark-prefix (kbd "a r") 'mc/mark-all-in-region)
  (define-key mc/mark-prefix (kbd "a f") 'mc/mark-all-like-this-in-defun)
  (define-key mc/mark-prefix (kbd "a F") 'mc/mark-all-words-like-this-in-defun)
  (define-key mc/mark-prefix (kbd "a S") 'mc/mark-all-symbols-like-this-in-defun)
  (define-key mc/mark-prefix (kbd "t") 'mc/mark-sgml-tag-pair)

  (define-key mc/mark-prefix (kbd "n n") 'mc/mark-next-like-this)
  (define-key mc/mark-prefix (kbd "n w") 'mc/mark-next-word-like-this)
  (define-key mc/mark-prefix (kbd "n s") 'mc/mark-next-symbol-like-this)
  (define-key mc/mark-prefix (kbd "p p") 'mc/mark-previous-like-this)
  (define-key mc/mark-prefix (kbd "p w") 'mc/mark-previous-word-like-this)
  (define-key mc/mark-prefix (kbd "p s") 'mc/mark-previous-symbol-like-this)

  (if (featurep 'visual-regexp)
      (define-key mc/mark-prefix (kbd "v") 'vr/mc-mark))
  ;; `vr/select-mc-mark', `vr/select-replace', `vr/select-query-replace' etc.

  :config
  (setq mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory))
  ;; (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode)
  
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
