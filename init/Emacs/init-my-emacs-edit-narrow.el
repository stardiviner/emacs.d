;;; init-my-emacs-edit-narrow.el --- init for Narrow
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Narrowing ]

;;; Usage:
;;
;; - prefix --> [C-x n]
;; - [C-x n n] -- narrow to region
;; - [C-x n w] -- widen (undo narrow)

;;; don't disable narrowing functions
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)

(unless (boundp 'my-narrow-prefix)
  (define-prefix-command 'my-narrow-prefix))
(define-key my-edit-prefix (kbd "n") 'my-narrow-prefix)

(define-key my-narrow-prefix (kbd "w") 'widen)
(define-key my-narrow-prefix (kbd "r") 'narrow-to-region)
(define-key my-narrow-prefix (kbd "d") 'narrow-to-defun)
(define-key my-narrow-prefix (kbd "p") 'narrow-to-page)

;;; custom keybinding for handy (narrow + indirect-buffer)
;;
;; Usage: [C-x n i], you can kill narrowed indirect buffer like normal buffer with [C-x k]. the modification will keep.
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

;; (global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)
(define-key my-narrow-prefix (kbd "i") 'narrow-to-region-indirect)


(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

(define-key narrow-map (kbd "r") 'narrow-to-region) ; backup `narrow-to-region'.
(define-key narrow-map (kbd "n") #'narrow-or-widen-dwim)
(define-key my-narrow-prefix (kbd "n") #'narrow-or-widen-dwim)

;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
;; (define-key ctl-x-map "n" #'narrow-or-widen-dwim)


;;; [ fancy-narrow ] -- immitate `narrow-to-region' with more eye-candy.

(use-package fancy-narrow
  :ensure t
  :config
  (set-face-attribute 'fancy-narrow-blocked-face nil
                      :foreground "#666666"
                      :background (color-darken-name (face-background 'default) 2)
                      )

  (global-set-key [remap narrow-to-region] 'fancy-narrow-to-region)
  (global-set-key [remap narrow-to-defun] 'fancy-narrow-to-defun)
  (global-set-key [remap narrow-to-page] 'fancy-narrow-to-page)
  (global-set-key [remap widen] 'fancy-widen)
  )


;;;_ Multiple Narrowings


;;;_ narrow-indirect



;;;_ narrow-reindent


;;;_ narrowed-page-navigation


;;;_ recursive-narrow




(provide 'init-my-emacs-edit-narrow)

;;; init-my-emacs-edit-narrow.el ends here
