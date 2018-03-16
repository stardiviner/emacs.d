;;; init-emacs-edit-narrow.el --- init for Narrow
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'narrow-prefix)
  (define-prefix-command 'narrow-prefix))
(define-key editing-prefix (kbd "n") 'narrow-prefix)

;;; [ narrow ]

;;; don't disable narrowing functions
(put 'narrow-to-region 'disabled nil)     ; [C-x n n]
(put 'narrow-to-page 'disabled nil)       ; [C-x n p]
(put 'narrow-to-defun 'disabled nil)      ; [C-x n d]
(put 'upcase-region 'disabled nil)        ; [C-x C-u]
(put 'downcase-region 'disabled nil)      ; [C-x C-l]

(define-key narrow-map (kbd "r") 'narrow-to-region)
(define-key narrow-map (kbd "n") 'narrow-or-widen-dwim)

(define-key narrow-prefix (kbd "w") 'widen)
(define-key narrow-prefix (kbd "r") 'narrow-to-region)
(define-key narrow-prefix (kbd "d") 'narrow-to-defun)
(define-key narrow-prefix (kbd "p") 'narrow-to-page)

;;; custom keybinding for handy (narrow + indirect-buffer)
(defun narrow-to-region-indirect (start end)
  "Restrict editing in buffer on region `START' and `END' indirectly.
You can kill narrowed indirect buffer like normal buffer with \\<kill-buffer>.
And the modification will keep."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(define-key narrow-prefix (kbd "i") 'narrow-to-region-indirect)


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

(define-key narrow-prefix (kbd "n") #'narrow-or-widen-dwim)

;;; [ fancy-narrow ] -- immitate `narrow-to-region' with more eye-candy.

(use-package fancy-narrow
  :ensure t
  :defer t
  :bind (([remap narrow-to-region] . fancy-narrow-to-region)
         ([remap narrow-to-defun] . fancy-narrow-to-defun)
         ([remap narrow-to-page] . fancy-narrow-to-page)
         ([remap widen] . fancy-widen)))


(provide 'init-emacs-edit-narrow)

;;; init-emacs-edit-narrow.el ends here
