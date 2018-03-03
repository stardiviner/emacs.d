;;; init-prog-comment.el --- init Comment settings for Programming in Emacs

;;; Commentary:


;;; Code:

(unless (boundp 'prog-comment-prefix)
  (define-prefix-command 'prog-comment-prefix))
(global-set-key (kbd "M-;") 'prog-comment-prefix)


(setq comment-auto-fill-only-comments t
      comment-multi-line t
      )


;;; prefix: [M-;], `prog-comment-prefix'

(define-key prog-comment-prefix (kbd "M-;") 'comment-dwim)
;; or with [C-u N]
(global-set-key (kbd "C-x C-;") 'comment-line)
(define-key prog-comment-prefix (kbd "r") 'comment-or-uncomment-region)
(define-key prog-comment-prefix (kbd "l") 'comment-line)
(define-key prog-comment-prefix (kbd "b") 'comment-box)
(define-key prog-comment-prefix (kbd "B") 'comment-box-with-fill-column)

(defun comment-box-with-fill-column (b e) ; begin, end
  "Draw a box comment around the region of B and E.

But arrange for the region to extend to at least the fill
column.  Place the point after the comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ? (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))



(unless (boundp 'comment-tag-prefix)
  (define-prefix-command 'comment-tag-prefix))
(global-set-key (kbd "M-g c") 'comment-tag-prefix)

;;; [ comment-tags ] -- Emacs package to highlight and manage comment tags like TODO, BUG, FIXME, etc.

(use-package comment-tags
  :ensure t
  :init
  (setq comment-tags-keymap-prefix (kbd "M-g c"))
  :config
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "orange"))
          ("FIXME" . ,(list :weight 'bold :foreground "red"))
          ("BUG" . ,(list :weight 'bold :foreground "red"))
          ("HACK" . ,(list :weight 'bold :foreground "cyan"))
          ("KLUDGE" . ,(list :weight 'bold :foreground "forest green"))
          ))

  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)

  (add-hook 'prog-mode-hook 'comment-tags-mode)
  )

;; [ poporg ] -- Editing program comments or strings in text mode.

(use-package poporg
  :ensure t
  :bind (("C-c '" . poporg-dwim)
         :map prog-comment-prefix
         ("'" . poporg-dwim))
  :init
  ;; display poporg popup buffer below the selected window with 0.3 height.
  (add-to-list 'display-buffer-alist
               '("\\*poporg:\ .*?\\*" ; *poporg: init-emacs-window.el*
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height . 0.3)
                 ))
  :config
  (setq poporg-adjust-fill-column t
        poporg-delete-trailing-whitespace t)
  (defun my-poporg-set-face (theme)
    "Reload customized faces on `circadian' `THEME' toggling."
    (set-face-attribute 'poporg-edited-face nil
                        :foreground "chocolate"
                        :background (cl-case (alist-get 'background-mode (frame-parameters))
                                      ('light
                                       (color-darken-name (face-background 'default) 10))
                                      ('dark
                                       (color-darken-name (face-background 'default) 5)))))
  (add-hook 'circadian-after-load-theme-hook #'my-poporg-set-face)

  (define-key poporg-mode-map [remap save-buffer] 'poporg-edit-exit)
  )


;;; [ org-commentary ] -- generate/update conventional library headers using Org-mode.

;; (use-package org-commentary
;;   :ensure t
;;   )


(provide 'init-prog-comment)

;;; init-prog-comment.el ends here
