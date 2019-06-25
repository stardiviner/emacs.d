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

;;; [ hl-todo ] -- highlight TODO and similar keywords.

(use-package hl-todo
  :ensure t
  :defer t
  :commands (hl-todo-next hl-todo-previous hl-todo-occur)
  :bind (:map comment-tag-prefix
              ("n" . hl-todo-next)
              ("p" . hl-todo-previous)
              ("o" . hl-todo-occur)
              ("i" . hl-todo-insert-keyword))
  :init (add-hook 'prog-mode-hook #'hl-todo-mode)
  :config
  (add-to-list 'hl-todo-keyword-faces '("PERFORMANCE" . "#5f7f5f"))
  (add-to-list 'hl-todo-activate-in-modes 'conf-mode 'append))

;; [ poporg ] -- Editing program comments or strings in text mode.

(use-package poporg
  :ensure t
  :bind (("C-c '" . poporg-dwim)
         :map prog-comment-prefix ("'" . poporg-dwim)
         :map poporg-mode-map ([remap save-buffer] . poporg-edit-exit))
  :init
  ;; display poporg popup buffer below the selected window with 0.3 height.
  (add-to-list 'display-buffer-alist
               '("\\*poporg:\ .*?\\*" ; *poporg: init-emacs-window.el*
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height . 0.3)
                 ))
  :config
  (set-face-attribute 'poporg-edited-face nil
                      :foreground "chocolate"
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light
                                     (color-darken-name (face-background 'default) 10))
                                    ('dark
                                     (color-darken-name (face-background 'default) 5))))
  )


;;; [ org-commentary ] -- insert/generate/update conventional library headers using Org-mode.

(use-package org-commentary
  :ensure t
  :commands (org-commentary-update))

;;; [ banner-comment ] -- turn a comment into a banner.

(use-package banner-comment
  :ensure t
  :commands (banner-comment)
  :bind (:map prog-comment-prefix ("M-b" . banner-comment)))


(provide 'init-prog-comment)

;;; init-prog-comment.el ends here
