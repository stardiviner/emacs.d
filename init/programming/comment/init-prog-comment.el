;;; init-prog-comment.el --- init Comment settings for Programming in Emacs

;;; Commentary:


;;; Code:

(unless (boundp 'prog-comment-prefix)
  (define-prefix-command 'prog-comment-prefix))
(global-set-key (kbd "M-;") 'prog-comment-prefix)
(global-set-key (kbd "M-g c") 'prog-comment-prefix)

;;; [ newcomment ] -- (un)comment regions of buffers.

(use-package newcomment
  :custom ((comment-auto-fill-only-comments t)
           (comment-multi-line t))
  ;; or with [C-u N]
  :bind (("C-x C-;" . comment-line)
         :map prog-comment-prefix
         ("M-;" . comment-dwim)
         ("r" . comment-or-uncomment-region)
         ("l" . comment-line)
         ("B" . comment-box)
         ("b" . comment-box-with-fill-column))
  :init
  (defun comment-box-with-fill-column (begin end)
    "Draw a box comment around the region of B and E.

But arrange for the region to extend to at least the fill
column.  Place the point after the comment box."
    (interactive "r")
    (if (region-active-p)
        (let ((end (copy-marker end t)))
          (goto-char begin)
          (end-of-line)
          (insert-char ? (- fill-column (current-column)))
          (comment-box begin end 1)
          (goto-char end)
          (set-marker end nil))
      (user-error "No region selected!"))))

;;; [ banner-comment ] -- turn a comment into a banner.

(use-package banner-comment
  :ensure t
  :defer t
  :commands (banner-comment)
  :bind (:map prog-comment-prefix ("=" . banner-comment)))


;;; [ hl-todo ] -- highlight TODO and similar keywords.

(use-package hl-todo
  :ensure t
  :defer t
  :commands (hl-todo-next hl-todo-previous hl-todo-occur)
  :bind (:map prog-comment-prefix
              ("n" . hl-todo-next)
              ("p" . hl-todo-previous)
              ("o" . hl-todo-occur)
              ("t" . hl-todo-insert))
  :hook (after-init . global-hl-todo-mode)
  :config
  (add-to-list 'hl-todo-keyword-faces '("ISSUE" . "#ff8c00"))
  (add-to-list 'hl-todo-keyword-faces '("DEBUG" . "#ff8c00"))
  (add-to-list 'hl-todo-keyword-faces '("TEST" . "tomato"))
  (add-to-list 'hl-todo-keyword-faces '("PERFORMANCE" . "#5f7f5f"))
  (add-to-list 'hl-todo-keyword-faces '("PATCH" . "dodger blue"))
  (add-to-list 'hl-todo-activate-in-modes 'conf-mode 'append))

;; [ poporg ] -- Editing program comments or strings in text mode.

(use-package poporg
  :ensure t
  :defer t
  :custom-face
  (poporg-edited-face
   ((t `(:foreground "chocolate"
                     :background ,(cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light
                                     (color-darken-name (face-background 'default) 10))
                                    ('dark
                                     (color-darken-name (face-background 'default) 5)))))))
  :bind (("C-c '" . poporg-dwim)
         :map prog-comment-prefix ("'" . poporg-dwim)
         :map poporg-mode-map ([remap save-buffer] . poporg-edit-exit))
  :init
  ;; display poporg popup buffer below the selected window with 0.3 height.
  (add-to-list 'display-buffer-alist
               '("\\*poporg:\ .*?\\*" . ((display-buffer-below-selected) (window-height . 0.3)))))


(provide 'init-prog-comment)

;;; init-prog-comment.el ends here
