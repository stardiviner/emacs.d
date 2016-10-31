;;; init-my-prog-comment.el --- init Comment settings for Programming in Emacs

;;; Commentary:


;;; Code:


(unless (boundp 'my-prog-comment-map)
  (define-prefix-command 'my-prog-comment-map))
(global-set-key (kbd "M-;") 'my-prog-comment-map)



(setq comment-auto-fill-only-comments t
      comment-multi-line t
      )


;;; prefix: [M-;], `my-prog-comment-map'

(define-key my-prog-comment-map (kbd "M-;") 'comment-dwim)
;; or with [C-u N]
(global-set-key (kbd "C-x C-;") 'comment-line)
(define-key my-prog-comment-map (kbd "l") 'comment-line)
(define-key my-prog-comment-map (kbd "b") 'comment-box)
(define-key my-prog-comment-map (kbd "B") 'comment-box-with-fill-column)

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


;;; [ boxquote ]

(use-package boxquote
  :ensure t
  :defer t
  :init
  (define-key narrow-map (kbd "q") 'boxquote-narrow-to-boxquote-content)

  (unless (boundp 'my-boxquote-map)
    (define-prefix-command 'my-boxquote-map))
  (define-key my-prog-comment-map (kbd "q") 'my-boxquote-map)

  (define-key my-boxquote-map (kbd "q") 'boxquote-boxquote)
  (define-key my-boxquote-map (kbd "u") 'boxquote-unbox)
  (define-key my-boxquote-map (kbd "t") 'boxquote-text)
  (define-key my-boxquote-map (kbd "U") 'boxquote-unbox-region)
  (define-key my-boxquote-map (kbd "r") 'boxquote-region)
  (define-key my-boxquote-map (kbd "b") 'boxquote-buffer)
  (define-key my-boxquote-map (kbd "f") 'boxquote-defun)
  (define-key my-boxquote-map (kbd "c") 'boxquote-shell-command)
  (define-key my-boxquote-map (kbd "F") 'boxquote-describe-function)
  (define-key my-boxquote-map (kbd "K") 'boxquote-describe-key)
  (define-key my-boxquote-map (kbd "V") 'boxquote-describe-variable)
  (define-key my-boxquote-map (kbd "C-w") 'boxquote-kill)
  (define-key my-boxquote-map (kbd "C-y") 'boxquote-yank)
  (define-key my-boxquote-map (kbd "p") 'boxquote-paragraph)
  
  :config
  ;; (setq boxquote-title-format "[ %s ]")

  ;; `message-completion-function' (like capf)
  ;; (setq message-expand-name-databases '(bbdb eudb))
  )

;;; --------------------------------------------------------------------

(unless (boundp 'fic-prefix)
  (define-prefix-command 'fic-prefix))
(global-set-key (kbd "M-g f") 'fic-prefix)

;;; [ hl-todo ]-- highlight todo, fixme and similar keywords.

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)

  (define-key fic-prefix (kbd "p") 'hl-todo-previous)
  (define-key fic-prefix (kbd "n") 'hl-todo-next)
  (define-key fic-prefix (kbd "o") 'hl-todo-occur)
  )

;; [ poporg ] -- Editing program comments or strings in text mode.

(use-package poporg
  :ensure t
  :init
  (add-to-list 'display-buffer-alist
               '("\\*poporg:\ .*?\\*" ; *poporg: init-my-emacs-window.el*
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height . 0.3)
                 ))
  (global-set-key (kbd "C-c '") 'poporg-dwim)
  ;; (define-key my-prog-comment-map (kbd "'") 'poporg-dwim)
  ;; (define-key poporg-mode-map [remap save-buffer] 'poporg-edit-exit)

  :config
  (setq poporg-adjust-fill-column t
        poporg-delete-trailing-whitespace t)
  (set-face-attribute 'poporg-edited-face nil
                      :foreground "green yellow"
                      :background (color-darken-name (face-background 'default) 5)
                      :slant 'italic)
  )


;;; [ org-commentary ] -- generate/update conventional library headers using Org-mode.

;; (use-package org-commentary
;;   :ensure t
;;   )


(provide 'init-my-prog-comment)

;;; init-my-prog-comment.el ends here
