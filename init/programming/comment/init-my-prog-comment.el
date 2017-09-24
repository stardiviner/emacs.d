;;; init-my-prog-comment.el --- init Comment settings for Programming in Emacs

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


;;; [ boxquote ]

(use-package boxquote
  :ensure t
  :config
  ;; (setq boxquote-title-format "[ %s ]")

  ;; `message-completion-function' (like capf)
  ;; (setq message-expand-name-databases '(bbdb eudb))

  (define-key narrow-map (kbd "q") 'boxquote-narrow-to-boxquote-content)

  (unless (boundp 'my-boxquote-map)
    (define-prefix-command 'my-boxquote-map))
  (define-key prog-comment-prefix (kbd "q") 'my-boxquote-map)

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
  )

;;; --------------------------------------------------------------------

(unless (boundp 'comment-tag-prefix)
  (define-prefix-command 'comment-tag-prefix))
(global-set-key (kbd "M-g c") 'comment-tag-prefix)

;;; [ hl-todo ]-- highlight todo, fixme and similar keywords.

;; (use-package hl-todo
;;   :ensure t
;;   :config
;;   (set-face-attribute 'hl-todo nil
;;                       :inherit nil
;;                       :foreground "orange red"
;;                       :box '(:line-width 1 :color "gray")
;;                       )
;;
;;   (define-key comment-tag-prefix (kbd "p") 'hl-todo-previous)
;;   (define-key comment-tag-prefix (kbd "n") 'hl-todo-next)
;;   (define-key comment-tag-prefix (kbd "l") 'hl-todo-occur)
;;
;;   ;; (global-hl-todo-mode 1)
;;   (add-hook 'prog-mode-hook #'hl-todo-mode)
;;   )

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
  (add-to-list 'display-buffer-alist
               '("\\*poporg:\ .*?\\*" ; *poporg: init-my-emacs-window.el*
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height . 0.3)
                 ))
  :config
  (setq poporg-adjust-fill-column t
        poporg-delete-trailing-whitespace t)
  (set-face-attribute 'poporg-edited-face nil
                      :foreground "chocolate"
                      :background (color-darken-name (face-background 'default) 5)
                      :slant 'italic)

  (define-key poporg-mode-map [remap save-buffer] 'poporg-edit-exit)
  )


;;; [ org-commentary ] -- generate/update conventional library headers using Org-mode.

;; (use-package org-commentary
;;   :ensure t
;;   )


(provide 'init-my-prog-comment)

;;; init-my-prog-comment.el ends here
