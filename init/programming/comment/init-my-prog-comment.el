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
  :config
  ;; (setq boxquote-title-format "[ %s ]")

  ;; `message-completion-function' (like capf)
  ;; (setq message-expand-name-databases '(bbdb eudb))

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
  )

;;; [ fixmee ] -- Quickly navigate to FIXME notices in Emacs.

(unless (boundp 'my-prog-comment-fixme-map)
  (define-prefix-command 'my-prog-comment-fixme-map))
(global-set-key (kbd "M-g f") 'my-prog-comment-fixme-map)


(use-package fixmee
  :ensure t
  :init
  ;; disable fixmee default global keybindings.
  (setq fixmee-smartrep-prefix nil
        fixmee-view-listing-keystrokes nil
        fixmee-goto-nextmost-urgent-keystrokes nil
        fixmee-goto-next-by-position-keystrokes nil
        fixmee-goto-prevmost-urgent-keystrokes nil
        fixmee-goto-previous-by-position-keystrokes nil)

  :config
  (setq fixmee-cache-refresh-interval 30)

  (set-face-attribute 'fixmee-notice-face nil
                      :background "dark orange" :foreground "#222222"
                      :weight 'bold
                      )
  
  ;; (add-to-list 'fixmee-exclude-modes 'xxx-mode)
  ;; (global-fixmee-mode -1)

  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook 'fixmee-mode))

  (define-key my-prog-comment-fixme-map (kbd "l") 'fixmee-view-listing)
  (define-key my-prog-comment-fixme-map (kbd "n") 'fixmee-goto-next-by-position)
  (define-key my-prog-comment-fixme-map (kbd "p") 'fixmee-goto-previous-by-position)
  (define-key my-prog-comment-fixme-map (kbd "N") 'fixmee-goto-nextmost-urgent)
  (define-key my-prog-comment-fixme-map (kbd "P") 'fixmee-goto-prevmost-urgent)

  (defun fixmee-insert-keywords (prefix-arg)
    "Insert fixmee patterns: @@@, XXX, todo, fixme.
And specify urgent with PREFIX-ARG."
    (interactive "P")

    (let* ((fixmee-patterns '("@@@" "XXX" "todo" "fixme"))
           (fixmee-keyword (completing-read "fixmee patterns: " fixmee-patterns))
           (urgent-number (if prefix-arg
                              prefix-arg
                            (string-to-number (read-string "urgent number: "))
                            ))
           (last-char-of-fixmee-keyword
            (substring-no-properties fixmee-keyword (1- (length fixmee-keyword))))
           (fixmee-keyword-string
            (concat fixmee-keyword
                    (make-string
                     (1- urgent-number)
                     (string-to-char last-char-of-fixmee-keyword))
                    ": ")))
      (insert fixmee-keyword-string))
    )
  
  (define-key my-prog-comment-fixme-map (kbd "i") 'fixmee-insert-keywords)
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
  :config
  (global-set-key (kbd "C-c '") 'poporg-dwim)
  ;; (define-key my-prog-comment-map (kbd "'") 'poporg-dwim)
  ;; (define-key poporg-mode-map [remap save-buffer] 'poporg-edit-exit)
  
  (setq poporg-adjust-fill-column t
        poporg-delete-trailing-whitespace t)  
  (set-face-attribute 'poporg-edited-face nil
                      :foreground "green yellow"
                      :background (color-darken-name (face-background 'default) 5)
                      :slant 'italic)
  )


(provide 'init-my-prog-comment)

;;; init-my-prog-comment.el ends here
