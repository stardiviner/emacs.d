;;; init-my-prog-comment.el --- init Comment settings for Programming in Emacs

;;; Commentary:


;;; Code:

;;; prefix: [M-;], `my-prog-comment-map'

(define-key my-prog-comment-map (kbd "M-;") 'comment-dwim)

;; or with [C-u N]
(global-set-key (kbd "C-x C-;") #'comment-line)
(define-key my-prog-comment-map (kbd "l") #'comment-line)


;;; Emacs default comment [M-;]

;; Usage:
;; - [M-;] -- indent-for-comment
;;   - for new line
;;   - for region
;; - comment-region
;;   - [C-u N M-x comment-region] -- where `N` is an integer, adds or removes N `;`.
;;   - [C-u M-x [un]comment-region] -- uncomments the region, no matter how many `;` have.
;; - bind [C-x C-;] to `comment-region` is very handy.

;; TODO: http://endlessparentheses.com/new-in-emacs-25-1-comment-line.html
;; Emacs 25.1
;; (global-set-key (kbd "M-;") #'comment-line)
;; (global-set-key (kbd "C-;") #'comment-line)


;;; comment annotations keywords list.

(setq comment-fic-keywords-list
      '("FIXME" "TODO" "BUG" "ISSUE" "ERROR"
        "OPTIMIZE" "HACK" "REFACTOR" "REVIEW" "TEST"
        "NOTE" "NOTICE" "README"))


;;; [ `comment-box' ]

(define-key my-prog-comment-map (kbd "b") 'comment-box)

(define-key my-prog-comment-map (kbd "f") 'comment-box-with-fill-column)

(defun comment-box-with-fill-column (b e) ; begin, end
  "Draw a box comment around the region of B and E.

But arrange for the region to extend to at least the fill
column. Place the point after the comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ? (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))



;;; comment annotations function
;; (defun font-lock-comment-annotations ()
;;   "Highlight a bunch of well known comment annotations like FIXME:.
;;
;; This functions should be added to the hooks of major modes for programming."
;;   (font-lock-add-keywords nil
;;                           '(("\\<\\(FIXME\\|TODO\\|ISSUE\\|BUG\\|ERROR\\|HACK\\|OPTIMIZE\\|REFACTOR\\|REVIEW\\|TEST\\|NOTE\\|NOTICE\\|README\\):" 1
;;                              '(:foreground "dark red" :background "black"
;;                                            :box '(:color "red" :line-width -1 :style nil)
;;                                            :weight 'bold) t))))

;; (add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;;; [ fixmee ] -- Quickly navigate to FIXME notices in Emacs

;;; Usage:
;;
;; - patterns:
;;   - @@@
;;   - XXX         ; only this one is case-sensitive
;;   - todo
;;   - fixme
;;
;; - urgency :: e.g. FIXMEEEEEE, use command: `fixmee-goto-nextmost-urgent'.
;;
;; - C-c f 	 :: fixmee-goto-nextmost-urgent
;; - C-c F 	 :: fixmee-goto-prevmost-urgent
;; - C-c v 	 :: fixmee-view-listing
;; - M-n 	 :: fixmee-goto-next-by-position ; only when the point is
;; - M-p 	 :: fixmee-goto-previous-by-position ; inside a fixme notice



;;; [ fixme-mode ] -- highlight fixme, todo, bug and other warning comments, and jumping to.

;; (require 'fixme-mode)


;;; [ fic-mode ] --- "fixme in comments (and strings)"

(use-package fic-mode
  :ensure t
  :config
  (setq fic-highlighted-words comment-fic-keywords-list)

  (dolist (hook
           '(prog-mode-hook
             ))
    (add-hook hook #'turn-on-fic-mode)
    )
  
  (set-face-attribute 'font-lock-fic-face nil
                      :foreground "dark orange"
                      :background "#444444")
  )


;;;_* outorg -- Convert source-code buffers temporarily to Org-mode for comment editing.

;;; Usage:
;;
;; - [prefix] + [C-c '] (outorg-edit-as-org) :: main command
;; - [M-# #] (or M-x outorg-edit-as-org) ::
;; - [M-#] (outorg-copy-edits-and-exit) ::
;; - [C-x C-s] (outorg-save-edits-to-tmp-file) ::

(use-package outorg
  ;; :ensure t
  :config
  ;; Outorg (like outshine) assumes that you set `outline-minor-mode-prefix' in your init-file to 'M-#':
  ;; NOTE: must be set before outline is loaded
  (defvar outline-minor-mode-prefix "\M-#")

  (global-set-key (kbd "C-c '") 'outorg-edit-as-org)
  )


;;_* [ poporg ] -- Editing program comments or strings in text mode.
;;
;;; Usage:
;;
;; - [poporg-dwim] :: [C-c ']
;; - [poporg-edit-and-exit] :: [C-c '], [C-x C-s] in opened buffer.
;; - `poporg-edit-hook'

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
