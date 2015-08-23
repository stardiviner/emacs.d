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

(setq comment-annotation-keyword-list
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



;;; highlight FIXME:

;;; comment annotations function
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations like FIXME:.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|ISSUE\\|BUG\\|ERROR\\|HACK\\|OPTIMIZE\\|REFACTOR\\|REVIEW\\|TEST\\|NOTE\\|NOTICE\\|README\\):" 1
                             '(:foreground "dark red" :background "black"
                                           :box '(:color "red" :line-width -1 :style nil)
                                           :weight 'bold) t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;;; [ fixme-mode ] -- highlight fixme, todo, bug and other warning comments, and jumping to.

;; (require 'fixme-mode)


;;; [ fic-mode ] --- "fixme in comments (and strings)"

;; highlight "FIXME"

(setq fic-highlighted-words
      '("FIXME" "TODO" "BUG" "ISSUE" "ERROR"
        "OPTIMIZE" "HACK" "REFACTOR" "REVIEW" "TEST"
        "NOTE" "NOTICE" "README"))

(setq fic-foreground-color "red")
(setq fic-background-color "yellow")

(dolist (hook
         '(prog-mode-hook
           ;; org-mode-hook
           ))
  ;; (add-hook hook 'turn-on-fic-mode)
  (add-hook hook (lambda () (fic-mode t)))
  )


;;; fic-ext-mode.el
;; (require 'fic-ext-mode)

;; (defcustom fic-highlighted-words
;;   '("FIXME" "TODO" "BUG" "ISSUE" "ERROR")
;;   "words to highlight"
;;   :group 'fic-ext-mode)

;; (dolist (hook
;;       '(prog-mode-hook))
;;   (add-hook hook (lambda () (fic-ext-mode t))))

;; force re-fontification initially (if you manually turn on fic-mode)
;; (font-lock-fontify-buffer)


;;;_* outorg -- Convert source-code buffers temporarily to Org-mode for comment editing.

;;; Usage:
;;
;; - [prefix] + [C-c '] (outorg-edit-as-org) :: main command
;; - [M-# #] (or M-x outorg-edit-as-org) ::
;; - [M-#] (outorg-copy-edits-and-exit) ::
;; - [C-x C-s] (outorg-save-edits-to-tmp-file) ::

;; (require 'outorg)
;;
;; ;; Outorg (like outshine) assumes that you set `outline-minor-mode-prefix' in your init-file to 'M-#':
;; ;; NOTE: must be set before outline is loaded
;; (defvar outline-minor-mode-prefix "\M-#")
;;
;; (global-set-key (kbd "C-c '") 'outorg-edit-as-org)



;;_* [ poporg ] -- Editing program comments or strings in text mode.
;;
;;; Usage:
;;
;; - [poporg-dwim] :: [C-c ']
;; - [poporg-edit-and-exit] :: [C-c '], [C-x C-s] in opened buffer.
;; - `poporg-edit-hook'

(setq poporg-adjust-fill-column t
      poporg-delete-trailing-whitespace t)

;; Org-mode Babel like keybindings.
(if (featurep 'poporg)
    (progn
      (global-set-key (kbd "C-c '") 'poporg-dwim)
      ;; (define-key my-prog-comment-map (kbd "'") 'poporg-dwim)
      ;; (define-key poporg-mode-map [remap save-buffer] 'poporg-edit-exit)
      ))

(set-face-attribute 'poporg-edited-face nil
                    :foreground "green yellow"
                    :background (color-darken-name (face-background 'default) 5)
                    :slant 'italic)


(provide 'init-my-prog-comment)

;;; init-my-prog-comment.el ends here
