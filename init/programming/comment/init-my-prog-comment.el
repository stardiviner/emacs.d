;;; init-my-prog-comment.el --- init Comment settings for Programming in Emacs

;;; Commentary:


;;; Code:


(define-key my-prog-comment-map (kbd "M-;") 'comment-dwim)


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
      '("FIXME" "TODO" "BUG" "ISSUE" "ERROR" "OPTIMIZE" "HACK" "REFACTOR" "REVIEW"
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
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|ISSUE\\|BUG\\|ERROR\\|HACK\\|OPTIMIZE\\|REFACTOR\\|REVIEW\\|NOTE\\|NOTICE\\|README\\):" 1
                             '(:foreground "dark red" :background "black"
                                           :box '(:color "red" :line-width -1 :style nil)
                                           :weight 'bold) t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;;; [ fixme-mode ] -- highlight fixme, todo, bug and other warning comments, and jumping to.

;; (require 'fixme-mode)


;;; [ fic-mode ] --- "fixme in comments (and strings)"

(require 'fic-mode)

(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "ISSUE" "ERROR" "OPTIMIZE" "HACK" "REFACTOR" "REVIEW"
                              "NOTE" "NOTICE" "README"))

;; (setq fic-foreground-color "red")
;; (setq fic-background-color "yellow")

(set-face-attribute 'fic-face nil
                    :foreground "red" :background "black"
                    :weight 'normal
                    :box '(:color "dark red" :line-width -1 :style nil))
(set-face-attribute 'fic-author-face nil
                    :foreground "yellow" :background "black"
                    :weight 'bold
                    :box '(:color "yellow" :line-width -1 :style nil))

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



(provide 'init-my-prog-comment)

;;; init-my-prog-comment.el ends here
