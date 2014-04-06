;;; init-my-prog-comment.el --- init Comment settings for Programming in Emacs

;;; Commentary:


;;; Code:


;;; Emacs default comment [C-;]
;; Usage:
;; - [M-;] -- indent-for-comment
;;   - for new line
;;   - for region
;; - comment-region
;;   - [C-u N M-x comment-region] -- where `N` is an integer, adds or removes N `;`.
;;   - [C-u M-x [un]comment-region] -- uncomments the region, no matter how many `;` have.
;; - bind [C-x C-;] to `comment-region` is very handy.

;;; comment annotations function
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|ISSUE\\|BUG\\|NOTE\\|NOTICE\\):" 1
                             '(:foreground "dark red" :background "black"
                                           :box '(:color "#222222" :line-width 1 :style nil)
                                           :weight 'bold) t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;;; [ fixme-mode ] -- highlight fixme, todo, bug and other warning comments, and jumping to.

;; (require 'fixme-mode)


;;; [ fic-mode ] --- "fixme in comments (and strings)"
(require 'fic-mode)

(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "ISSUE" "ERROR" "NOTE" "NOTICE"))

;; (setq fic-foreground-color "red")
;; (setq fic-background-color "yellow")

;; (set-face-attribute 'fic-face nil
;;                     :foreground "red")
;; (set-face-attribute 'font-lock-fic-face nil ; 'fic-face, 'font-lock-fic-face.
;;                     :foreground "red" :background "black"
;;                     :weight 'bold
;;                     :box '(:color "#222222" :line-width 1 :style nil)
;;                     )

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
