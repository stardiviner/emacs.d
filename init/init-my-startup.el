;;; init-my-startup.el --- init startup things

;;; Commentary:

;;; Code:


;;; initial Emacs frame size

;; 1:
;; (add-hook 'before-make-frame-hook
;;           #'(lambda ()
;;               (add-to-list 'default-frame-alist '(left . 0))
;;               (add-to-list 'default-frame-alist '(top . 0))
;;               (add-to-list 'default-frame-alist '(height . 150))
;;               (add-to-list 'default-frame-alist '(width . 100))))

;; 2:
;; (if window-system
;;    (set-frame-size (selected-frame) 120 45))

;; 3:
;; ~/.Xresources (or .Xdefaults):
;; Emacs*geometry:  80x24

;; 4:
;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if (display-graphic-p)
;;       (progn
;;         ;; use 120 char wide window for largeish displays
;;         ;; and smaller 80 columns windows for smaller displays
;;         ;; pick whatever number make sense for you
;;         (if (> (x-display-pixel-width) 1080)
;;             (add-to-list 'default-frame-alist (cons 'width 250))
;;           (add-to-list 'default-frame-alist (cons 'width 80)))
;;         ;; or:
;;         ;; (add-to-list 'default-frame-alist (cons 'width 120))
;;         ;; for the height, subtract a couple hundred pixels from the screen
;;         ;; height (for panels, menubars and whatnot), then divide by the height
;;         ;; of a char to get the height we want.
;;         (add-to-list 'default-frame-alist
;;                      (cons 'height (/ (- (x-display-pixel-height) 150)
;;                                       (frame-char-height))))
;;         ;; or:
;;         ;; (add-to-list 'default-frame-alist (cons 'height 600))
;;         ;; FIXME add parameters for this function. only show one window when
;;         ;; startup default frame.
;;         ;; (delete-other-windows)
;;         )))
;;
;; (set-frame-size-according-to-resolution)

;; 5.
;; (setq initial-frame-alist
;;       '((top . 1) (left . -220)
;;         (width . 120)
;;         (height . 55)
;;         ))



(find-file "~/Code/learning/Ruby/test.rb")



;; replace initial/scratch buffer with our primary `.org' file
(setq my-org-special-directory (expand-file-name "~/Org"))
;; (setq initial-buffer-choice (concat my-org-special-directory "/Tasks.org"))


;;; start Sauron at Emacs startup
;; (sauron-start)


;;; open Org-Agenda at startup.

;; (org-agenda-list)
;; or
;; (org-todo-list t)

;; FIXME: (org-agenda-redo 5) ; Urgent tasks

;; (setq initial-buffer-choice "*Org Agenda*")


;;; Temporary remove semantic in `python-mode-hook'

;;; FIXME: temporary solve python+semantic stack issue.
;; TODO: search how to solve this issue?
;; `semantic-python-get-system-include-path'
(defun wisent-python-default-setup ()
  )
(add-hook 'after-init-hook
          '(lambda ()
             (remove-hook 'python-mode-hook 'wisent-python-default-setup)
             ))



(provide 'init-my-startup)

;;; init-my-startup.el ends here
