;;; init-my-emacs-mouse.el --- init for Mouse.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Mouse ]

;;; disable all mouse click events
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

(mouse-avoidance-mode 'animation)

;; make pointer invisible while typing.
;; The pointer becomes visible again when the mouse is moved.
(setq make-pointer-invisible t)


;;; disable mouse minor mode

(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " 🐭"
  :keymap (make-sparse-keymap))

(dolist (type '(mouse down-mouse drag-mouse
                      double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
    ;; Yes, I actually HAD to go up to 7 here.
    (dotimes (n 7)
      (let ((k (format "%s%s-%s" prefix type n)))
        (define-key disable-mouse-mode-map
          (vector (intern k)) #'ignore)))))

(disable-mouse-mode 1)


;;; [ Drag & Drop (dnd protocol) ]

;; (setq dnd-open-file-other-window t)

;; (add-to-list 'dnd-protocol-alist '())


(provide 'init-my-emacs-mouse)

;;; init-my-emacs-mouse.el ends here
