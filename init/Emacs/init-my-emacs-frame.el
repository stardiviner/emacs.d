;;; init-my-emacs-frame.el --- init Emacs frame
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; default frame size

(setq default-frame-alist
      '((height . 120)
        (width . 90)
        (left-fringe) (right-fringe)
        ))

(setq initial-frame-alist
      '((height . 120)
        (width . 90)
        (left-fringe) (right-fringe)))

(setq window-system-default-frame-alist
      '((height . 120)
        (width . 90)
        (left-fringe) (right-fringe)))

(setq minibuffer-frame-alist
      '((height . 2)
        (width . 80)))


;;; Adjust Frame’s Opacity

;; (defun stardiviner/adjust-opacity (frame incr)
;;   (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
;;          (newalpha (+ incr oldalpha)))
;;     (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
;;       (modify-frame-parameters frame (list (cons 'alpha newalpha))))))


;;; [ zoom-frm ]



(provide 'init-my-emacs-frame)

;;; init-my-emacs-frame.el ends here
