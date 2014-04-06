;;; init-my-emacs-frame.el --- init Emacs frame
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Adjust Frame’s Opacity

;; (defun stardiviner/adjust-opacity (frame incr)
;;   (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
;;          (newalpha (+ incr oldalpha)))
;;     (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
;;       (modify-frame-parameters frame (list (cons 'alpha newalpha))))))



(provide 'init-my-emacs-frame)

;;; init-my-emacs-frame.el ends here
