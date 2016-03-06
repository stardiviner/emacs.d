;;; init-my-prog-programming.el --- init for common Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prog-mode ]

;;; `prog-mode' is a major mode provided by Emacs. Typically, it is not used
;;; directly, instead many programming-related major modes are derived from this
;;; mode.

;; User can add things to `prog-mode-hook', which are executed for all
;; programming modes (that are derived from `prog-mode').
;; One benefit of using this mode is that global minor modes no longer have to
;; maintain a long list of suitable major modes. Instead, they can simply check
;; if a mode is derived from one of the base modes.
;; Other often used base modes include `special-mode' and `text-mode'.

;;; Usage:
;;
;; Some major programming modes is not included in `prog-mode' alist.
;; - check out what major modes which current in "prog-mode" list.
;; --> TODO:
;;
;; You can define a new major mode derived from ‘prog-mode’ using the following:
;;
;; (define-derived-mode alpha-mode prog-mode "Alpha"
;;   "Major mode for editing alpha files."
;;   ...)
;;
;; You can check if the major mode of the current buffer is derived from ‘prog-mode’ using:
;;
;; (derived-mode-p 'prog-mode)
;;
;; A global minor mode that will be enabled for all ‘prog-mode’ modes can be defined using:
;;
;; (define-global-minor-mode my-global-mode my-mode
;;   (lambda ()
;;     (when (derived-mode-p 'prog-mode)
;;       (my-mode 1))))

;;; TODO: add un-included programming modes into `prog-mode' alist variable.


;;; Turn a non prog-mode derived major-mode into a prog-mode derived major-mode.
;;;
;;; You should place this after anything else you add to erlang-mode-hook to make
;;; sure prog-mode-hook gets called before anything else. That way erlang-mode
;;; can clobber any settings in prog-mode that it doesn't like.
;;
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (unless (derived-mode-p 'prog-mode))
;;             (run-hooks 'prog-mode-hook)))

(dolist (hook '(ruby-mode-hook
                html-mode-hook
                css-mode-hook
                ))
  (add-hook hook (lambda ()
                   (unless (derived-mode-p 'prog-mode))
                   (run-hooks 'prog-mode-hook))))



(provide 'init-my-prog-programming)

;;; init-my-prog-programming.el ends here
