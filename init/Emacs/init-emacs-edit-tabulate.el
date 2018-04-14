;;; init-emacs-edit-tabulate.el --- init for tabulate
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Align ]

;;; Usage:
;;
;; - commands prefix with `align-', `sort-',
;; - custom variable `align-rules-list'.
;; - `sort-fields', `sort-regexp-fields', `sort-numeric-fields', `sort-columns', `reverse-region',
;;
;; - region select text + [C-u M-x align-regexp] (could contains group in regexp pattern)

(setq align-highlight-change-face 'highlight)

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression.

For example: input regexp like [[:space:]]+ for align several space separated section/region."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 1 t)
  ;; The final `t' (aka true) is responsible for repeating the task.
  ;; Call that command with the regular expression `[[:space:]]+'
  )

(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(text-column-whitespace
                           (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                           (group   . 2)
                           (modes   . align-text-modes)
                           (repeat  . t)))))

;;; [ ialign ] -- interactive Emacs command `align-regexp'.

(use-package ialign
  :ensure t
  :commands (ialign-interactive-align))


(provide 'init-emacs-edit-tabulate)

;;; init-emacs-edit-tabulate.el ends here
