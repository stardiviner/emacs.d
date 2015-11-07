;;; init-my-emacs-edit-tabulate.el --- init for tabulate
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

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(text-column-whitespace
                           (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                           (group   . 2)
                           (modes   . align-text-modes)
                           (repeat  . t)))))


;;; [ Table Editing ]

;;; http://ergoemacs.org/emacs/emacs_table.html

;;; shows you how to use emacs's “table” feature. This feature will let you
;;; format tabular data by ASCII drawing. Then you can interactively create and
;;; edit tables with emacs commands to insert/delete column/row. You can also
;;; convert it to HTML or LaTeX formats.

;;;_* Usage:
;;
;; - (info "(emacs) Text Based Tables")
;; - [M-x table-] :: commands prefix with `table-'.



(provide 'init-my-emacs-edit-tabulate)

;;; init-my-emacs-edit-tabulate.el ends here
