;;; init-my-prog-document-assistant.el --- code assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ howdoi ] -- instant coding answers via Emacs.

;;; $ pip install howdoi

;;; Do you find yourself constantly Googling for how to do basic programing
;;; tasks? Suppose you want to know how to format a date in bash. Why open your
;;; browser and read through blogs when you can just M-x howdoi-query RET format
;;; date bash RET.

;;; Howdoi.el is a way to query Stack Overflow directly from the Emacs and get
;;; back the most upvoted answer to the first question that comes up for that
;;; query.

;;; Usage:
;;
;; - [M-x howdoi-query RET <your-query> RET] ;; e.g.: M-x howdoi-query RET format date bash RET
;; - [M-x howdoi-query-line-at-point] ;; takes a query from a line at point and shows an answer in a pop up buffer.

;; By default pop up buffer displays only answers. You could change
;; howdoi-display-question custom variable to show also a question.
;;
;; In the mentioned pop up buffer enables HowDoI major-mode. There are such key
;; bindings are available:
;;
;;     n - howdoi-show-next-question
;;     p - howdoi-show-previous-question
;;     b - howdoi-browse-current-question
;;     u - howdoi-query
;;     < - beginning-of-buffer
;;     > - end-of-buffer
;;     q - quit window
;;
;; There is also howdoi-minor-mode available with a list of key bindings:
;;
;;     C-c C-o n - howdoi-show-next-question
;;     C-c C-o p - howdoi-show-previous-question
;;     C-c C-o c - howdoi-show-current-question
;;     C-c C-o b - howdoi-browse-current-question
;;     C-c C-o u - howdoi-query
;;     C-c C-o l - howdoi-query-line-at-point
;;     C-c C-o r - howdoi-query-line-at-point-replace-by-code-snippet

(use-package howdoi
  ;; :ensure t
  :config
  (define-key my-prog-help-document-map (kbd "h") 'howdoi-query)
  )


(provide 'init-my-prog-document-assistant)

;;; init-my-prog-document-assistant.el ends here
