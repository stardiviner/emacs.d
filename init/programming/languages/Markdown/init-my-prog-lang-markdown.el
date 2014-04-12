;;; init-my-prog-lang-markdown.el --- init Emacs for Markdown
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ markdown-mode ]

(require 'markdown-mode)



;;; for Stack Overflow {It's All Text} (Firefox Addon)
;; Integrate Emacs with Stack Exchange http://stackoverflow.com/a/10386560/789593
(add-to-list 'auto-mode-alist '("stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt" . markdown-mode))
;; Alternatively, if `as-external-alist' is defined—if M-x describe-variable RET
;; as-external-alist doesn't fail—it will probably override your
;; auto-mode-alist. It has a slightly different format (it's a list of pairs
;; instead of a list of cons cells) so this will work:
;; (add-to-list 'as-external-alist '("stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt" markdown-mode))



(provide 'init-my-prog-lang-markdown)

;;; init-my-prog-lang-markdown.el ends here
