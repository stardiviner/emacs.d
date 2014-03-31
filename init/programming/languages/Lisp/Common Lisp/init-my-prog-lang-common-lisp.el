(add-to-list 'auto-mode-alist '("\\.lisp\\'" . common-lisp-mode))


;;; re-define upstream default function 'lispdoc key binding.
(define-key lisp-mode-map (kbd "C-h d") 'lispdoc)


(provide 'init-my-prog-lang-common-lisp)
