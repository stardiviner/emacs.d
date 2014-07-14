;;; init-my-autopair.el --- init autopair
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ autopair ]

(require 'autopair)

(autoload 'autopair "autopair" t)

(setq autopair-pair-criteria 'help-balance
      autopair-skip-criteria 'help-balance
      autopair-autowrap 'help-balance ; 'help-balance, t
      autopair-blink t)

(setq autopair-extra-pairs `(:everywhere (;; chinese punctuation
                                          (?‘. ?’)
                                          (?“. ?”)
                                          (?（. ?）)
                                          (?【. ?】)
                                          (?〖. ?〗)
                                          (?『. ?』)
                                          (?｛. ?｝)
                                          (?「. ?」)
                                          (?〔. ?〕)
                                          (?［. ?］)
                                          (?《. ?》)
                                          (?〈. ?〉)
                                          (?«. ?»)
                                          (?‹. ?›)
                                          )))


;; (autopair-global-mode 1)

(dolist (hook
         '(prog-mode-hook
           ;; ess-mode-hook                ; Emacs Speaks Statistics
           ))
  (add-hook hook #'(lambda ()
                     ;; FIXME test whether paredit is active
                     (unless (and (boundp 'paredit-mode) paredit-mode)
                       (autopair-mode))
                     )))


;;; More tricks
;;; prevent the { (opening brace) character from being autopaired in C++ comments.
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push ?{
;;                     (getf autopair-dont-pair :comment))))
;;; autopair-handle-action-fns lets you write some emacs-lisp that overrides/extends the actions taken by autopair after it decides something must be paired, skipped or deleted. To work with triple quoting in python mode, you can use this for example:
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))
;;; autopair-extra-pairs lets you define extra pairing and skipping behaviour for pairs not programmed into the syntax table. Watch out, this is work-in-progress, a little unstable and does not help balancing at all. To have < and > pair in c++-mode buffers, but only in code, use:
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (getf autopair-extra-pairs :code))))
;;; if you program in emacs-lisp you might also like the following to pair backtick (`) and quote (’).
;;; for quote Emacs Lisp code. e.g. `org-mode'
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq autopair-extra-pairs `(:comment ((?`. ?'))))
              ;; (push '(?` . ?')
              ;;       (getf autopair-extra-pairs :comment))
              ;; (push '(?` . ?')
              ;;       (getf autopair-extra-pairs :string))
              )
          )



(provide 'init-my-autopair)

;;; init-my-autopair.el ends here
