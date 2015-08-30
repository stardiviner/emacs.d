;;; init-my-prog-tags-gtags.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:
;;
;; - $ gtags
;;   go to the root of your linux, or any C project, directory and type gtags.
;;   This will create all the infrastructure that we will soon be using in emacs.


;;; Code:

;;; [ gtags ] -- (GNU Global)


;;; [ helm-gtags ]

;;; Usage:
;;
;; - `helm-gtags-dwim'
;; - `helm--gtags-update-tags' :: update gtags tags.

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      ;; helm-gtags-prefix-key "\C-c"
      ;; helm-gtags-suggested-key-mapping t
      )

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     ;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "M-s t") 'helm-gtags-select)
     
     ;; (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     ;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     ;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     ;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     ;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     
     
     (define-key my-prog-lookup-tags-map (kbd "g") 'helm-gtags-dwim)
     (define-key my-prog-lookup-tags-map (kbd "u") 'helm-gtags-update-tags)
     ))


;;; [ ggtags ] -- Emacs frontend to GNU Global source code tagging system.

;; Open any file in a project and type M-x ggtags-mode. Use
;; M-. (ggtags-find-tag-dwim) to find the tag at point. If the project has not
;; been indexed (i.e. no GTAGS file exists), ggtags will ask for the project
;; root directory and index it recursively. Alternatively one can invoke
;; ggtags-create-tags to index a directory. The mode line will display the
;; directory name next to the buffer name. If point is at a valid definition
;; tag, it is underlined.
;;
;; ggtags is similar to the standard etags package. For example these keys M-.,
;; M-,, M-* and C-M-. should work as expected in ggtags-mode.
;;
;; The following search commands are available:
;;
;; - `ggtags-find-tag-dwim'
;;    Find a tag by context.
;;
;;    If point is at a definition tag, find references, and vice versa. If point
;;    is at a line that matches `ggtags-include-pattern', find the include file
;;    instead.
;;
;;    To force finding a definition tag, call it with a `prefix' (C-u).
;;
;; - `ggtags-find-tag-mouse'
;;    Like ggtags-find-tag-dwim but suitable for binding to mouse events.
;;
;; - `ggtags-find-definition'
;;    Find definition tags. With C-u ask for the tag name with completion.
;;
;; - `ggtags-find-reference'
;;    Find reference tags. With C-u ask for the tag name with completion.
;;
;; - `ggtags-find-other-symbol'
;;    Find tags that have no definitions. With C-u ask for the tag name with completion.
;;
;; - `ggtags-find-tag-regexp'
;;    Find definition tags matching a regexp. By default it lists all matching tags in the project. With C-u restrict the lists to a directory of choice.
;;
;; - `ggtags-idutils-query'
;;    Use idutils to find matches.
;;
;; - `ggtags-grep'
;;    Grep for lines matching a regexp. This is usually the slowest.
;;
;; - `ggtags-find-file'
;;    Find a file from all the files indexed by gtags.
;;
;; - `ggtags-query-replace'
;;    Do a query replace in all files found in a search.

(hook-modes c-dialects-mode
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (and (featurep 'ggtags-mode) (ggtags-mode 1)))
  )


(provide 'init-my-prog-tags-gtags)

;;; init-my-prog-tags-gtags.el ends here
