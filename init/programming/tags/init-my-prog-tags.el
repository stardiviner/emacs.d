;;; init-my-prog-tags.el -- init tags file config for Emacs.

;;; Commentary:

;;; Code:


;; like tags: function, variable, class, scope etc lookup.
(unless (boundp 'my-prog-lookup-map)
  (define-prefix-command 'my-prog-lookup-map))
(global-set-key (kbd "C-c l") 'my-prog-lookup-map)

(unless (boundp 'my-prog-lookup-tags-map)
  (define-prefix-command 'my-prog-lookup-tags-map))
(define-key my-prog-lookup-map (kbd "t") 'my-prog-lookup-tags-map)


;;; [ tags settings ]

(setq tags-add-tables t ; always add new tags to tables
      ;; tags-included-tables
      ;; tags-table-list (list
      ;;                  (expand-file-name "/usr/share/lib/TAGS"))
      tags-revert-without-query t
      ;; tags-completion-table
      tags-apropos-verbose t
      )


;;; Build Tags



;; NOTE: use etags & gtags, because company-mode support.
(require 'init-my-prog-tags-etags)
(require 'init-my-prog-tags-gtags)
;; (require 'init-my-prog-tags-ctags)
;; (require 'init-my-prog-tags-rtags)
;; (require 'init-my-prog-tags-cscope)


(provide 'init-my-prog-tags)

;;; init-my-prog-tags.el ends here
