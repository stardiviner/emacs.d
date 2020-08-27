;;; init-prog-tags.el -- init tags file config for Emacs.

;;; Commentary:

;; like tags: function, variable, class, scope etc lookup.

;;; Code:


(unless (boundp 'tags-prefix)
  (define-prefix-command 'tags-prefix))
(global-set-key (kbd "M-g t") 'tags-prefix)

;;; [ tags settings ]

(setq tags-add-tables t ; always add new tags to tables
      tags-revert-without-query t
      tags-apropos-verbose t)

;;; [ xref ]

(use-package xref
  ;; disable the following elements to avoid jump to other window when xref.
  :custom (xref-prompt-for-identifier '(not xref-find-definitions
                                            ;; xref-find-definitions-other-window
                                            ;; xref-find-definitions-other-frame
                                            ))
  :init (add-to-list 'display-buffer-alist '("^\\*xref\\*$" . (display-buffer-below-selected))))

;;; [ helm-xref ] -- Helm interface for xref results.

;; (use-package helm-xref
;;   :ensure t
;;   :custom (xref-show-xrefs-function 'helm-xref-show-xrefs))

;;; [ Imenu ] -- [M-x imenu]

(use-package imenu
  :ensure t
  :bind (([remap imenu] . counsel-imenu) ; [C-x j]: `helm-imenu', `counsel-imenu'.
         ("C-c ." . helm-imenu)))

;; NOTE: use etags & gtags, because company-mode support.
;; (require 'init-etags)
;; (require 'init-gtags)
;; (require 'init-ctags)
;; (require 'init-cscope) ; <----- prefer "cscope"
;; (require 'init-rtags)


(provide 'init-prog-tags)

;;; init-prog-tags.el ends here
