;;; init-my-keybindings.el --- init keybindings
;;
;;; Commentary:

;;; Code:


;;; Emacs

;;; edit
(unless (boundp 'my-edit-prefix-map)
  (define-prefix-command 'my-edit-prefix-map))
(global-set-key (kbd "C-c e") 'my-edit-prefix-map)

;;; search
(unless (boundp 'my-search-prefix-map)
  (define-prefix-command 'my-search-prefix-map))
(global-set-key (kbd "C-c s") 'my-search-prefix-map)

;;; regexp
(unless (boundp 'my-regexp-prefix-map)
  (define-prefix-command 'my-regexp-prefix-map))
(global-set-key (kbd "C-c r") 'my-regexp-prefix-map)


;;; Programming

(unless (boundp 'my-programming-prefix-map)
  (define-prefix-command 'my-programming-prefix-map))
(global-set-key (kbd "C-c p") 'my-programming-prefix-map)


(unless (boundp 'code-map)
  (define-prefix-command 'code-map))
;; (define-key my-programming-prefix-map (kbd "c") 'code-map)
(global-set-key (kbd "C-c c") 'code-map)

(unless (boundp 'help-document-map)
  (define-prefix-command 'help-document-map))
;; (define-key my-programming-prefix-map (kbd "h") 'help-document-map)
(global-set-key (kbd "C-h d") 'help-document-map)

(unless (boundp 'inferior-map)
    (define-prefix-command 'inferior-map))
;; (define-key my-programming-prefix-map (kbd "i") 'inferior-map)
(global-set-key (kbd "C-c i") 'inferior-map)

(unless (boundp 'vcs-map)
  (define-prefix-command 'vcs-map))
;; (define-key my-programming-prefix-map (kbd "v") 'vcs-map)
(global-set-key (kbd "C-c v") 'vcs-map)

(unless (boundp 'vcs-git-map)
  (define-prefix-command 'vcs-git-map))
(define-key 'vcs-map (kbd "g") 'vcs-git-map)

(unless (boundp 'project-map)
  (define-prefix-command 'project-map))
;; (define-key my-programming-prefix-map (kbd "p") 'project-map)
(global-set-key (kbd "C-c p") 'project-map)


;;; Programming Languages


;;; Tools

(unless (boundp 'my-tools-prefix-map)
  (define-prefix-command 'my-tools-prefix-map))
(global-set-key (kbd "C-c t") 'my-tools-prefix-map)

(unless (boundp 'my-org-prefix-map)
  (define-prefix-command 'my-org-prefix-map))
(global-set-key (kbd "C-c o") 'my-org-prefix-map)


;;; Others




(provide 'init-my-keybindings)

;;; init-my-keybindings.el ends here
