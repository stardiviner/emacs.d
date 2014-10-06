;;; init-my-keybindings.el --- init keybindings
;;
;;; Commentary:

;;; Code:


;;; Emacs

(global-set-key [remap toggle-frame-maximized] 'toggle-frame-fullscreen)

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


(unless (boundp 'my-prog-code-map)
  (define-prefix-command 'my-prog-code-map))
;; (define-key my-programming-prefix-map (kbd "c") 'my-prog-code-map)
(global-set-key (kbd "C-c c") 'my-prog-code-map)

(unless (boundp 'my-prog-help-document-map)
  (define-prefix-command 'my-prog-help-document-map))
;; (define-key my-programming-prefix-map (kbd "h") 'my-prog-help-document-map)
(global-set-key (kbd "C-h d") 'my-prog-help-document-map)

;; like function, variable, class, scope etc lookup.
(unless (boundp 'my-prog-lookup-map)
  (define-prefix-command 'my-prog-lookup-map))
(global-set-key (kbd "C-c l") 'my-prog-lookup-map)

;; (define-key my-prog-lookup-map (kbd "l") 'helm-semantic-or-imenu)

(unless (boundp 'my-prog-inferior-map)
    (define-prefix-command 'my-prog-inferior-map))
;; (define-key my-programming-prefix-map (kbd "i") 'my-prog-inferior-map)
(global-set-key (kbd "C-c i") 'my-prog-inferior-map)

(unless (boundp 'my-prog-debug-prefix)
  (define-prefix-command 'my-prog-debug-prefix))
(global-set-key (kbd "C-c d") 'my-prog-debug-prefix)

(unless (boundp 'my-prog-vcs-map)
  (define-prefix-command 'my-prog-vcs-map))
;; (define-key my-programming-prefix-map (kbd "v") 'my-prog-vcs-map)
(global-set-key (kbd "C-c v") 'my-prog-vcs-map)

(unless (boundp 'my-prog-vcs-git-map)
  (define-prefix-command 'my-prog-vcs-git-map))
(define-key 'my-prog-vcs-map (kbd "g") 'my-prog-vcs-git-map)

(unless (boundp 'my-prog-project-map)
  (define-prefix-command 'my-prog-project-map))
;; (define-key my-programming-prefix-map (kbd "p") 'my-prog-project-map)
(global-set-key (kbd "C-c p") 'my-prog-project-map)

;; TODO: check where work as my expect.
(unless (boundp 'my-prog-lint-map)
  (define-prefix-command 'my-prog-lint-map))

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c !") 'my-prog-lint-map)
            (define-key my-prog-lint-map (kbd "b") 'flycheck-buffer)
            ))


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
