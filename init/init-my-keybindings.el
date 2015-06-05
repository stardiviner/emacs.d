;;; init-my-keybindings.el --- init keybindings
;;
;;; Commentary:

;; A key binding is a mapping (relation) between an Emacs command and a key
;; sequence. The same command can be bound to more than one key sequence. A
;; given key sequence is the binding of at most one command in any given context
;; (e.g. any given buffer). The same key sequence can be bound to different
;; commands in different contexts and different keymaps.

;; A keymap is a collection of key bindings, so it is a mapping (relation)
;; between Emacs commands and key sequences. A keymap can be global, local, or
;; applicable only to a minor mode.

;;; Code:

;;; Fn[1~12]

;; "<f1>" "<f2>" "<f3>" "<f12>"

;; (-each '()
;;   (lambda (fn-key)
;;     ()))


;;; C-c [a-z]

;; TODO: improve it.
;; (-each '("a" "b" "c" "d" "z")
;;   (lambda (ctrl-c-command-key)
;;     (let ((ctrl-c-key (concat "C-c " ctrl-c-command-key)))
;;       (global-unset-key (read-kbd-macro ctrl-c-key))
;;       (define-prefix-command (intern (concat ctrl-c-key "-map")))
;;       (global-set-key (read-kbd-macro ctrl-c-key) (intern (concat ctrl-c-key "-map"))))))



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

(unless (boundp 'my-prog-comment-map)
  (define-prefix-command 'my-prog-comment-map))
(global-set-key (kbd "M-;") 'my-prog-comment-map)

(unless (boundp 'my-prog-help-document-map)
  (define-prefix-command 'my-prog-help-document-map))
;; (define-key my-programming-prefix-map (kbd "h") 'my-prog-help-document-map)
(global-set-key (kbd "C-h d") 'my-prog-help-document-map)

;; like tags: function, variable, class, scope etc lookup.
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

(unless (boundp 'my-prog-bug-bts-map)
  (define-prefix-command 'my-prog-bug-bts-map))
(global-set-key (kbd "C-c b") 'my-prog-bug-bts-map)

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
            (define-key my-prog-lint-map (kbd "!") 'flycheck-buffer)
            (define-key my-prog-lint-map (kbd "b") 'flycheck-buffer)
            ))

(unless (boundp 'my-prog-refactor-map)
  (define-prefix-command 'my-prog-refactor-map))
(global-set-key (kbd "C-c RET") 'my-prog-refactor-map)

(unless (boundp 'my-prog-test-map)
  (define-prefix-command 'my-prog-test-map))
(global-set-key (kbd "C-c t") 'my-prog-test-map)

(unless (boundp 'my-prog-database-map)
  (define-prefix-command 'my-prog-database-map))
(global-set-key (kbd "C-c D") 'my-prog-database-map)

(unless (boundp 'my-prog-tools-map)
  (define-prefix-command 'my-prog-tools-map))
(global-set-key (kbd "C-c T") 'my-prog-tools-map)



;;; Programming Languages


;;; Tools

(unless (boundp 'my-tools-prefix-map)
  (define-prefix-command 'my-tools-prefix-map))
(global-set-key (kbd "C-x t") 'my-tools-prefix-map)

(unless (boundp 'my-org-prefix-map)
  (define-prefix-command 'my-org-prefix-map))
(global-set-key (kbd "C-c o") 'my-org-prefix-map)

;;; Launcher
(define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
;; (define-key ctl-x-map "l" 'launcher-map)
;; (global-set-key (kbd "s-l") 'launcher-map)
;; (define-key launcher-map "c" #'calc)


;;; Others




(provide 'init-my-keybindings)

;;; init-my-keybindings.el ends here
