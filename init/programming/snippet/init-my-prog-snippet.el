;;; init-my-prog-snippet.el --- init Programming snippet engines

;;; Commentary:


;;; Code:

;;; [ YASnippet ] --- (template/snippet engine)

(use-package yasnippet
  :ensure t
  :defer t
  ;; auto set major mode: snippet-mode.
  :mode (("\\.snippet$" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ; personal snippets directory
          ))

  ;; indent
  (setq yas-indent-line 'auto) ; 'auto, 'fixed
  (setq yas-also-auto-indent-first-line nil)
  ;; Python indent issue
  (add-hook 'python-mode-hook
            (lambda ()
              (make-local-variable 'yas-indent-line)
              (setq yas-indent-line 'fixed)))

  ;; wrap around region
  (setq yas-wrap-around-region t) ; snippet expansion wraps around selected region.

  ;; stacked expansion
  (setq yas-triggers-in-field t) ; allow stacked expansions (snippets inside field).

  (setq yas-snippet-revival t) ; re-activate snippet field after undo/redo.

  ;; (setq yas-key-syntaxes '("w" "w_" "w_." "w_.()" yas-try-key-from-whitespace))

  ;; for `yas-choose-value'.
  ;; (setq yas-prompt-functions )

  (setq yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# group: ${3:group.subgroup}${4:
# expand-env: ((${5:VAR} ${6:VALUE}))}${7:
# type: snippet/command}
# --
$0`(yas-escape-text yas-selected-text)`"
        )

  ;; turn of auto-fill for long length code
  (add-hook 'snippet-mode #'turn-off-auto-fill)

  ;; Faces
  (set-face-attribute 'yas-field-highlight-face nil
		                  :inherit 'highlight
                      :box '(:color "dim gray" :line-width 1)
                      )

  
  ;; (define-key yas-minor-mode-map [tab] 'yas-expand)
  ;; (define-key yas-minor-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key yas-minor-mode-map (kbd "C-c \\") 'yas-insert-snippet)
  
  ;; project local snippets
  (defun yasnippet-project-local ()
    (interactive)
    (make-local-variable 'yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs
                 (concat (projectile-project-root) ".snippets"))
    )
  (add-hook 'projectile-find-file-hook #'yasnippet-project-local)
  
  (defun my-yas-exit-animation ()
    ;; (popup-tip "snippet exited")
    ;; (message "snippet exited")
    (let ((beacon-size 20)
          (beacon-color "deep pink"))
      (beacon-blink))
    )
  (add-hook 'yas-after-exit-snippet-hook #'my-yas-exit-animation)
  
  ;; enable global yasnippet-mode
  (yas-global-mode 1)
  )


;;; [ auto-yasnippet ] -- quickly create disposable yasnippets.

;; (use-package auto-yasnippet
;;   :ensure t
;;   :config
;;   (setq aya-persist-snippets-dir "~/.emacs.d/snippets"
;;         aya-create-with-newline t)
;;   )

;;; [ org-sync-snippets ] -- simple extension to export snippets to org-mode and vice versa.

(use-package org-sync-snippets
  :ensure t
  :after org ; to fix variable `org-directory' is not customized to "~/Org" issue.
  :config
  (setq org-sync-snippets-snippets-dir (concat user-emacs-directory "snippets/"))
  (setq org-sync-snippets-org-snippets-file (concat (file-name-as-directory org-directory) "Programming/Code Snippets/yasnippets.org"))
  (add-hook 'yas-after-reload-hook 'org-sync-snippets-snippets-to-org)
  )


(provide 'init-my-prog-snippet)

;;; init-my-prog-snippet.el ends here
