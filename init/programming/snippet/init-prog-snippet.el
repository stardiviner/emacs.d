;;; init-prog-snippet.el --- init Programming snippet engines

;;; Commentary:


;;; Code:

;;; [ YASnippet ] --- (template/snippet engine)

(use-package yasnippet
  :ensure t
  :defer t
  :delight yas-minor-mode
  ;; auto set major mode: snippet-mode.
  :mode (("\\.snippet$" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :init (yas-global-mode 1)
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
  (setq yas-wrap-around-region t) ; snippet $0 field expansion wraps around selected region.
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
                      :foreground nil :background nil
                      :box '(:color "dim gray" :line-width 1))
  
  ;; (define-key yas-minor-mode-map [tab] 'yas-expand)
  ;; (define-key yas-minor-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key yas-minor-mode-map (kbd "C-c \\") 'yas-insert-snippet)
  
  ;; project local snippets
  (defun yasnippet-project-local ()
    (interactive)
    (make-local-variable 'yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs
                 (concat (projectile-project-root) ".snippets")))
  (add-hook 'projectile-find-file-hook #'yasnippet-project-local))

;;; [ ivy-yasnippet ] -- preview yasnippet snippets with Ivy.

(use-package ivy-yasnippet
  :ensure t
  :defer t
  :bind (:map yas-minor-mode-map ([remap yas-insert-snippet] . ivy-yasnippet)))

;;; [ auto-yasnippet ] -- quickly create disposable yasnippets.

(use-package auto-yasnippet
  :ensure t
  :defer t
  :bind (:map yas-minor-mode-map
              ("C-c & a" . aya-create)
              ("C-c & e" . aya-expand)
              ("C-c & o" . aya-open-line)
              ("C-c & s" . aya-persist-snippet))
  :init (setq aya-persist-snippets-dir "~/.emacs.d/snippets"))

;;; [ org-sync-snippets ] -- simple extension to export snippets to org-mode and vice versa.

(use-package org-sync-snippets
  :ensure t
  :defer t
  :after org ; to fix variable `org-directory' is not customized to "~/Org" issue.
  :commands (org-sync-snippets-snippets-to-org org-sync-snippets-org-to-snippets)
  :init (setq org-sync-snippets-snippets-dir (concat user-emacs-directory "snippets/")
              org-sync-snippets-org-snippets-file
              (concat (file-name-as-directory org-directory)
                      "Programming Code/Code Snippets/yasnippets.org"))
  (add-hook 'yas-after-reload-hook 'org-sync-snippets-snippets-to-org))

;;; [ code-archive ] -- saving selecting code regions and inserting them as org-mode styled code blocks.

(use-package code-archive
  :ensure t
  :defer t
  :commands (code-archive-save-code code-archive-insert-org-block code-archive-goto-src)
  :init (setq code-archive-dir (concat org-directory "/Programming Code/Code Snippets/"))
  :config
  (add-to-list 'org-capture-templates
               '("s" "code [s]nippet archive" entry
                 (file (lambda () (concat code-archive-dir "snippets.org")))
                 "* %? %(code-archive-org-src-tag \"%F\")
:PROPERTIES:
:FILE:  %F
:END:
%(code-archive-do-org-capture \"%F\")")))


(provide 'init-prog-snippet)

;;; init-prog-snippet.el ends here
