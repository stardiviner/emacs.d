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
  :custom-face
  (yas-field-highlight-face ((t (:inherit 'highlight
                                          :foreground nil :background nil
                                          :box '(:color "dim gray" :line-width 1)))))
  :config
  ;; add personal snippets directory to snippets collection list.
  (add-to-list 'yas-snippet-dirs `,(expand-file-name "snippets" user-emacs-directory))

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
  
  ;; (define-key yas-minor-mode-map [tab] 'yas-expand)
  ;; (define-key yas-minor-mode-map (kbd "TAB") 'indent-for-tab-command)

  (define-key yas-minor-mode-map (kbd "C-c \\") 'yas-insert-snippet)
  
  ;; project local snippets
  (defun yasnippet-project-local ()
    (interactive)
    (make-local-variable 'yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs
                 (concat (projectile-project-root) ".snippets")))
  (add-hook 'projectile-find-file-hook #'yasnippet-project-local)

  ;; beacon animation when snippet exit hook
  (when (featurep 'beacon)
    (defun my/yas-exit-animation ()
      (let ((beacon-size 20)
            (beacon-color "deep pink"))
        (beacon-blink)))
    (add-hook 'yas-after-exit-snippet-hook #'my/yas-exit-animation))

  (yas-global-mode 1))

;;; [ yasnippet-snippets ] -- Official snippet collection for the yasnippet package.

;; (use-package yasnippet-snippets
;;   :ensure t)

;;; [ ivy-yasnippet ] -- preview yasnippet snippets with Ivy.

(use-package ivy-yasnippet ; [C-c \]
  :ensure t
  :defer t
  :bind (:map yas-minor-mode-map ([remap yas-insert-snippet] . ivy-yasnippet)))


;;; code snippets capture template
(defun my/org-capture-get-src-block-string (major-mode)
  "Given a major mode symbol, return the associated org-src block
string that will enable syntax highlighting for that language

E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

(defun my/org-capture-code-snippet (f)
  (with-current-buffer (find-buffer-visiting f)
    (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
          (func-name (read-from-minibuffer "Function name: "))
          (file-name (buffer-file-name))
          (line-number (line-number-at-pos (region-beginning)))
          (org-src-mode (my/org-capture-get-src-block-string major-mode)))
      (format
       "file:%s::%s
In ~%s~:

#+begin_src %s
%s
#+end_src"
       file-name
       line-number
       func-name
       org-src-mode
       code-snippet))))

;; use region select to capture.
(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               `("s" ,(format "%s\tcreate new code snippet"
                              (all-the-icons-faicon "code" :face 'all-the-icons-cyan :v-adjust 0.05))
                 entry (file (lambda () (concat org-directory "/Programming Code/Code Snippets/snippets.org")))
                 "* %?\n%(my/org-capture-code-snippet \"%F\")")))

;;; [ org-sync-snippets ] -- simple extension to export snippets to org-mode and vice versa.

;; (use-package org-sync-snippets
;;   :ensure t
;;   :defer t
;;   :after org ; to fix variable `org-directory' is not customized to "~/Org" issue.
;;   :commands (org-sync-snippets-snippets-to-org org-sync-snippets-org-to-snippets)
;;   :init (setq org-sync-snippets-snippets-dir (expand-file-name "snippets/" user-emacs-directory)
;;               org-sync-snippets-org-snippets-file
;;               (concat (file-name-as-directory org-directory)
;;                       "Programming Code/Code Snippets/yasnippets.org"))
;;   (add-hook 'yas-after-reload-hook 'org-sync-snippets-snippets-to-org))

;;; [ code-archive ] -- saving selecting code regions and inserting them as org-mode styled code blocks.

;; (use-package code-archive
;;   :ensure t
;;   :defer t
;;   :commands (code-archive-save-code code-archive-insert-org-block code-archive-goto-src)
;;   :custom (code-archive-dir (concat org-directory "/Programming Code/Code Snippets/"))
;;   :init
;;   (with-eval-after-load 'org-capture
;;     (add-to-list 'org-capture-templates
;;                  `("s" ,(format "%s\tcode snippet archive"
;;                                 (all-the-icons-faicon "code" :face 'all-the-icons-cyan))
;;                    entry
;;                    (file (lambda () (concat code-archive-dir "snippets.org")))
;;                    "* %? %(code-archive-org-src-tag \"%F\")
;; :PROPERTIES:
;; :FILE:  %F
;; :END:
;; %(code-archive-do-org-capture \"%F\")"))))


(provide 'init-prog-snippet)

;;; init-prog-snippet.el ends here
