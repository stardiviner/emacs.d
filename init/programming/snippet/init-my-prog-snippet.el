;;; init-my-prog-snippet.el --- init Programming snippet engines

;;; Commentary:


;;; Code:

;;; [ YASnippet ] --- (template/snippet engine)

(use-package yasnippet
  :ensure t
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

  ;; auto set major mode: snippet-mode.
  (add-to-list 'auto-mode-alist
               '("\\.yasnippet$" . snippet-mode)
               '("\\.snippet$" . snippet-mode))

  ;; turn of auto-fill for long length code
  (add-hook 'snippet-mode #'turn-off-auto-fill)

  ;; Faces
  (set-face-attribute 'yas-field-highlight-face nil
                      ;; :inherit 'region
                      :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('dark "gray")
                                    ('light "black"))
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('dark
                                     (color-lighten-name (face-background 'default) 10))
                                    ('light
                                     (color-darken-name (face-background 'default) 4)))
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
  
  ;; change cursor dynamically when prefix text is expandable.
  ;; `yas--templates-for-key-at-point'.
  (defvar yas--expandable-keys-overlay nil)

  (defun yas-show-expand-keys ()
    "Put overlay on text which is an expandable snippet key.
This function is intended to be added to `post-command-hook'."
    (let ((keys-at-point (and yas-minor-mode (yas--templates-for-key-at-point)))
          (have-overlay (overlayp (buffer-local-value 'yas--expandable-keys-overlay (current-buffer)))))
      (if keys-at-point
          (let ((beg (nth 1 keys-at-point))
                (end (nth 2 keys-at-point)))
            (if have-overlay
                (move-overlay yas--expandable-keys-overlay beg end)
              (setq-local yas--expandable-keys-overlay
                          (make-overlay beg end)))
            (overlay-put yas--expandable-keys-overlay 'face '(:box t)))
        (when have-overlay
          (delete-overlay yas--expandable-keys-overlay)))))

  (add-hook 'post-command-hook #'yas-show-expand-keys)

  
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




(provide 'init-my-prog-snippet)

;;; init-my-prog-snippet.el ends here
