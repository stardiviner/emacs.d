;;; init-my-prog-snippet.el --- init Programming snippet engines

;;; Commentary:


;;; Code:

;;; [ YASnippet ] --- (template/snippet engine)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ; personal snippets directory
          ;; "~/.emacs.d/el-get/yasnippet/snippets/" YASnippet bundled snippets
          ))

  ;; (setq yas-verbosity 0)

  ;; keybindings
  ;; (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand) ; (kbd "<tab>") is same with [tab]
  ;; (define-key yas-minor-mode-map (kbd "TAB") #'indent-for-tab-command)

  ;; quick shortcut for inserting snipppet with completing system.
  (define-key yas-minor-mode-map (kbd "C-c \\") 'yas-insert-snippet)

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

  ;; menu
  (setq yas-use-menu 'abbreviate)

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
$0"
        )

  (setq yas-good-grace t)

  ;; auto set major mode: snippet-mode.
  (add-to-list 'auto-mode-alist
               '("\\.yasnippet$" . snippet-mode)
               '("\\.snippet$" . snippet-mode))

  ;; turn of auto-fill for long length code
  (add-hook 'snippet-mode #'turn-off-auto-fill)

  ;; Faces
  (set-face-attribute 'yas-field-highlight-face nil
                      :background "#555555" :foreground "white"
                      :overline "black"
                      )

  (defun my-yas-exit-animation ()
    ;; (popup-tip "snippet exited")
    ;; (message "snippet exited")
    (let ((beacon-size 20)
          (beacon-color "deep pink"))
      (beacon-blink))
    )
  (add-hook 'yas-after-exit-snippet-hook #'my-yas-exit-animation)
  
  ;; change cursor dynamically when prefix text is expandable.
  (defun my-cursor-change-on-snippet ()
    (interactive)
    ;; (yas-next-field-or-maybe-expand)
    (if (or yas-triggers-in-field ; FIXME: improve condition
            (yas-next-field-will-exit-p))
        (progn
          (set-cursor-color "red")
          (setq cursor-type 'hollow))
      (set-cursor-color "cyan")
      (setq cursor-type '(hbar . 2))
      )
    )

  ;; (add-hook 'post-command-hook #'my-cursor-change-on-snippet)
  
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
