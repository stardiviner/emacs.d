;;; init-my-prog-snippet.el --- init Programming snippet engines

;;; Commentary:


;;; Code:

;;; [ YASnippet ] --- (template/snippet engine)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ; personal snippets directory
          ;; "~/.emacs.d/el-get/yasnippet/snippets/" YASnippet bundled snippets
          ))

  ;; (setq yas-verbosity 0)

  ;; keybindings
  (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand) ; (kbd "<tab>") is same with [tab]
  (define-key yas-minor-mode-map (kbd "TAB") #'indent-for-tab-command)
  (setq yas-trigger-key "<tab>") ; old value: "TAB"
  (setq yas-next-field-key '("<tab>"))
  (setq yas-prev-field-key '("<S-tab>" "<backtab>"))
  (setq yas-skip-and-clear-key '("C-d"))

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
  (setq yas-trigger-symbol (char-to-string ?\x21E5))

  ;; (setq yas-key-syntaxes '("w" "w_" "w_." "w_.()" yas-try-key-from-whitespace))

  ;; for `yas-choose-value'.
  (setq yas-prompt-functions
        '(yas-dropdown-prompt       ; work both in minibuffer and X window system.
          yas-completing-prompt     ; minibuffer prompting.
          yas-ido-prompt            ; minibuffer prompting.
          yas-x-prompt              ; X window system.
          yas-no-prompt))

  (setq yas-new-snippet-default "\
# -*- mode: snippet; require-final-newline: nil -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# group: ${4:group.subgroup}
# binding: ${5:direct-keybinding}}${6:
# expand-env: ((${7:some-var} ${8:some-value}))}${9:
# type: snippet/command}
# --
$0"
        )

  (setq yas-good-grace t)

  ;; auto set major mode: snippet-mode.
  (add-to-list 'auto-mode-alist
               '("\\.yasnippet$" . snippet-mode)
               '("\\.snippet$" . snippet-mode))
  (add-hook 'snippet-mode
            (lambda ()
              ;; turn of auto-fill for long length code
              (turn-off-auto-fill)))

  ;; Faces
  (set-face-attribute 'yas-field-highlight-face nil
                      :background "#555555" :foreground "white"
                      :overline "black"
                      )

  ;;; use different way to notify user the snippet exited.
  (add-hook 'yas-after-exit-snippet-hook
            (lambda ()
              (popup-tip "snippet exited")
              ))

  (yas-global-mode 1)
  )


;; auto set major mode: snippet-mode.

(add-to-list 'auto-mode-alist
             '("\\.yasnippet$" . snippet-mode)
             '("\\.snippet$" . snippet-mode))

(add-hook 'snippet-mode
          (lambda ()
            ;; turn of auto-fill for long length code
            (turn-off-auto-fill)))


;;; Faces

(set-face-attribute 'yas-field-highlight-face nil
                    :background "#555555" :foreground "white"
                    :overline "black"
                    )


;;; use different way to notify user the snippet exited.

(add-hook 'yas-after-exit-snippet-hook
          (lambda ()
            (popup-tip "snippet exited")
            ))


;; It will test whether it can expand, if yes, cursor color -> green.

;; (require 'cursor-chg)

;; (defun yasnippet-can-fire-p (&optional field)
;;   (interactive)
;;   (setq yas--condition-cache-timestamp (current-time))
;;   (let (templates-and-pos)
;;     (unless (and yas-expand-only-for-last-commands
;;                  (not (member last-command yas-expand-only-for-last-commands)))
;;       (setq templates-and-pos (if field
;;                                   (save-restriction
;;                                     (narrow-to-region (yas--field-start field)
;;                                                       (yas--field-end field))
;;                                     (yas--templates-for-key-at-point))
;;                                 (yas--templates-for-key-at-point))))

;;     (set-cursor-color (if (and templates-and-pos (first templates-and-pos)) 
;;                           "yellow" "cyan"))
;;     (curchg-set-cursor-type (if (and templates-and-pos (first templates-and-pos))
;;                                 "bar" "hbar"))))

;; ;; As pointed out by Dmitri, this will make sure it will update color when needed.
;; (add-hook 'post-command-hook 'yasnippet-can-fire-p)


;;; [ yasnippet-snippets ]


;;; [ yasnippets ]




(provide 'init-my-prog-snippet)

;;; init-my-prog-snippet.el ends here
