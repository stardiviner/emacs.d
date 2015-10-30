;;; init-my-prog-snippet.el --- init Programming snippet engines

;;; Commentary:


;;; Code:

;;; [ YASnippet ] --- (template/snippet engine)

;; Usage:
;; - [TAB] -- \t, to expand and jump to next field.
;; - [<tab>] -- [tab],
;; - [C-d]
;;     (yas-skip-and-clear-or-delete-char)
;;      -- to clear unmodified field if at field start, skips to next tab.
;; - [M-x yas-reload-all] -- reload all snippets.
;; - (yas-next-field-or-maybe-expand)
;; - (yas/minor-mode)
;; - (yas/global-mode)
;; - (yas/expand-snippet template)
;; - [M-x snippet-mode]
;; - (yas-before-expand-snippet-hook)
;; - (yas-after-exit-snippet-hook)
;; - (yas-activate-extra-mode (mode))
;;    Activates the snippets for the given mode in the buffer.
;;    The function can be called in the hook of a minor mode to activate snippets associated with that mode.
;; - (yas-deactivate-extra-mode (mode))
;;    Deactives the snippets for the given mode in the buffer.
;; - (yas--modes-to-active) :: show what modes snippet is active in current buffer.

;; Expanding Snippets:
;;   * triggering expansion
;;     - type an snippet's *trigger key*
;;     - press yas-trigger-key (usually to "TAB"), then press [TAB] again to jump to next field.
;;     - use the snippet's *keybinding*.
;;     - call yas-insert-snippet
;;     - use [C-c & C-s] or [M-x yas-insert-snippet]
;;     - using hippie-expand
;;     - use auto-complete
;; Snippet development:
;; - quickly finding snippets
;;   - [C-c & C-n] / [M-x yas-new-snippet]
;;   - [C-c & C-v] / [M-x yas-visit-snippet-file]
;; - Using the "snippet-mode" major mode
;;   - [M-x snippet-mode]
;;       -- provide syntax highlighting.
;;   - [C-c C-c] / [M-x yas-load-snippet-buffer]
;;       -- load current editing snippet into correct mode and menu.
;;   - [C-c C-t] / [M-x yas-tryout-snippet]
;;       -- open a new empty buffer when editing a snippet, sets it to the
;;          appropriate major mode and inserts the snippet there, so you can
;;          see what it looks like.
;;; In (yas-new-snippet) buffer
;;
;; - =[C-c C-c]= :: =(yas-load-snippet-buffer-and-close)=
;; - =[C-c C-l]= :: =(yas-load-snippet-buffer)=
;; - =[C-c C-t]= :: =(yas-tryout-snippet)=
;;
;;; The `.yas-parents' file.
;;
;; It's very useful to have certain modes share snippets between themselves. To
;; do this, choose a mode subdirectory and place a .yas-parents containing a
;; whitespace-separated list of other mode names. When you reload those modes
;; become parents of the original mode.
;;
;; Structure:
;; .
;; |-- cc-mode
;; |   `-- syntax/for
;; |              while
;; |-- c-mode
;; |   `-- .yas-parents   # contains "cc-mode text-mode"
;; |   `-- syntax/while.yasnippet
;; |   `-- printf.yasnippet
;; |-- java-mode
;; |   `-- println
;; `-- text-mode
;; |-- email
;; `-- time
;;
;; - [text-mode/perl-mode/doctype]
;; Each file (may) end with the suffix ".yasnippet".
;; [doctype]+[TAB], a multiple choice menu will be shown.
;; - doctype.xhtml1
;; - doctype.xhtml1_1
;; - doctype.xhtml2
;;
;; File names starting with a period are not template definition but provide
;; information purposes. For example: [.readme].


(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ; personal snippets directory
        ;; "~/.emacs.d/el-get/yasnippet/snippets/" YASnippet bundled snippets
        ))

;; (setq yas-verbosity 0)


;;; keybindings

(define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand) ; (kbd "<tab>") is same with [tab]
(define-key yas-minor-mode-map (kbd "TAB") #'indent-for-tab-command)

(setq yas-trigger-key "<tab>") ; old value: "TAB"
(setq yas-next-field-key '("<tab>"))
(setq yas-prev-field-key '("<S-tab>" "<backtab>"))
(setq yas-skip-and-clear-key '("C-d"))



;;; indent

(setq yas-indent-line 'auto) ; 'auto, 'fixed
(setq yas-also-auto-indent-first-line nil)

;;; Python indent issue

(add-hook 'python-mode-hook
          (lambda ()
            (make-local-variable 'yas-indent-line)
            (setq yas-indent-line 'fixed)))


;;; wrap around region
(setq yas-wrap-around-region t) ; t: snippet expansion wraps around selected region.

;; stacked expansion
(setq yas-triggers-in-field t) ; t: allow stacked expansions (snippets inside field).

(setq yas-snippet-revival t) ; t: re-activate snippet field after undo/redo.

;; TODO: (setq yas--extra-modes) ; An internal list of modes for which to also lookup snippets.
;; TODO: (setq yas--guessed-modes)

;;; menu
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


;;; Faces

(set-face-attribute 'yas-field-highlight-face nil
                    :background "khaki" :foreground "#444444"
                    :overline "black"
                    )
(set-face-attribute 'yas--field-debug-face nil
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


;;; enable YASnippet

(yas-global-mode 1)


;;; [ yasnippet-snippets ]


;;; [ yasnippets ]




(provide 'init-my-prog-snippet)

;;; init-my-prog-snippet.el ends here
