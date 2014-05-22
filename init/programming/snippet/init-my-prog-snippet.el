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
;;   - [M-x yas-new-snippet]
;;   - [M-x yas-visit-snippet-file]
;; - Using the "snippet-mode" major mode
;;   - [M-x snippet-mode]
;;       -- provide syntax highlighting.
;;   - [C-c C-c] / [M-x yas-load-snippet-buffer]
;;       -- load current editing snippet into correct mode and menu.
;;   - [C-c C-t] / [M-x yas-tryout-snippet]
;;       -- open a new empty buffer when editing a snippet, sets it to the
;;          appropriate major mode and inserts the snippet there, so you can
;;          see what it looks like.
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

;;; In (yas-new-snippet) buffer
;;
;; - =[C-c C-c]= :: =(yas-load-snippet-buffer-and-close)=
;; - =[C-c C-l]= :: =(yas-load-snippet-buffer)=
;; - =[C-c C-t]= :: =(yas-tryout-snippet)=


(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ; personal snippets directory
        ;; "~/.emacs.d/el-get/yasnippet/snippets/" YASnippet bundled snippets
        ))


;;; keybindings
(setq yas-trigger-key "TAB")
(setq yas-next-field-key '("<tab>"))
(setq yas-prev-field-key '("<S-tab>" "<backtab>"))
(setq yas-skip-and-clear-key '("C-d"))

;; (define-key yas-minor-mode (kbd "") 'yas-abort-snippet)



;;; indent
(setq yas-indent-line 'auto) ; 'auto, 'fixed
(setq yas-also-auto-indent-first-line nil)

;;; wrap around region
(setq yas-wrap-around-region t) ; t: snippet expansion wraps around selected region.

;; stacked expansion
(setq yas-triggers-in-field t) ; t: allow stacked expansions (snippets inside field).

(setq yas-snippet-revival t) ; t: re-activate snippet field after undo/redo.

;; TODO: (setq yas--extra-modes) ; An internal list of modes for which to also lookup snippets.
;; TODO: (setq yas--guessed-modes)

;;; menu
(setq yas-use-menu 'abbreviate)
(setq yas-trigger-symbol " =>") ; the text used in menu to represent the trigger.


(setq yas-key-syntaxes
      '("w" "w_" "w_." "w_.()" "^ ") ; default
      )

;; for `yas-choose-value'.
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-completing-prompt
                             yas-ido-prompt
                             yas-x-prompt
                             yas-no-prompt))

(setq yas-new-snippet-default "\
# -*- mode: snippet; require-final-newline: nil -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((${6:some-var} ${7:some-value}))}${8:
# type: command}
# --
$0"
      )

(setq yas-good-grace t)


;; auto set major mode: snippet-mode.
(add-to-list 'auto-mode-alist
             '("\\.yasnippet$" . snippet-mode)
             '("\\.snippet$" . snippet-mode))

(add-hook 'snippet-mode (lambda ()
                          ;; turn of auto-fill for long length code
                          (turn-off-auto-fill)))


;; ???
;; (yas--document-symbols 2 `("Interactive functions" . ,#'interactive-form)
;;                        `("Customization variables" . ,#'(lambda (sym)
;;                                                           (and (boundp sym)
;;                                                                (get sym 'standard-value))))
;;                        `("Useful functions" . ,#'fboundp)
;;                        `("Useful variables" . ,#'boundp))


;;; set function `yas-new-snippet' default template
;; (setq yas-new-snippet-default "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:\n# binding: ${4:direct-keybinding}}${5:\n# expand-env: ((${6:some-var} ${7:some-value}))}${8:\n# type: command}\n# --\n$0")


;;; Faces
(set-face-attribute 'yas-field-highlight-face nil
                    :background "black" :foreground " "
                    :box '(:color "#009494" :line-width 1 :style nil)
                    )
(set-face-attribute 'yas--field-debug-face nil
                    :background " " :foreground " "
                    :box nil
                    ;; :box '(:color "cyan")
                    )


;;; enable YASnippet

(yas-global-mode 1) ; or [M-x yas-reload-all] if you've started YASnippet already.

;;
;; use yas-minor-mode on a pre-buffer basis To use YASnippet as a non-global
;; minor mode, replace (yas-global-mode 1) with (yas-reload-all) to load the
;; snippet tables. Then add a call to (yas-minor-mode) to the major-modes where
;; you to enable YASnippet.
;; (dolist (hook
;;          '(prog-mode-hook
;;            org-mode-hook
;;            ))
;;   (add-hook hook '(lambda () (yas-minor-mode))))

(diminish 'yas-minor-mode)




(provide 'init-my-prog-snippet)

;;; init-my-prog-snippet.el ends here
