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


;;; keybindings
;; - [Tab]
;; - or [C-Tab]

;;; source code implement
;; (defvar yas-keymap  (let ((map (make-sparse-keymap)))
;;                       (define-key map [(tab)]       'yas-next-field-or-maybe-expand)
;;                       (define-key map (kbd "TAB")   'yas-next-field-or-maybe-expand)
;;                       (define-key map [(shift tab)] 'yas-prev-field)
;;                       (define-key map [backtab]     'yas-prev-field)
;;                       (define-key map (kbd "C-g")   'yas-abort-snippet)
;;                       (define-key map (kbd "C-d")   'yas-skip-and-clear-or-delete-char)
;;                       map)
;;   "The active keymap while a snippet expansion is in progress.")

(setq yas-trigger-key "<tab>") ; old value: "TAB"
(setq yas-next-field-key '("<tab>"))
(setq yas-prev-field-key '("<S-tab>" "<backtab>"))
(setq yas-skip-and-clear-key '("C-d"))



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


;; (setq yas-key-syntaxes '("w" "w_" "w_." "w_.()" "^ "))

;; for `yas-choose-value'.
(setq yas-prompt-functions
      '(yas-dropdown-prompt       ; work both in minibuffer and X window system.
        yas-completing-prompt     ; minibuffer prompting.
        yas-ido-prompt            ; minibuffer prompting.
        yas-x-prompt              ; X window system.
        yas-no-prompt))

;; (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
;;   (when (featurep 'popup)
;;     (popup-menu*
;;      (mapcar
;;       (lambda (choice)
;;         (popup-make-item
;;          (or (and display-fn (funcall display-fn choice))
;;              choice)
;;          :value choice))
;;       choices)
;;      :prompt prompt
;;      ;; start isearch mode immediately
;;      :isearch t
;;      )))

;;; TODO: https://github.com/capitaomorte/yasnippet/issues/488
;; improve this two functions.
;; (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
;;   (popup-menu*
;;    (mapcar
;;     (lambda (choice)
;;       (popup-make-item
;;        (if (yas--template-p choice)
;;            (format "%10.10s %-10.10s  %s"
;;                    (if (yas--template-group choice)
;;                        (s-join "/" (yas--template-group choice))
;;                      "")
;;                    (if (yas--template-key choice)
;;                        (yas--template-key choice)
;;                      "")
;;                    (if (yas--template-name choice)
;;                        (yas--template-name choice)
;;                      ""))
;;          choice)
;;        :value choice))
;;     choices)
;;    :prompt prompt
;;    :max-width 80
;;    :isearch t))

;; FIXME: this should be niced up and contributed back.
;; (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
;;   (let ((group-max-len 0)
;;         (key-max-len 0)
;;         (fmt "")
;;         (popup-items))
;;
;;     (mapcar #'(lambda (choice)
;;                 (when (yas--template-p choice)
;;                   (setq group-max-len (max group-max-len
;;                                            (+ (length (yas--template-group choice) )
;;                                               (apply '+ (mapcar 'length (yas--template-group choice))))))
;;                   (setq key-max-len (max key-max-len (length (yas--template-key choice))))))
;;             choices)
;;
;;     (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds  %%s"
;;                       (if (> group-max-len 0 ) "" " ")
;;                       group-max-len group-max-len
;;                       (if (> group-max-len 0 ) " > " "")
;;                       key-max-len key-max-len))
;;
;;     (setq popup-items
;;           (mapcar
;;            #'(lambda (choice)
;;                (popup-make-item
;;                 (if (yas--template-p choice)
;;                     (format fmt
;;                             (if (yas--template-group choice)
;;                                 (s-join "/" (yas--template-group choice))
;;                               "")
;;                             (if (yas--template-key choice)
;;                                 (yas--template-key choice)
;;                               "")
;;                             (if (yas--template-name choice)
;;                                 (yas--template-name choice)
;;                               ""))
;;                   (format " %s" choice))
;;                 :value choice))
;;            choices))
;;
;;     (popup-menu*
;;      popup-items
;;      :prompt prompt
;;      :max-width 80
;;      :isearch t)))


;; (add-to-list 'yas-prompt-functions 'yas-popup-isearch-prompt)



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

(setq yas-dont-activate '(minibufferp))


;; It will test whether it can expand, if yes, cursor color -> green.

(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))

    (set-cursor-color (if (and templates-and-pos (first templates-and-pos)) 
                          "yellow" "cyan"))
    (curchg-set-cursor-type (if (and templates-and-pos (first templates-and-pos))
                                "bar" "hbar"))))

;; As pointed out by Dmitri, this will make sure it will update color when needed.
(add-hook 'post-command-hook 'yasnippet-can-fire-p)
(diminish 'yas-minor-mode)




(provide 'init-my-prog-snippet)

;;; init-my-prog-snippet.el ends here
