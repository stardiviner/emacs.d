;;; init-my-emacs-edit.el --- init Emacs editing
;;; Commentary:

;;; Code:

;;;_ undo-tree
;; [C-x u] -> undo-tree-visualizer-mode
;;    `- [C-p/n]  -- move up/down
;;    `- [C-b/f]  -- move left/right
;;    `- t    -- timestamp
;;    `- q    -- quit

(require 'undo-tree)

(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;;;_ Edit

(delete-selection-mode t)             ; typed text replaces the active selection

;;;_ Markers

;;;_ Rectangle

;;; - [C-x SPC] / [C-x r r m] (custom keybinding) :: `rectangle-mark-mode'

;;;_ kill-ring-search

;; Copied something important half an hour ago? Tired of hitting M-y 20 times?
;; Now you can search the kill ring incrementally and yank the result!
;; Just hit M-C-y to start the search. M-y and C-y work as usual.

;; (require 'kill-ring-search)
;;
;; (autoload 'kill-ring-search "kill-ring-search"
;;   "Search the kill ring in the minibuffer."
;;   (interactive))
;;
;; (global-set-key (kbd "C-M-y") 'kill-ring-search)

;;;_ kill-ring-ido

;;; Usage:
;; - [C-M-y] ::

;; (if (featurep 'ido)
;;     (require 'kill-ring-ido)
;;   (global-set-key (kbd "C-M-y") 'kill-ring-ido))



;;;_ [ edit-server ]

;;; Usage:
;;
;; edit browser text-area.

(require 'edit-server)

(when (require 'edit-server nil t)
  (setq edit-server-new-frame t)
  (edit-server-start))

(setq edit-server-url-major-mode-alist
      '(("github\\.com" . markdown-mode)
        ("stackoverflow\\.com" . markdown-mode)
        ("segmentfault\\.com" . markdown-mode)
        ))

;;;_ Macro

;;; Usage:
;; - [C-x C-k] -- macro prefix.
;; - [C-x (/)] -- start/end macro.
;; - [F3/F4] -- start/end macro or insert counter/repeat.

;;;_ query & replace

(setq query-replace-highlight t
      query-replace-lazy-highlight t
      query-replace-show-replacement t
      ;; TODO: added in Emacs 25.
      ;; query-replace-from-to-separator
      )

;;;_ Electric

;; NOTE: when electric auto insert ) for (, but when you delete (, electric will not auto delete ) for you.

;; NOTICE: this conflict with ParEdit and auto-pair.
;; (when (fboundp 'electric-pair-mode)
;;   (setq-default electric-pair-mode 1))
;; ------------------------------------------
;; (electric-pair-mode t) ; automatically insert delimiter pairs.

;; (electric-indent-mode t)

;; (dolist (hook
;;          '(org-mode-hook
;;            ruby-mode-hook
;;            python-mode-hook
;;            html-mode-hook
;;            css-mode-hook
;;            c-mode-hook
;;            ;; ess-mode-hook                ; Emacs Speaks Statistics
;;            ))
;;   (add-hook hook #'(lambda () (electric-pair-mode t))))

;;;_ skeleton

(setq skeleton-pair t
      skeleton-pair-alist
      '((?\" _ "\"" >)
        (?\' _ "\'" >)
        (?\( _ ")" >)
        (?\[ _ "]" >)
        (?\{ _ "}" >)
        ;; chinese pairs
        (?“ _ "”" >)
        (?‘ _ "’" >)
        (?\（ _ "）" >)
        (?\【 _ "】" >)
        (?\〖 _ "〗" >)
        )
      )

;;;_ Narrowing

;;; Usage:
;; - prefix --> [C-x n]
;; - [C-x n n] -- narrow to region
;; - [C-x n w] -- widen (undo narrow)

;;; don't disable narrowing functions
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)

(unless (boundp 'my-narrow-prefix-map)
  (define-prefix-command 'my-narrow-prefix-map))
(define-key my-edit-prefix-map (kbd "n") 'my-narrow-prefix-map)

(define-key my-narrow-prefix-map (kbd "w") 'widen)
(define-key my-narrow-prefix-map (kbd "n") 'narrow-to-region)
(define-key my-narrow-prefix-map (kbd "r") 'narrow-to-region)
(define-key my-narrow-prefix-map (kbd "d") 'narrow-to-defun)
(define-key my-narrow-prefix-map (kbd "p") 'narrow-to-page)

;;; custom keybinding for handy (narrow + indirect-buffer)
;; Usage: [C-x n i], you can kill narrowed indirect buffer like normal buffer with [C-x k]. the modification will keep.
;; FIXME: the region highlight doesn't disappear, this is a problem.
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

;; (global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)
(define-key my-narrow-prefix-map (kbd "i") 'narrow-to-region-indirect)


(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

(define-key narrow-map "r" 'narrow-to-region) ; backup `narrow-to-region'.
(define-key narrow-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
;; (define-key ctl-x-map "n" #'narrow-or-widen-dwim)


;;;_ Mark --- [C-SPC / C-@] + [C-u C-SPC / C-u C-@] + [C-`] / [M-`]

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

;; (defun exchange-point-and-mark-no-activate ()
;;   "Identical to \\[exchange-point-and-mark] but will not activate the region."
;;   (interactive)
;;   (exchange-point-and-mark)
;;   (deactivate-mark nil))
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)


;;;_ expand-region

;;; Expand region increases the selected region by semantic units. Just keep
;;; pressing the key until it selects what you want.

(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

;;;_ Multiple Narrowings


;;;_ capitalized-words-mode

;; (capitalized-words-mode 1)

;;;_ auto-capitalize

;;; Usage:
;; - auto capitalize words of an sentence.
;; - prevent a word in the `auto-capitalize-words' list from being capitalized or upcased in a particular context.
;;   e.g. ("GNU.emacs.sources"), insert the following whitepsace or punctuation character with:
;;   `M-x quoted insert' (e.g. `gnu C-q .').

;; (autoload 'auto-capitalize "auto-capitalize" "Autoload function `auto-capitalize'." t)
;; (autoload 'auto-capitalize-mode "auto-capitalize" "Toggle `auto-capitalize' minor mode in the buffer." t)
;; (autoload 'turn-on-auto-capitalize-mode "auto-capitalize" "Turn on `auto-capitalize' minor mode in the buffer." t)
;; (autoload 'enable-auto-capitalize-mode "auto-capitalize" "Enable `auto-capitalize' minor mode in the buffer." t)

;; (setq auto-capitalize-yank nil
;;       auto-capitalize-words '("\\<Emacs\\>" "\\<Linux\\>"
;;                               "\\<Android\>>" "\\<iOS\\>" "\\<Mac\\>")
;;       ;; TODO: auto-capitalize-predicate
;;       )

;;; To turn on (unconditional) capitalization in all Text modes.
;; (turn-on-auto-capitalize-mode)
;;; To enable (interactive) capitalization in all Text modes.
;; (enable-auto-capitalize-mode)

;; (dolist (hook '(text-mode-hook
;;                 org-mode-hook
;;                 markdown-mode-hook
;;                 ;; latex-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (turn-on-auto-capitalize-mode))))


;;;_ [ iedit ] -- Edit multiple regions simultaneously in a buffer or a region
;;;
;;; This package includes Emacs minor modes (iedit-mode and
;;; iedit-rectangle-mode) based on a API library (iedit-lib) and allows you to
;;; edit one occurrence of some text in a buffer (possibly narrowed) or region,
;;; and simultaneously have other occurrences edited in the same way, with
;;; visual feedback as you type.
;;
;; Normal scenario of Iedit mode is like:
;;
;; 1. Highlight certain contents - by press [C-;] All occurrences of a symbol,
;;    string or a rectangle in the buffer or a region may be highlighted
;;    corresponding to current mark, point and prefix argument. Refer to the
;;    document of `iedit-mode’ for details.
;;
;; 2. Edit one of the occurrences The change is applied to other occurrences
;;    simultaneously.
;;
;; 3. Finish - by pressing [C-;] again
;;
;; You can also use Iedit mode as a quick way to temporarily show only the
;; buffer lines that match the current text being edited. This gives you the
;; effect of a temporary `keep-lines’ or `occur’. To get this effect, hit C-’
;; when in Iedit mode - it toggles hiding non-matching lines.
;;
;; Renaming refactoring is convinient in Iedit mode
;; - The symbol under point is selected as occurrence by default and only complete symbols are matched
;; - With digit prefix argument 0, only occurrences in current function are matched
;; - Restricting symbols in current region can be done by pressing C-; again
;; - Last renaming refactoring is remembered and can be applied to other buffers later
;; - Restricting the search area to just the current line can be done by pressing M-I.
;; - Restricting the search area to the lines near the current line can be done
;;   by pressing M-{ and M-}. These will expand the search region one line at a
;;   time from the top and bottom. Add a prefix argument to go the opposite
;;   direction.

;;; Iedit-rectangle-mode provides rectangle support with visible rectangle
;;; highlighting, which is similar with cua mode rectangle support. But it’s
;;; lighter weight and uses iedit mechanisms.

;;; There are also some other facilities you may never think about. Refer to the
;;; document of function `iedit-mode’ (C-h f iedit-mode RET) for more details.

;;; Usage:
;;
;; - [C-h iedit-mode RET] -- to get help of iedit-mode
;; - [M-x iedit-mode]
;;
;; - [C-;] -- highlight certain contents
;; - [C-'] -- toggle unmatched lines visible
;; - [M-;] -- apply global modification
;;
;; - [Tab] -- next occurrence
;; - [S-Tab] -- prev occurrence
;; - [M-<] -- first occurrence
;; - [M->] -- last  occurrence
;;
;; - [M-b] -- toggle buffering
;; - [M-c] -- toggle case sensitive
;;
;; - [M-d] -- restrict function
;;
;; - [M-d] -- delete occurrences
;; - [M-SPC] -- blank occurences
;; - [M-l] -- downcase occurrences
;; - [M-u] -- upcase occurrences
;; - [M-n] -- number occurrences
;; - [M-r] -- replace occurrences
;;
;; --------------------------------
;;
;; - [M-x iedit-rectangle-mode] -- visible rectangle.
;; - [M-k] -- Iedit kill rectangle.
;; Steps:
;; - mark a rectangle like Emacs rectangle with [C-@ / C-SPC].
;; - after marked the rectangle, then press [C-c C-;] to enable iedit rectangle mode, and highlight the rectangle.

(require 'iedit)

(autoload 'iedit-mode "Iedit" "Edit multiple regions with the same content simultaneously." t)
(autoload 'iedit-rectangle-mode "Iedit rectangle" "Edit narrowed text." t)

(setq iedit-occurrence-face 'isearch) ; 'highlight

(defun iedit-dwim (arg)
  "If ARG, start iedit but use \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

;; (global-set-key (kbd "C-;") 'iedit-mode)
(define-key my-edit-prefix-map (kbd "e") 'iedit-dwim)


;;;_ [ multiple-cursors ]

;;; Usage:
;;
;; https://github.com/magnars/multiple-cursors.el
;; - [C-c c] -- prefix of mc.
;; - <return> / [C-g] -- get out of multiple-cursors
;; - [C-c e c c] -- `mc/mark-next-like-this'
;; - [C-c e c r] -- `set-rectangular-region-anchor'
;; - [region] + [C-c e c m] -- mark variants (region is used to limit mark area)
;;
;; Unknown commands
;;
;; Multiple-cursors uses two lists of commands to know what to do: the run-once
;; list and the run-for-all list. It comes with a set of defaults, but it would
;; be beyond silly to try and include all the known Emacs commands.
;;
;; So that's why multiple-cursors occasionally asks what to do about a
;; command. It will then remember your choice by saving it in
;;
;;     `~/.emacs.d/.mc-lists.el' You can change the location with:

(require 'multiple-cursors)

(setq mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory))

;; (setq mc/keymap "C-c c")

(unless (boundp 'my-mc-prefix-map)
  (define-prefix-command 'my-mc-prefix-map))
(define-key my-edit-prefix-map (kbd "c") 'my-mc-prefix-map)

(define-key my-mc-prefix-map (kbd "c") 'mc/mark-all-dwim)
(define-key my-mc-prefix-map (kbd "r") 'set-rectangular-region-anchor)
(define-key my-mc-prefix-map (kbd "l") 'mc/edit-lines)
(define-key my-mc-prefix-map (kbd "a") 'mc/edit-beginnings-of-lines)
(define-key my-mc-prefix-map (kbd "e") 'mc/edit-ends-of-lines)
(define-key my-mc-prefix-map (kbd "n") 'mc/insert-numbers)
(define-key my-mc-prefix-map (kbd "s") 'mc/sort-regions)
(define-key my-mc-prefix-map (kbd "R") 'mc/reverse-regions)

(unless (boundp 'my-mc/mark-prefix-map)
  (define-prefix-command 'my-mc/mark-prefix-map))
(define-key my-mc-prefix-map (kbd "m") 'my-mc/mark-prefix-map)

(define-key my-mc/mark-prefix-map (kbd "a a") 'mc/mark-all-like-this-dwim)
(define-key my-mc/mark-prefix-map (kbd "a l") 'mc/mark-all-like-this)
(define-key my-mc/mark-prefix-map (kbd "a w") 'mc/mark-all-words-like-this)
(define-key my-mc/mark-prefix-map (kbd "a s") 'mc/mark-all-symbols-like-this)
(define-key my-mc/mark-prefix-map (kbd "a r") 'mc/mark-all-in-region)
(define-key my-mc/mark-prefix-map (kbd "a f") 'mc/mark-all-like-this-in-defun)
(define-key my-mc/mark-prefix-map (kbd "a F") 'mc/mark-all-words-like-this-in-defun)
(define-key my-mc/mark-prefix-map (kbd "a S") 'mc/mark-all-symbols-like-this-in-defun)
(define-key my-mc/mark-prefix-map (kbd "t") 'mc/mark-sgml-tag-pair)

(define-key my-mc/mark-prefix-map (kbd "n n") 'mc/mark-next-like-this)
(define-key my-mc/mark-prefix-map (kbd "n w") 'mc/mark-next-word-like-this)
(define-key my-mc/mark-prefix-map (kbd "n s") 'mc/mark-next-symbol-like-this)
(define-key my-mc/mark-prefix-map (kbd "p p") 'mc/mark-previous-like-this)
(define-key my-mc/mark-prefix-map (kbd "p w") 'mc/mark-previous-word-like-this)
(define-key my-mc/mark-prefix-map (kbd "p s") 'mc/mark-previous-symbol-like-this)

(if (featurep 'visual-regexp)
    (define-key my-mc/mark-prefix-map (kbd "v") 'vr/mc-mark))
;; TODO: `vr/select-mc-mark', `vr/select-replace', `vr/select-query-replace' etc.

;; First mark the word, then add more cursors.

;; To get out of multiple-cursors-mode, press <return> or C-g. The latter will
;; first disable multiple regions before disabling multiple cursors. If you want
;; to insert a newline in multiple-cursors-mode, use [C-j].

;; (setq mc/mode-line '("mc:"
;;                      (:eval
;;                       (format
;;                        #("%d" 0 2
;;                          (face font-lock-warning-face))
;;                        (mc/num-cursors)))))

(set-face-attribute 'mc/cursor-face nil
                    :inverse-video nil
                    :foreground nil
                    :background "dark red")
(set-face-attribute 'mc/region-face nil
                    :inverse-video nil
                    :foreground nil
                    :background (color-darken-name (face-background 'default) 4))

;;;_ Imenu

;;; Usage:
;; - [M-x imenu-?] :: invoke imenu functions.

(require 'imenu)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a SYMBOL-LIST in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;; (global-set-key (kbd "M-i") 'ido-goto-symbol)

;;;_ whitespace-mode

;; (require 'whitespace)

;; ;; automatically clean up bad whitespace
;; (setq whitespace-action '(auto-cleanup))
;; ;; only show bad whitespace
;; (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

;; (whitespace-mode 1)
;; (global-whitespace-mode)

;;;_ Ace Jump mode

(require 'ace-jump-mode)

;; (global-set-key [remap flyspell-auto-correct-previous-word] nil)
;; FIXME: this does not work, conflict with `flyspell-auto-correct-previous-word'.
(global-set-key (kbd "C-;") 'ace-jump-mode)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)
;; original [M-/]


;;;_ predictive-mode -- tries to predict the rest of the word, and offers you an appropriate completion.

;; (require 'predictive)

;; ;;; ### Predictive ###
;; ;;; --- 英语助手
;; (set-default 'predictive-auto-add-to-dict t) ;自动加入词典
;; (setq predictive-add-to-dict-ask nil)        ;加入词典不询问
;; (setq predictive-auto-learn t)               ;自动学习
;; (setq predictive-completion-speed 0.1)       ;查找补全的速度(秒)
;; (setq completion-auto-show-delay 0.5)        ;弹出补全tooltip的延迟(秒)
;; (dolist (hook (list
;;                'erc-mode-hook
;;                'rcirc-mode-hook
;;                'message-mode-hook
;;                'yaoddmuse-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (predictive-mode 1))))

;; (lazy-unset-key
;;  '("TAB")
;;  completion-dynamic-map)                ;卸载按键
;; (lazy-set-key
;;  '(
;;    ("M-h" . completion-accept)          ;接受辅助补全
;;    ("M-H" . completion-reject)          ;拒绝辅助补全
;;    )
;;  completion-map
;;  )

;;;_ Align

;;; Usage:
;; - commands prefix with `align-', `sort-',
;; - custom variable `align-rules-list'.
;; - `sort-fields', `sort-regexp-fields', `sort-numeric-fields', `sort-columns', `reverse-region',
;;
;; - region select text + [C-u M-x align-regexp] (could contains group in regexp pattern)

(setq align-highlight-change-face 'highlight)

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression.

For example: input regexp like [[:space:]]+ for align several space separated section/region."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 1 t)
  ;; The final `t' (aka true) is responsible for repeating the task.
  ;; Call that command with the regular expression `[[:space:]]+'
  )

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(text-column-whitespace
                           (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                           (group   . 2)
                           (modes   . align-text-modes)
                           (repeat  . t)))))

;;;_ Table Editing

;;; http://ergoemacs.org/emacs/emacs_table.html

;;; shows you how to use emacs's “table” feature. This feature will let you
;;; format tabular data by ASCII drawing. Then you can interactively create and
;;; edit tables with emacs commands to insert/delete column/row. You can also
;;; convert it to HTML or LaTeX formats.

;;;_* Usage:
;;
;; - (info "(emacs) Text Based Tables")
;; - [M-x table-] :: commands prefix with `table-'.


;;;_
(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
