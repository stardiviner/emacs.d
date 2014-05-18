;;; init-my-emacs-edit.el --- init Emacs editing
;;; Commentary:

;;; Code:

;;; [ undo-tree ]
;; [C-x u] -> undo-tree-visualizer-mode
;;    `- [C-p/n]  -- move up/down
;;    `- [C-b/f]  -- move left/right
;;    `- t    -- timestamp
;;    `- q    -- quit

(require 'undo-tree)

(global-undo-tree-mode t)
(diminish 'undo-tree-mode)


;;; [ Edit ]

(delete-selection-mode t)             ; typed text replaces the active selection


;;; [ kill-ring-search ] --

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


;;; [ kill-ring-ido ] --

;;; Usage:
;; - [C-M-y] ::

(require 'kill-ring-ido)

(global-set-key (kbd "C-M-y") 'kill-ring-ido)


;;; [ Electric ]

;; TODO: when electric auto insert ) for (, but when you delete (, electric will not auto delete ) for you.

;; NOTICE: this conflict with ParEdit and auto-pair.
;; (when (fboundp 'electric-pair-mode)
;;   (setq-default electric-pair-mode 1))
;; ------------------------------------------
;; (electric-pair-mode t) ; automatically insert delimiter pairs.

(show-paren-mode)      ; show matching parenthesis

;; TODO: (electric-indent-mode t)

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


;;; [ skeleton ]

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


;;; [ Narrowing ]
;;; Usage:
;; - prefix --> [C-x n]
;; - [C-x n n] -- narrow to region
;; - [C-x n w] -- widen (undo narrow)

;;; don't disable narrowing functions
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)



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

(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)


;;; [ Mark ] --- [C-SPC / C-@] + [C-u C-SPC / C-u C-@] + [C-`] / [M-`]

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


;;; [ Multiple Narrowings ]


;;; [ capitalized-words-mode ]
;; (capitalized-words-mode 1)


;;; [ auto-capitalize ]

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
;;       ;; TODO auto-capitalize-predicate
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


;;; [ Iedit ] -- Edit multiple regions simultaneously in a buffer or a region
;;;
;;; This package includes Emacs minor modes (iedit-mode and
;;; iedit-rectangle-mode) based on a API library (iedit-lib) and allows you to
;;; edit one occurrence of some text in a buffer (possibly narrowed) or region,
;;; and simultaneously have other occurrences edited in the same way, with
;;; visual feedback as you type.
;;
;; Normal scenario of Iedit mode is like:
;;
;; - Highlight certain contents - by press C-; All occurrences of a symbol, string or a rectangle in the buffer or a region may be highlighted corresponding to current mark, point and prefix argument. Refer to the document of `iedit-mode’ for details.
;;
;; - Edit one of the occurrences The change is applied to other occurrences simultaneously.
;; - Finish - by pressing C-; again
;
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
;; - Restricting the search area to the lines near the current line can be done by pressing M-{ and M-}. These will expand the search region one line at a time from the top and bottom. Add a prefix argument to go the opposite direction.

;;; Iedit-rectangle-mode provides rectangle support with visible rectangle
;;; highlighting, which is similar with cua mode rectangle support. But it’s
;;; lighter weight and uses iedit mechanisms.

;;; There are also some other facilities you may never think about. Refer to the
;;; document of function `iedit-mode’ (C-h f iedit-mode RET) for more details.

;;; Usage:
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

;; (require 'iedit)

;; (autoload 'iedit-mode "Iedit" "Edit multiple regions with the same content simultaneously." t)
;; (autoload 'iedit-rectangle-mode "Iedit rectangle" "Edit narrowed text." t)

;; ;; (setq iedit-occurrence-face 'isearch)

;; (defun iedit-dwim (arg)
;;   "If ARG, start iedit but use \\[narrow-to-defun] to limit its scope."
;;   (interactive "P")
;;   (if arg
;;       (iedit-mode)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         ;; this function determines the scope of `iedit-start'.
;;         (if iedit-mode
;;             (iedit-done)
;;           ;; `current-word' can of course be replaced by other
;;           ;; functions.
;;           (narrow-to-defun)
;;           (iedit-start (current-word) (point-min) (point-max)))))))



;;; [ multiple-cursors ]

;;; Usage:
;;; https://github.com/magnars/multiple-cursors.el
;;; - [C-c c] -- prefix of mc.
;;; - [C-c c c] / [C-S-c C-S-c] -- edit-lines

(require 'multiple-cursors)

;; TODO: (setq mc/keymap "C-c c")

;; multiple-cursors
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; ---------------------------------------------------------
;; From active region to multiple cursors:
(global-set-key (kbd "C-c c c") 'mc/mark-all-dwim) ; try to be smart.
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c l") 'mc/edit-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(if (featurep 'visual-regexp)
    (global-set-key (kbd "C-c c m") 'vr/mc-mark))
;; TODO: `vr/select-mc-mark', `vr/select-replace', `vr/select-query-replace' etc.

;; First mark the word, then add more cursors.

;; To get out of multiple-cursors-mode, press <return> or C-g. The latter will
;; first disable multiple regions before disabling multiple cursors. If you want
;; to insert a newline in multiple-cursors-mode, use [C-j].

;; (set-face-attribute 'mc/cursor nil
;;                     :foreground "cyan")


;;; [ Imenu ]

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

(global-set-key (kbd "M-i") 'ido-goto-symbol)


;; Ace Jump mode

(require 'ace-jump-mode)

;; (global-set-key [remap flyspell-auto-correct-previous-word] nil)
;; FIXME: this does not work, conflict with `flyspell-auto-correct-previous-word'.
(global-set-key (kbd "C-;") 'ace-jump-mode)


;;----------------------------------------------------------------------------
;; TODO: Expand region
;;----------------------------------------------------------------------------
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)



;;; [ predictive-mode ] -- tries to predict the rest of the word, and offers you an appropriate completion.

(require 'predictive)

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


(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
