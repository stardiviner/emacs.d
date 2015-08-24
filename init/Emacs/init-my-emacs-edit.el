;;; init-my-emacs-edit.el --- init Emacs editing
;;; Commentary:

;;; Code:

;;;_ undo-tree
;; [C-x u] -> undo-tree-visualizer-mode
;;    `- [C-p/n]  -- move up/down
;;    `- [C-b/f]  -- move left/right
;;    `- t    -- timestamp
;;    `- q    -- quit

(global-undo-tree-mode t)

;;;_ Edit

(delete-selection-mode t)             ; typed text replaces the active selection

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

;;;_ expand-region

;;; Expand region increases the selected region by semantic units. Just keep
;;; pressing the key until it selects what you want.

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



;;;_ scratch.el -- launch a scratch buffer for the current mode.

(autoload 'scratch "scratch" nil t)

(define-key my-prog-inferior-map (kbd "C-c") 'scratch)

;;;_ Imenu

;;; Usage:
;; - [M-x imenu-?] :: invoke imenu functions.

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



(require 'init-my-emacs-edit-narrow)
(require 'init-my-emacs-edit-tabulate)
(require 'init-my-emacs-edit-multiple-cursors)



;;;_
(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
