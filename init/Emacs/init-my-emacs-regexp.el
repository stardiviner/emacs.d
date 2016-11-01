;;; init-my-emacs-regexp.el --- init my Emacs regexp expression


;;; Commentary:

;;; [ regular expression ]


;;; Code:


(unless (boundp 'my-regexp-prefix)
  (define-prefix-command 'my-regexp-prefix))
(global-set-key (kbd "C-c r") 'my-regexp-prefix)


;;; [ re-builder ] -- instant regexp builder

(use-package re-builder
  :ensure t
  :config
  ;; (setq reb-re-syntax 'read) ; 'read, 'string, 'rx

  (define-key reb-mode-map (kbd "C-c C-q") 'reb-quit)

  (define-key my-regexp-prefix (kbd "b") 're-builder)
  )

;;; [ pcre2el ] -- convert between PCRE, Emacs and rx regexp syntax.

(use-package pcre2el
  :ensure t
  :defer t
  :init
  (define-key my-regexp-prefix (kbd "t") 'rxt-mode)
  (define-key my-regexp-prefix (kbd "T") 'rxt-global-mode)
  )


;;; [ rx ] -- A regular expression IDE for Emacs, to help with the creation and testing of regular expressions.


;;; [ regex-tool ]

(use-package regex-tool
  :ensure t
  :defer t)


;;; [ visual-regexp ] --

;;; visual-regexp for Emacs is like `replace-regexp' (or
;;; `query-replace-regexp'), but with live visual feedback directly in the
;;; buffer.

(use-package visual-regexp
  :ensure t
  :defer t
  :init
  (unless (boundp 'visual-regexp-map)
    (define-prefix-command 'visual-regexp-map))
  (define-key my-regexp-prefix (kbd "v") 'visual-regexp-map)

  (define-key visual-regexp-map (kbd "s") 'vr/isearch-forward)
  (define-key visual-regexp-map (kbd "b") 'vr/isearch-backward)
  (define-key visual-regexp-map (kbd "r") 'vr/replace)
  (define-key visual-regexp-map (kbd "q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (if (featurep 'multiple-cursors)
      (define-key visual-regexp-map (kbd "m") 'vr/mc-mark))
  ;; `vr/select-mc-mark', `vr/select-replace' etc.
  
  :config
  ;; 'emacs engine is the native built-in Emacs engine. it is case-sensitive.
  (setq vr/engine 'emacs) ; 'python, 'emacs, 'custom, 'vr/command-python, 'vr/command-custom,

  (setq vr/match-separator-use-custom-face t
        vr/match-separator-string " ⇨ ")
  )

(use-package visual-regexp-steroids
  :ensure t
  :defer t)


;;; [ ample-regexp ] -- Compose and reuse Emacs regular expressions with ease.

(use-package ample-regexps
  :ensure t
  :defer t)


;;; Making Elisp regex look nicer
;;
;; This is just a small improvement to make e.g. \\( show up in regular
;; expressions without the escape chars, but instead fontified with
;; font-lock-keyword-face. It doesn't affect the underlying code at all, just
;; makes it look nicer. For the \\| I chose ∨ - the logical or character.

;; (defun fontify-glyph (item glyph)
;;   `((,item
;;      (0 font-lock-keyword-face t)
;;      (0 (prog1
;;             (compose-region (match-beginning 0)
;;                             (match-end 0)
;;                             ,glyph) nil)))))
;;
;; (font-lock-add-keywords 'emacs-lisp-mode
;;                         (fontify-glyph "\\\\\\\\|" "∨"))
;; (font-lock-add-keywords 'emacs-lisp-mode
;;                         (fontify-glyph "\\\\\\\\(" "("))
;; (font-lock-add-keywords 'emacs-lisp-mode
;;                         (fontify-glyph "\\\\\\\\)" ")"))


;;; At first, I wanted to just inline a picture, but then I thought that
;;; htmlize-buffer would be able to handle it. It didn't, so I just edited a
;;; small snippet by hand:
;;
;; (or (string-match "^([^\n%|]*?)|(([^\n]*)?$" str)
;;    (string-match "^([^\n%|]*?)(%[^\n]*)?$" str))

;;; It's really satisfying to see those escape chars vanish as I type in a
;;; capture group in the regex, especially with the help of lispy-mode. Here are
;;; some relevant tests for the regex support:

;; (if (featurep 'lispy-mode) ; (functionp 'lispy-with)
;;     '(progn
;;        (should (string= (lispy-with "\"a regex \\\\|\"" "(")
;;                         "\"a regex \\\\(|\\\\)\""))
;;        (should (string= (lispy-with "\"\\\\(|foo\\\\)\"" "\C-?")
;;                         "\"|foo\""))
;;        (should (string= (lispy-with "\"\\\\(foo\\\\)|\"" "\C-?")
;;                         "\"foo|\""))
;;        (should (string= (lispy-with "\"|\\\\(foo\\\\)\"" "\C-d")
;;                         "\"|foo\""))
;;        (should (string= (lispy-with "\"\\\\(foo|\\\\)\"" "\C-d")
;;                         "\"foo|\""))
;;        ))


(provide 'init-my-emacs-regexp)

;;; init-my-emacs-regexp.el ends here
