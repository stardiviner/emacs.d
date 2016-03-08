;;; init-my-emacs-regexp.el --- init my Emacs regexp expression


;;; Commentary:

;;; [ regular expression ]


;;; Code:


(unless (boundp 'my-regexp-prefix)
  (define-prefix-command 'my-regexp-prefix))
(global-set-key (kbd "C-c r") 'my-regexp-prefix)


;;; [ re-builder ] -- instant regexp builder

;;; Usage:
;;
;; - [M-x regexp-builder] -- start regexp-builder
;; - [C-c C-q] -- quit regexp-builder window.
;; - `reb-change-syntax' :: change default regexp syntax.
;; - `reb-mode'

(define-key my-regexp-prefix (kbd "b") 're-builder)


;;; [ re-builder+ ]

;; (unless (package-installed-p 're-builder+)
;;   (package-install 're-builder+))
;; (require 're-builder+)
;; TODO what's the difference for 'read, 'string and 'rx.
;; (setq reb-re-syntax 'read) ; 'read, 'string, 'rx


;;; [ pcre2el ] -- convert between PCRE, Emacs and rx regexp syntax.

;; Overview
;;
;; pcre2el or rxt (RegeXp Translator or RegeXp Tools) is a utility for working
;; with regular expressions in Emacs, based on a recursive-descent parser for
;; regexp syntax. In addition to converting (a subset of) PCRE syntax into its
;; Emacs equivalent, it can do the following:
;;
;; - convert Emacs syntax to PCRE.
;;
;; - convert either syntax to rx, an S-expression based regexp syntax.
;;
;; - untangle complex regexps by showing the parse tree in rx form and
;;   highlighting the corresponding chunks of code.
;;
;; - show the complete list of strings (productions) matching a regexp, provided
;;   the list is finite.
;;
;; - provide live font-locking of regexp syntax (so far only for Elisp buffers –
;;   other modes on the todo list).

;; Usage
;;
;; Enable `rxt-mode' or its global equivalent `rxt-global-mode' to get the
;; default key-bindings. There are three sets of commands: commands that take a
;; PCRE regexp, commands which take an Emacs regexp, and commands that try to do
;; the right thing based on the current mode. Currently, this means Emacs syntax
;; in `emacs-lisp-mode' and `lisp-interaction-mode', and PCRE syntax everywhere
;; else.
;;
;; The default key bindings all begin with [C-c /] and have a mnemonic
;; structure: [C-c / <source> <target>], or just [C-c / <target>] for the “do
;; what I mean” commands. The complete list of key bindings is given here and
;; explained in more detail below:

(use-package pcre2el
  :ensure t
  :config
  (define-key my-regexp-prefix (kbd "t") 'rxt-mode)
  (define-key my-regexp-prefix (kbd "T") 'rxt-global-mode)
  )


;;; [ rx ] -- A regular expression IDE for Emacs, to help with the creation and testing of regular expressions.


;;; [ regex-tool ]

(use-package regex-tool
  :ensure t)


;;; [ visual-regexp ] --

;;; visual-regexp for Emacs is like `replace-regexp' (or
;;; `query-replace-regexp'), but with live visual feedback directly in the
;;; buffer.

(use-package visual-regexp
  :ensure t
  :config
  ;; 'emacs engine is the native built-in Emacs engine. it is case-sensitive.
  (setq vr/engine 'emacs) ; 'python, 'emacs, 'custom, 'vr/command-python, 'vr/command-custom,

  (setq vr/match-separator-use-custom-face t
        vr/match-separator-string " ⇨ ")

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
  ;; TODO: `vr/select-mc-mark', `vr/select-replace' etc.

  
  ;; TODO:
  ;; (set-face-attribute 'vr/match-separator-face nil
  ;;                     :foreground "red"
  ;;                     :inverse-video nil
  ;;                     :weight 'bold)
  ;; (set-face-attribute 'vr/match-0 nil
  ;;                     :background (color-darken-name (face-background 'default) 3)
  ;;                     :box '(:color "red" :line-width -1)
  ;;                     :inverse-video nil :weight 'normal
  ;;                     )
  ;; (set-face-attribute 'vr/match-1 nil
  ;;                     :background (color-darken-name (face-background 'default) 3)
  ;;                     :box '(:color "dark red" :line-width -1)
  ;;                     :inverse-video nil :weight 'normal
  ;;                     )
  ;; (set-face-attribute 'vr/group-0 nil
  ;;                     :background (color-darken-name (face-background 'default) 3)
  ;;                     :inverse-video nil :weight 'normal
  ;;                     )
  ;; (set-face-attribute 'vr/group-1 nil
  ;;                     :background (color-darken-name (face-background 'default) 3)
  ;;                     :inverse-video nil :weight 'normal
  ;;                     )
  ;; (set-face-attribute 'vr/group-2 nil
  ;;                     :background (color-darken-name (face-background 'default) 3)
  ;;                     :inverse-video nil :weight 'normal
  ;;                     )
  )

(use-package visual-regexp-steroids
  :ensure t)


;;; [ ample-regexp ] -- Compose and reuse Emacs regular expressions with ease.

;;; Usage:
;;
;;; <basic usage>
;;
;; - The main item of the API is the `define-arx' macro. Let's start with a simple example:
;;
;;   (define-arx hello-world-rx '()) ;; -> hello-world-rx
;;   (hello-world-rx "Hello, world") ;; -> "Hello, world"
;;   (hello-world-rx (* "Hello, world")) ;; -> "\\(?:Hello, world\\)*"
;;
;; `define-arx' defines a macro that converts s-exps into regular
;; expressions. If you're familiar with `rx' package — if not, I encourage you
;; to do so — you're probably starting to experience déjà vu. You're right: `rx'
;; is used underneath, ample-regexps is just a cherry on the pie adding
;; customization with a hint of syntactic sugar atop.

(use-package ample-regexps
  :ensure t
  )


;;; [ Swiper ] -- gives you an overview as you search for a regex.

(use-package swiper
  :ensure t
  :config  
  (define-key my-regexp-prefix (kbd "s") 'swiper)
  )


;;; Making Elisp regex look nicer
;;
;; This is just a small improvement to make e.g. \\( show up in regular
;; expressions without the escape chars, but instead fontified with
;; font-lock-keyword-face. It doesn't affect the underlying code at all, just
;; makes it look nicer. For the \\| I chose ∨ - the logical or character.

(defun fontify-glyph (item glyph)
  `((,item
     (0 font-lock-keyword-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph) nil)))))

(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\|" "∨"))
(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\(" "("))
(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\)" ")"))


;;; At first, I wanted to just inline a picture, but then I thought that
;;; htmlize-buffer would be able to handle it. It didn't, so I just edited a
;;; small snippet by hand:
;;
;; (or (string-match "^([^\n%|]*?)|(([^\n]*)?$" str)
;;    (string-match "^([^\n%|]*?)(%[^\n]*)?$" str))

;;; It's really satisfying to see those escape chars vanish as I type in a
;;; capture group in the regex, especially with the help of lispy-mode. Here are
;;; some relevant tests for the regex support:

(if (featurep 'lispy-mode) ; (functionp 'lispy-with)
    '(progn
       (should (string= (lispy-with "\"a regex \\\\|\"" "(")
                        "\"a regex \\\\(|\\\\)\""))
       (should (string= (lispy-with "\"\\\\(|foo\\\\)\"" "\C-?")
                        "\"|foo\""))
       (should (string= (lispy-with "\"\\\\(foo\\\\)|\"" "\C-?")
                        "\"foo|\""))
       (should (string= (lispy-with "\"|\\\\(foo\\\\)\"" "\C-d")
                        "\"|foo\""))
       (should (string= (lispy-with "\"\\\\(foo|\\\\)\"" "\C-d")
                        "\"foo|\""))
       ))


(provide 'init-my-emacs-regexp)

;;; init-my-emacs-regexp.el ends here
