;;; init-my-emacs-spell.el --- settings
;;; Commentary:

;;; Code:


;;; [ aspell & ispell ]

;;; Usage:
;;
;; - [M-$]
;; - [M-x ispell-complete-word]

(autoload 'ispell "ispell" t)

;; find aspell and hunspell automatically
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_US"))
  (setq ispell-really-hunspell t)
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
  )
 (t
  (setq ispell-program-name nil))
 )

(setq ispell-dictionary "english"
      ;; ispell-local-dictionary
      ispell-personal-dictionary nil ; If nil, the default (~/.ispell_LANGUAGE) will be used
      ;; ispell-complete-word-dict "/usr/share/dict/words"
      ;; ispell-alternate-dictionary "/usr/share/dict/words"
      ispell-silently-savep t ; save silently. stop confirm when saving personal dictionary.
      ispell-parser 'use-mode-name
      )


;;; [ Flyspell ]

;;; Usage:
;;
;; - [M-$] -- correct word
;;   - press [i] and answer "yes" to add word into personal dictionary.
;; - [C-.] / [C-;] -- automatically correct last misspelled word, cycle through suggestions.
;; - [M-x flyspell-region] -- checks all words inside a region
;; - [M-x flyspell-buffer] -- checks the whole buffer

(setq flyspell-default-dictionary "en")

;; (flyspell-mode 1)

;;; programming code
;; flyspell-prog-mode : enable flyspell for comments in source code
(dolist (hook
         '(prog-mode-hook
           ))
  (add-hook hook 'flyspell-prog-mode))

;;; TeX
(add-hook 'tex-mode-hook
          (lambda ()
            (setq ispell-parser 'tex)))

;;; text
(dolist (hook
         '(text-mode-hook
           markdown-mode-hook
           ;; org-mode-hook
           ))
  (add-hook hook 'flyspell-mode))

;;; performance
;; I highly suggest setting ‘flyspell-issue-message-flag’ to nil, as printing
;; messages for every word (when checking the entire buffer) causes an enormous
;; slowdown.
(setq flyspell-issue-message-flag nil)

(setq flyspell-delay 10
      flyspell-use-meta-tab nil ; use [M-TAB] to correct word.
      flyspell-highlight-flag t
      flyspell-consider-dash-as-word-delimiter-flag t
      flyspell-persistent-highlight t
      ;; flyspell-mode-line-string " FlySpell"
      )

;; (define-key flyspell-mode-map (kbd "C-.") 'flyspell-correct-word-before-point)
(define-key flyspell-mode-map (kbd "C-.") 'flyspell-auto-correct-previous-word)
(define-key flyspell-mode-map (kbd "C-,") 'flyspell-goto-next-error)
;; (add-hook 'flyspell-mode-hook
;;           (unbind-key "C-;" flyspell-mode-map)) ; conflict with iedit-mode toggle keybinding.
(define-key flyspell-mode-map (kbd "C-M-i") nil) ; fix Org-mode abbreviations expand keybinding [M-Tab].

(set-face-attribute 'flyspell-incorrect nil
                    :background "#444444" :foreground "red"
                    :underline '(:color "dark red" :style wave))
(set-face-attribute 'flyspell-duplicate nil
                    :background "#555555" :foreground "orange"
                    :underline '(:color "dark red" :style line))

(unless (boundp 'my-spell-prefix-map)
  (define-prefix-command 'my-spell-prefix-map))
(define-key my-edit-prefix-map (kbd "s") 'my-spell-prefix-map)

(define-key my-spell-prefix-map (kbd "C-s") 'flyspell-mode)
(define-key my-spell-prefix-map (kbd "m") 'flyspell-mode)
(define-key my-spell-prefix-map (kbd "p") 'flyspell-prog-mode)
(define-key my-spell-prefix-map (kbd "b") 'flyspell-buffer)
(define-key my-spell-prefix-map (kbd "r") 'flyspell-region)
(define-key my-spell-prefix-map (kbd "c") 'ispell-word) ; default keybinding [M-$].


;;; [ helm-flyspell ]

;;; Usage:
;;
;; `helm-flyspell-correct'


;;; [ flyspell-popup ] -- Correct the misspelled word with flyspell in popup menu.

;;; Usage:
;;
;; `flyspell-popup-correct'

(if (featurep 'helm-flyspell)
    (define-key flyspell-mode-map (kbd "C-;") #'helm-flyspell-correct)
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct))



;;; [ flyguess ] -- guess language/dictionary for a buffer

;; (setq flyguess-dictionary-list '("english" "american" "francais"))


;;; [ flyspell-guess ] -- flyspell dictionary guesser

;; to load flyspell-guess every time you start Emacs. to activate the guess indicator (in minor-mode-list: "en").
;; (eval-after-load 'flyspell-guess
;;   (lambda ()
;;     '(flyspell-insinuate-guess-indicator)))



;;; [ auto-dictionary ] -- tries to guess the buffer's text language and adjusts flyspell automatically.

;; (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))



(provide 'init-my-emacs-spell)

;;; init-my-emacs-spell.el ends here
