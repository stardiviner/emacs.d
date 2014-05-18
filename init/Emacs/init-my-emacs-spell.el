;;; init-my-emacs-spell.el --- settings
;;; Commentary:

;;; Code:


;;; aspell & ispell

;; - [C-;] ::

(require 'ispell)
(autoload 'ispell "ispell" t)

(setq ispell-dictionary "english")
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra")
      ispell-personal-dictionary "~/.emacs.d/data/spell/personal_dictionary/" ; personal spell dictionary.
      ispell-silently-savep t           ; save silently. stop confirm when saving personal dictionary.
      )

;;; [M-x ispell-complete-word]
;; (setq ispell-alternate-dictionary "/usr/share/dict/words")


;;; [ Flyspell ]

;;; Usage:
;;
;; - [M-$] / [M-TAB] -- correct word
;;   - press [i] and answer "yes" to add word into personal dictionary.
;; - [C-.] -- automatically correct word
;; - [C-;] -- automatically correct last misspelled word
;; - [M-x flyspell-region] -- checks all words inside a region
;; - [M-x flyspell-buffer] -- checks the whole buffer

(require 'flyspell)
;; (when (executable-find ispell-program-name)
;;   (require 'flyspell))
(autoload 'flyspell-mode "flyspell" "on-the-fly spelling checks" t)

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
         '(org-mode-hook
           text-mode-hook
           markdown-mode-hook))
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
      flyspell-mode-line-string " FlySpell"
      )

;; (define-key flyspell-mode-map (kbd "C-.") 'flyspell-correct-word-before-point)
(define-key flyspell-mode-map (kbd "C-.") 'flyspell-auto-correct-previous-word)


;;; [ flyspell-guess ] -- flyspell dictionary guesser

(require 'flyspell-guess)

;; to load flyspell-guess every time you start Emacs. to activate the guess indicator (in minor-mode-list: "en").
(eval-after-load 'flyspell-guess
  (lambda ()
    '(flyspell-insinuate-guess-indicator)))



;;; [ auto-dictionary ] -- tries to guess the buffer's text language and adjusts flyspell automatically.

;; (require 'auto-dictionary)
;;
;; (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))



(provide 'init-my-emacs-spell)

;;; init-my-emacs-spell.el ends here
