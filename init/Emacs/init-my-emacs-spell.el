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
      )

;;; [M-x ispell-complete-word]
;; (setq ispell-alternate-dictionary "/usr/share/dict/words")


;;; [ Flyspell ]

;;; Usage:
;;
;; - [M-$] / [M-TAB] -- correct word
;; - [C-.] -- automatically correct word
;; - [C-;] -- automatically correct last misspelled word
;; - [M-x flyspell-region] -- checks all words inside a region
;; - [M-x flyspell-buffer] -- checks the whole buffer

(require 'flyspell)
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
      ;; flyspell-mode-map
      flyspell-use-meta-tab t ; use [M-TAB] to correct word.
      flyspell-highlight-flag t
      flyspell-consider-dash-as-word-delimiter-flag t
      flyspell-persistent-highlight t
      flyspell-mode-line-string " FlySpell"
      )



(provide 'init-my-emacs-spell)

;;; init-my-emacs-spell.el ends here
