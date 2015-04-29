;;; init-my-emacs-spell.el --- settings
;;; Commentary:

;;; Code:


;;; aspell & ispell

;; - [C-;] ::

;; (require 'ispell)
(autoload 'ispell "ispell" t)

(setq ispell-dictionary "english"
      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra")
      ispell-personal-dictionary nil ; If nil, the default (~/.ispell_LANGUAGE) will be used
      ispell-silently-savep t ; save silently. stop confirm when saving personal dictionary.
      ispell-parser 'use-mode-name
      )

;; (setq ispell-local-dictionary-alist
;;       ispell-dictionary-alist)

;; FIXME: hunspell dictionary for english is not available.
;; On OS X/Darwin, make sure we add the path to the homebrew installs
;; brew install hunspell
;; (when (string-equal system-type "darwin")
;;   (setq exec-path (append exec-path '("/usr/local/bin"))))
;;
;; (when (executable-find "hunspell")
;;   (setq-default ispell-program-name "hunspell")
;;   (setq ispell-hunspell-dict-paths-alist '(("american" "/usr/share/myspell/dicts/en_US.aff")
;;                                            ("british" "/usr/share/myspell/dicts/en_GB.aff")
;;                                            ("english" "/usr/share/myspell/dicts/en_US.aff")
;;                                            ("en_NG" "/usr/share/myspell/dicts/en_NG.aff")
;;                                            ("en_PH" "/usr/share/myspell/dicts/en_PH.aff")
;;                                            ("en_AU" "/usr/share/myspell/dicts/en_AU.aff")
;;                                            ("en_HK" "/usr/share/myspell/dicts/en_HK.aff")
;;                                            ("en_CA" "/usr/share/myspell/dicts/en_CA.aff")
;;                                            ("en_NZ" "/usr/share/myspell/dicts/en_NZ.aff")
;;                                            ("en_GH" "/usr/share/myspell/dicts/en_GH.aff")
;;                                            ("en_ZW" "/usr/share/myspell/dicts/en_ZW.aff")
;;                                            ("en_DK" "/usr/share/myspell/dicts/en_DK.aff")
;;                                            ("en_SG" "/usr/share/myspell/dicts/en_SG.aff")
;;                                            ("en_TT" "/usr/share/myspell/dicts/en_TT.aff")
;;                                            ("en_IE" "/usr/share/myspell/dicts/en_IE.aff")
;;                                            ("en_BS" "/usr/share/myspell/dicts/en_BS.aff")
;;                                            ("en_BW" "/usr/share/myspell/dicts/en_BW.aff")
;;                                            ("en_BZ" "/usr/share/myspell/dicts/en_BZ.aff")
;;                                            ("en_GB" "/usr/share/myspell/dicts/en_GB.aff")
;;                                            ("en_JM" "/usr/share/myspell/dicts/en_JM.aff")
;;                                            ("en_AG" "/usr/share/myspell/dicts/en_AG.aff")
;;                                            ("en_ZA" "/usr/share/myspell/dicts/en_ZA.aff")
;;                                            ("en_US" "/usr/share/myspell/dicts/en_US.aff")
;;                                            ("en_IN" "/usr/share/myspell/dicts/en_IN.aff")
;;                                            ("en_NA" "/usr/share/myspell/dicts/en_NA.aff"))
;;         ;; ispell-hunspell-dictionary-alist
;;         ;; ispell-hunspell-dictionary-equivs-alist
;;         )
;;   (setq ispell-really-hunspell t))

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
(autoload 'flyspell-prog-mode "flyspell" "on-the-fly spelling checks for programming modes" t)

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


;;; [ flyguess ] -- guess language/dictionary for a buffer

;; (require 'flyguess)

;; (setq flyguess-dictionary-list '("english" "american" "francais"))


;;; [ flyspell-guess ] -- flyspell dictionary guesser

;; (require 'flyspell-guess)

;; to load flyspell-guess every time you start Emacs. to activate the guess indicator (in minor-mode-list: "en").
;; (eval-after-load 'flyspell-guess
;;   (lambda ()
;;     '(flyspell-insinuate-guess-indicator)))



;;; [ auto-dictionary ] -- tries to guess the buffer's text language and adjusts flyspell automatically.

;; (require 'auto-dictionary)
;;
;; (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))



(provide 'init-my-emacs-spell)

;;; init-my-emacs-spell.el ends here
