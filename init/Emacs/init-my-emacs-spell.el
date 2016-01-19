;;; init-my-emacs-spell.el --- settings
;;; Commentary:

;;; Code:


;;; [ aspell & ispell ]

;;; Usage:
;;
;; - [M-$]
;; - [M-x ispell-complete-word]

(autoload 'ispell "ispell" t)

(setq ispell-look-command "/usr/sbin/look")

;; find aspell automatically
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 (t
  (setq ispell-program-name nil))
 )

(setq ispell-dictionary "english"
      ;; ispell-local-dictionary
      ispell-personal-dictionary nil ; If nil, the default (~/.ispell_LANGUAGE) will be used
      ispell-complete-word-dict "/usr/share/dict/words"
      ;; ispell-alternate-dictionary "/usr/share/dict/words"
      ispell-silently-savep t ; save silently. stop confirm when saving personal dictionary.
      ispell-parser 'use-mode-name
      )


;; Whenever I make a typo:
;;
;; 1. Hit =[C-x C-i]=, instead of erasing the mistake;
;; 2. Select the appropriate correction (thanks to *Ispell*);
;; 3. Sleep easier at night knowing I'll never see that mistake again (thanks to
;; *abbrev*).
;;
;; The command now searches backward for the closest wrong word. So you can just
;; hit [C-x C-i] even if the mistake happened several words ago.

(define-key ctl-x-map "\C-i"
  #'my/ispell-word-then-abbrev)

(defun my/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (and (setq bef (thing-at-point 'word))
                  (not (ispell-word nil 'quiet))
                  (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


;;; [ Flyspell ]

;;; Usage:
;;
;; - [M-$] -- correct word
;;   - press [i] and answer "yes" to add word into personal dictionary.
;; - [C-.] / [C-;] -- automatically correct last misspelled word, cycle through suggestions.
;; - [M-x flyspell-region] -- checks all words inside a region
;; - [M-x flyspell-buffer] -- checks the whole buffer

(use-package flyspell
  :config
  (setq flyspell-default-dictionary "en"
        flyspell-delay 5
        ;; flyspell-before-incorrect-word-string
        ;; flyspell-after-incorrect-word-string
        flyspell-use-meta-tab nil ; use [M-TAB] to correct word.
        flyspell-highlight-flag t
        flyspell-consider-dash-as-word-delimiter-flag t
        flyspell-persistent-highlight t
        ;; flyspell-mode-line-string " FlySpell"
        )

  ;; performance
  ;; I highly suggest setting ‘flyspell-issue-message-flag’ to nil, as printing
  ;; messages for every word (when checking the entire buffer) causes an enormous
  ;; slowdown.
  (setq flyspell-issue-message-flag nil)


  ;; (define-key flyspell-mode-map (kbd "C-.") 'flyspell-correct-word-before-point)
  (define-key flyspell-mode-map (kbd "C-.") 'flyspell-auto-correct-previous-word)
  (define-key flyspell-mode-map (kbd "C-,") 'flyspell-goto-next-error)
  ;; (add-hook 'flyspell-mode-hook
  ;;           (unbind-key "C-;" flyspell-mode-map)) ; conflict with iedit-mode toggle keybinding.
  (define-key flyspell-mode-map (kbd "C-M-i") nil) ; fix Org-mode abbreviations expand keybinding [M-Tab].

  (unless (boundp 'my-spell-prefix)
    (define-prefix-command 'my-spell-prefix))
  (define-key my-edit-prefix (kbd "s") 'my-spell-prefix)

  (define-key my-spell-prefix (kbd "C-s") 'flyspell-mode)
  (define-key my-spell-prefix (kbd "m") 'flyspell-mode)
  (define-key my-spell-prefix (kbd "p") 'flyspell-prog-mode)
  (define-key my-spell-prefix (kbd "b") 'flyspell-buffer)
  (define-key my-spell-prefix (kbd "r") 'flyspell-region)
  (define-key my-spell-prefix (kbd "c") 'ispell-word) ; default keybinding [M-$].

  
  (set-face-attribute 'flyspell-incorrect nil
                      :background "#444444" :foreground "red"
                      :underline '(:color "dark red" :style wave))
  (set-face-attribute 'flyspell-duplicate nil
                      :background "#555555" :foreground "orange"
                      :underline '(:color "dark red" :style line))

  
  ;; programming code
  ;; flyspell-prog-mode : enable flyspell for comments in source code
  (dolist (hook
           '(prog-mode-hook
             ))
    (add-hook hook 'flyspell-prog-mode))

  ;; TeX
  (add-hook 'tex-mode-hook
            (lambda ()
              (setq ispell-parser 'tex)))

  ;; text
  (dolist (hook
           '(text-mode-hook
             markdown-mode-hook
             ;; org-mode-hook
             ))
    (add-hook hook 'flyspell-mode))

  ;; (flyspell-mode 1)
  )


;;; [ flyspell-lazy ]

(use-package flyspell-lazy
  ;; :ensure t
  :config
  ;; (flyspell-mode 1)
  ;; (flyspell-lazy-mode 1)
  )


;;; [ flyspell-popup ] -- Correct the misspelled word with flyspell in popup menu.

(use-package flyspell-popup
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
  )


;;; [ flyguess ] -- guess language/dictionary for a buffer

(use-package flyguess
  ;; :ensure t
  ;; :config
  ;; (setq flyguess-dictionary-list '("english" "american" "francais"))
  )


;;; [ flyspell-guess ] -- flyspell dictionary guesser

(use-package flyspell-guess
  ;; :config
  ;; to load flyspell-guess every time you start Emacs. to activate the guess indicator (in minor-mode-list: "en").
  ;; (eval-after-load 'flyspell-guess
  ;;   (lambda ()
  ;;     '(flyspell-insinuate-guess-indicator)))
  )


;;; [ auto-dictionary ] -- tries to guess the buffer's text language and adjusts flyspell automatically.

(use-package auto-dictionary
  ;; :ensure t
  ;; :config
  ;; (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))
  )


(provide 'init-my-emacs-spell)

;;; init-my-emacs-spell.el ends here
