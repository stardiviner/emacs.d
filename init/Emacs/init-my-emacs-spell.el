;;; init-my-emacs-spell.el --- init for spell settings.
;;; Commentary:

;;; Code:

;; bind to [M-g] keybindings.
(unless (boundp 'spell-prefix)
  (define-prefix-command 'spell-prefix))
(global-set-key (kbd "M-g s") 'spell-prefix)


;;; [ aspell & ispell ]

;;; Usage:
;;
;; - [M-$] / `ispell-word'
;; - [M-x ispell-complete-word]

(require 'ispell)

(setq ispell-look-command "/usr/bin/look")

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
      ispell-silently-savep t ; `ispell-pdict-save' save silently. stop confirm when saving personal dictionary.
      ispell-parser 'use-mode-name
      )

;;; skip regions in Org-mode for ispell.
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))

(define-key spell-prefix (kbd "s") 'ispell-word) ; [M-$]
(define-key spell-prefix (kbd "<tab>") 'ispell-complete-word)

;;; [ Flyspell ] -- [M-$], [C-;]

(use-package flyspell
  :ensure-system-package (ispell aspell)
  :preface
  ;; don't use [M-TAB] keybinding to correct word.
  (setq flyspell-use-meta-tab nil)
  :bind (:map flyspell-mode-map
              ("C-." . flyspell-correct-word-before-point)
              ("C-," . flyspell-goto-next-error))
  :init
  ;; global
  ;; (flyspell-mode 1)

  ;; programming code
  ;; flyspell-prog-mode : enable flyspell for comments in source code
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  ;; Org-mode
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             ;; ignore TeX commands
  ;;             (setq-local ispell-parser 'tex)
  ;;             (flyspell-mode 1)
  ;;             ))

  ;; TeX
  (add-hook 'tex-mode-hook
            (lambda ()
              (setq ispell-parser 'tex)
              (flyspell-mode 1)
              ))

  ;; text
  (dolist (hook
           '(text-mode-hook
             markdown-mode-hook
             ))
    (add-hook hook 'flyspell-mode))

  :config
  (setq flyspell-default-dictionary "en"
        flyspell-default-delayed-commands '(self-insert-command)
        flyspell-delayed-commands '(self-insert-command)
        flyspell-default-deplacement-commands '(next-line previous-line scroll-up scroll-down)
        flyspell-deplacement-commands '(next-line previous-line scroll-up scroll-down)
        flyspell-consider-dash-as-word-delimiter-flag t
        flyspell-after-incorrect-word-string "✗"
        ;; save correct wrong words into global abbrev table.
        ;; flyspell-abbrev-p t
        ;; flyspell-use-global-abbrev-table-p t
        )

  ;; performance
  ;; I highly suggest setting ‘flyspell-issue-message-flag’ to nil, as printing
  ;; messages for every word (when checking the entire buffer) causes an enormous
  ;; slowdown.
  (setq flyspell-issue-message-flag nil)

  ;; bind to [M-g] keybindings.
  (define-key spell-prefix (kbd "n") 'flyspell-goto-next-error)
  (define-key spell-prefix (kbd "c") 'flyspell-correct-word-before-point)
  )


;;; [ flyspell-correct ] -- correcting words with flyspell via custom interface.

(use-package flyspell-correct
  :ensure t
  :config
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-.") 'flyspell-correct-word-generic)))


(provide 'init-my-emacs-spell)

;;; init-my-emacs-spell.el ends here
