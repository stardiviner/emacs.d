;;; init-text-checker.el --- init for spell settings.
;;; Commentary:

;;; Code:

;; bind to [M-g] keybindings.
(unless (boundp 'text-checker-prefix)
  (define-prefix-command 'text-checker-prefix))
(global-set-key (kbd "M-g w") 'text-checker-prefix)

;;; [ aspell & ispell ]

;;; Usage:
;;
;; - [M-$] / `ispell-word'
;; - [M-x ispell-complete-word]

(use-package ispell
  :ensure t
  :defer t
  :bind (:map text-checker-prefix
              ("s" . ispell-word) ; [M-$]
              ("<tab>" . ispell-complete-word))
  :init
  (cond
   ;; use smarter "aspell" to replace default "ispell".
   ((executable-find "aspell")
    (setq-default ispell-program-name "aspell")
    ;; --reverse :: fix `aspell' conflict bug with `ispell'.
    (setq-default ispell-extra-args '("--reverse" "--sug-mode=ultra" "--lang=en_US")))
   ((executable-find "ispell")
    (setq-default ispell-program-name "ispell"))
   (t
    (setq ispell-program-name nil)))
  :config
  (setq ispell-dictionary "english"
        ;; ispell-local-dictionary
        ispell-personal-dictionary nil ; If nil, the default (~/.ispell_LANGUAGE) will be used
        ispell-complete-word-dict "/usr/share/dict/words"
        ;; ispell-alternate-dictionary "/usr/share/dict/words"
        ispell-silently-savep t ; `ispell-pdict-save' save silently.
        ispell-parser 'use-mode-name)
  ;;; skip regions in Org-mode for ispell.
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example")))

;;; [ Flyspell ] -- On-the-fly spell checker.

(use-package flyspell
  :ensure t
  :defer t
  :custom (flyspell-use-meta-tab nil)
  :bind (:map text-checker-prefix
              ("m" . flyspell-mode)
              :map flyspell-mode-map
              ("C-." . flyspell-correct-word-before-point)
              ("C-," . flyspell-goto-next-error)
              :map text-checker-prefix
              ("w" . flyspell-buffer)
              ("n" . flyspell-goto-next-error)
              ("c" . flyspell-correct-word-before-point))
  :init
  ;; global
  ;; (flyspell-mode 1)

  ;; programming code
  ;; flyspell-prog-mode : enable flyspell for comments in source code
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode) ; cause little performance issue on code completing.

  ;; Org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              ;; ignore TeX commands
              (setq-local ispell-parser 'tex)
              (flyspell-mode 1)))

  ;; TeX
  (add-hook 'tex-mode-hook
            (lambda ()
              (setq ispell-parser 'tex)
              (flyspell-mode 1)))

  ;; text
  (dolist (hook
           '(;; `text-mode' is parent mode of `org-mode' and `markdown-mode'.
             ;; text-mode-hook
             markdown-mode-hook))
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
  (setq flyspell-issue-message-flag nil))

;;; [ flyspell-correct ] -- correcting words with flyspell via custom interface.

(use-package flyspell-correct
  :ensure t
  :defer t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))



(provide 'init-text-checker)

;;; init-text-checker.el ends here
