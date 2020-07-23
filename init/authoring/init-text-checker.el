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
  :custom ((ispell-dictionary "english")
           (ispell-silently-savep t)    ; `ispell-pdict-save' save silently.
           )
  :config
  ;; --reverse :: fix `aspell' conflict bug with `ispell'.
  (when (string-equal ispell-program-name "ispell")
    (setq ispell-extra-args '("--reverse" "--sug-mode=ultra" "--lang=en_US")))
  ;; skip regions in Org-mode for ispell.
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example")))

;;; [ Flyspell ] -- On-the-fly spell checker.

(use-package flyspell
  :ensure t
  :defer t
  :delight flyspell-mode
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
  :custom ((flyspell-default-dictionary "en")
           ;; (flyspell-after-incorrect-word-string "✗")
           (flyspell-consider-dash-as-word-delimiter-flag t)
           (flyspell-issue-message-flag nil))
  :hook ((prog-mode . flyspell-prog-mode)
         ;; (text-mode . flyspell-mode)
         ;; (org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode))
  :config (add-hook 'tex-mode-hook
                    (lambda () (setq ispell-parser 'tex) (flyspell-mode 1))))

;;; [ flyspell-correct ] -- correcting words with flyspell via custom interface.

;; (use-package flyspell-correct
;;   :ensure t
;;   :defer t
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;;; [ flycheck-grammarly ] -- Grammarly support for Flycheck.

;; (use-package flycheck-grammarly
;;   :ensure t
;;   :config
;;   (add-hook 'mu4e-compose-mode-hook
;;             (lambda ()
;;               (flycheck-mode 1)
;;               ;; NOTE this `flycheck-grammarly' causes suspend in `mu4e-compose-mode' buffer editing.
;;               (setq-local flycheck-checker 'grammarly-checker))))


(provide 'init-text-checker)

;;; init-text-checker.el ends here
