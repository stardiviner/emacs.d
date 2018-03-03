;;; init-languages.el --- init for Languages

;;; Commentary:



;;; Code:


(unless (boundp 'search-prefix)
  (define-prefix-command 'search-prefix))
(unless (boundp 'language-search-prefix)
  (define-prefix-command 'language-search-prefix))
(define-key search-prefix (kbd "l") 'language-search-prefix)

;;; [ guess-language ] -- Robust automatic language detection.

;; (use-package guess-language
;;   :ensure t
;;   :init
;;   (add-hook 'text-mode-hook #'guess-language-mode)
;;   :config
;;   (setq guess-language-languages '(en))
;;   ;; (setq guess-language-langcodes '((en . ("en_GB" "English"))
;;   ;;                                  (it . ("it_IT" "Italian")))
;;   ;;       guess-language-languages '(en it)
;;   ;;       guess-language-min-paragraph-length 45)
;;   )


(provide 'init-languages)

;;; init-languages.el ends here
