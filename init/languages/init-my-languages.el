;;; init-my-languages.el --- init for Languages

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ guess-language ] -- Robust automatic language detection.

(use-package guess-language
  :ensure t
  :config
  (setq guess-language-languages '(en))
  (add-hook 'text-mode-hook #'guess-language-mode)
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-languages)

;;; init-my-languages.el ends here
