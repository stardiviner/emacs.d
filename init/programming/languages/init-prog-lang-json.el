;;; init-prog-lang-json.el --- init for JSON
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ json-mode ] -- Extends the builtin js-mode to add better syntax highlighting for JSON.

(use-package json-mode
  :ensure t
  :defer t
  :commands (json-mode-show-path
             json-mode-beautify jsons-print-path jsons-print-path-jq)
  :config
  (setq json-reformat:indent-width 2
        json-reformat:pretty-string? t ; decode some special characters. like \u00e4.
        ;; json-reformat:special-chars-as-pretty-string '((?\" . ?\")
        ;;                                                (?\\ . ?\\))
        ))

;;; [ jq-format ] -- Reformat JSON and JSONLines using jq.

(use-package jq-format
  :ensure t
  :defer t
  :after json-mode
  :commands (jq-format-json-on-save-mode
             jq-format-json-buffer jq-format-json-region
             jq-format-jsonlines-on-save-mode
             jq-format-jsonlines-buffer jq-format-jsonlines-region)
  :init (setq jq-format-command "jq"))

;;; [ json-snatcher ]

(use-package json-snatcher
  :ensure t
  :defer t)

;;; [ json-navigator ] -- view and navigate JSON structures.

(use-package json-navigator
  :ensure t
  :defer t)



(provide 'init-prog-lang-json)

;;; init-prog-lang-json.el ends here
