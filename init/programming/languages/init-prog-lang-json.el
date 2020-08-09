;;; init-prog-lang-json.el --- init for JSON
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ json-mode ] -- Extends the builtin js-mode to add better syntax highlighting for JSON.

(use-package json-mode
  :ensure t
  :defer t
  :custom (js-indent-level 2)
  :commands (json-mode-show-path json-mode-beautify))

;;; [ json-reformat ] -- Reformatting tool for JSON.

(use-package json-reformat
  :ensure t
  :commands (json-reformat-region)
  :custom ((json-reformat:indent-width 2)
           (json-reformat:pretty-string? t)))

;;; [ jq-format ] -- Reformat JSON and JSONLines using "jq".

(use-package jq-format
  :ensure t
  :defer t
  :after json-mode
  :commands (jq-format-json-on-save-mode
             jq-format-json-buffer jq-format-json-region
             jq-format-jsonlines-buffer jq-format-jsonlines-region)
  :hook ((json-mode . jq-format-json-on-save-mode)
         (json-mode . jq-format-jsonlines-on-save-mode)))

;;; [ json-snatcher ]

(use-package json-snatcher
  :ensure t
  :defer t
  :commands (jsons-print-path))

;;; [ json-navigator ] -- view and navigate JSON structures.

(use-package json-navigator
  :ensure t
  :defer t
  :commands (json-navigator-navigate-after-point json-navigator-navigate-region))



(provide 'init-prog-lang-json)

;;; init-prog-lang-json.el ends here
