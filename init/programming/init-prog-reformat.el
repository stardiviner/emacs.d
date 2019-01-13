;;; init-prog-reformat.el --- init for reformatters

;;; Time-stamp: <2019-01-13 15:19:41 stardiviner>

;;; Commentary:



;;; Code:

;;; [ reformatter ] -- Define commands which run reformatters on the current buffer.

(use-package reformatter
  :ensure t
  :config
  (reformatter-define json-format
                      :program "jq"
                      :args '("--indent" "4"))

  (reformatter-define yapf-format
                      :program "yapf")

  (reformatter-define isort-format
                      :program "isort"
                      :args '("--apply" "-")))



(provide 'init-prog-reformat)

;;; init-prog-reformat.el ends here
