;;; init-mermaid.el --- init for mermaid

;;; Time-stamp: <2018-08-02 08:36:30 stardiviner>

;;; Commentary:



;;; Code:

;;; [ ob-mermaid ] -- Generate mermaid diagrams using org-mode, org-babel and mermaid.cli.

(use-package ob-mermaid
  :ensure t
  :init
  ;; for local installation: "npm install mermaid"
  ;; (setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc")
  ;; for global installation: "npm install -g mermaid"
  (setq ob-mermaid-cli-path "~/Libraries/node_modules/bin/mmdc")
  )



(provide 'init-mermaid)

;;; init-mermaid.el ends here
