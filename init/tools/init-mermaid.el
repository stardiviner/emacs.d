;;; init-mermaid.el --- init for mermaid

;;; Commentary:



;;; Code:

;;; [ ob-mermaid ] -- Generate mermaid diagrams using org-mode, org-babel and mermaid.cli.

(use-package ob-mermaid
  :ensure t
  :defer t
  :commands (org-babel-execute:mermaid)
  :custom (;; for local installation: "npm install mermaid"
           (ob-mermaid-cli-path "~/node_modules/.bin/mmdc")
           ;; for global installation: "npm install -g mermaid"
           ;; (ob-mermaid-cli-path "~/Libraries/node_modules/bin/mmdc")
           ))



(provide 'init-mermaid)

;;; init-mermaid.el ends here
