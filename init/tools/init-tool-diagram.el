;;; init-tool-diagram.el --- init Emacs diagram tools
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ artist-mode ]

(use-package artist
  :ensure t
  :defer t
  :bind (:map tools-prefix ("a" . artist-mode))
  :config
  (defun artist-select-colors ()
    "Insert cXXX format colors for ditaa boxes."
    (interactive)
    (let* ((color-name
            (completing-read
             "cXXX: "
             (list "hex-number" "black" "red" "green" "yellow" "blue" "pink")))
           (color-value
            (cond
             ((equal color-name "hex-number") color-name)
             ((equal color-name "black") "BLK")
             ((equal color-name "red") "RED")
             ((equal color-name "green") "GRE")
             ((equal color-name "yellow") "YEL")
             ((equal color-name "blue") "BLU")
             ((equal color-name "pink") "PNK"))))
      (artist-text-insert-overwrite (current-column) (1- (count-lines 1 (point)))
                                    (concat "c" color-value))))

  (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-select-colors)
  (define-key artist-mode-map (kbd "C-c C-o") 'artist-select-operation))

;;; [ picture ] -- "Picture mode" -- editing using quarter-plane screen model.

(use-package picture
  :defer t)

;;; [ asymptote ] -- A vector graphics language (like metapost).

(use-package ob-asymptote
  :defer t
  :commands (org-babel-execute:asymptote)
  :config
  (add-to-list 'org-babel-load-languages '(asymptote . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("asymptote" . "asy")))

(require 'init-UML)
(require 'init-Graphviz) ; Graphviz, Dot
(require 'init-ditaa)
(require 'init-blockdiag)
(require 'init-mermaid)



(provide 'init-tool-diagram)

;;; init-tool-diagram.el ends here
