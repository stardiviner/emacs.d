;;; init-my-tool-diagram.el --- init Emacs diagram tools
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ artist-mode ]

(use-package artist
  :ensure t
  :defer t
  :init
  (define-key my-tools-prefix (kbd "a") 'artist-mode)

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
             ((equal color-name "pink") "PNK")
             )))
      
      (artist-text-insert-overwrite (current-column) (1- (count-lines 1 (point)))
                                    (concat "c" color-value))
      ))

  (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-select-colors)
  (define-key artist-mode-map (kbd "C-c C-o") 'artist-select-operation)
  )


;;; [ picture-mode ]



;;; ditaa


;;; graphviz-dot-mode

;; (use-package graphviz-dot-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'find-file-hook
;;             (lambda()
;;               (if (string= "dot" (file-name-extension
;;                                   buffer-file-name))
;;                   (progn
;;                     (message "Enabling Setings for dot-mode")
;;                     (setq fill-column 1000)
;;                     (base-auto-pair)
;;                     (local-set-key (kbd "<C-f6>") 'compile)
;;                     )
;;                 )))
;;   )


;;; [ plantuml-mode ] -- Major mode for PlantUML.

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-jar-path (locate-user-emacs-file "init/extra/plantuml.jar"))
  )



(provide 'init-my-tool-diagram)

;;; init-my-tool-diagram.el ends here
