;;; init-my-tool-diagram.el --- init Emacs diagram tools
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; artist-mode

;;; Usage:
;; - [M-x artist-mode RET]
;; Note: If using the keyboard to draw, use [C-u RET] to stop drawing.
;; - [C-c C-a C-o] :: select operation.
;; - [C-c C-a C-c] :: select setting.

(define-key my-tools-prefix-map (kbd "a") 'artist-mode)

(if (featurep 'ido)
    (lambda ()
      ;; integrate ido with artist-mode
      (defun artist-ido-select-operation (type)
        "Use ido to select a drawing operation in artist-mode"
        (interactive (list (ido-completing-read "Drawing operation: " 
                                                (list "Pen" "Pen Line" "line" "straight line" "rectangle" 
                                                      "square" "poly-line" "straight poly-line" "ellipse" 
                                                      "circle" "text see-thru" "text-overwrite" "spray-can" 
                                                      "erase char" "erase rectangle" "vaporize line" "vaporize lines" 
                                                      "cut rectangle" "cut square" "copy rectangle" "copy square" 
                                                      "paste" "flood-fill"))))
        (artist-select-operation type))

      (defun artist-ido-select-settings (type)
        "Use ido to select a setting to change in artist-mode"
        (interactive (list (ido-completing-read "Setting: " 
                                                (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars" 
                                                      "Rubber-banding" "Trimming" "Borders"))))
        (if (equal type "Spray-size") 
            (artist-select-operation "spray set size")
          (call-interactively (artist-fc-get-fn-from-symbol 
                               (cdr (assoc type '(("Set Fill" . set-fill)
                                                  ("Set Line" . set-line)
                                                  ("Set Erase" . set-erase)
                                                  ("Rubber-banding" . rubber-band)
                                                  ("Trimming" . trimming)
                                                  ("Borders" . borders)
                                                  ("Spray-chars" . spray-chars))))))))

      (add-hook 'artist-mode-init-hook 
                (lambda ()
                  (define-key artist-mode-map (kbd "C-c C-a C-o") 'artist-ido-select-operation)
                  (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-ido-select-settings)))
      ))



;;; [ picture-mode ]

;;; Usage:
;; - [M-x picture-mode]


;;; ditaa


;;; graphviz-dot-mode

;;; Usage:
;; - [C-c c] :: compile
;; - [C-c p] :: preview graph
;; - [M-;] :: 注释或者取消注释

;; (unless (package-installed-p 'graphviz-dot-mode)
;;   (package-install 'graphviz-dot-mode))

;; (load "graphviz-dot-mode.el" nil t t)

;; (add-hook 'find-file-hook (lambda()
;;                             (if (string= "dot" (file-name-extension
;;                                                 buffer-file-name))
;;                                 (progn
;;                                   (message "Enabling Setings for dot-mode")
;;                                   (setq fill-column 1000)
;;                                   (base-auto-pair)
;;                                   (local-set-key (kbd "<C-f6>") 'compile)
;;                                   )
;;                               )))





(provide 'init-my-tool-diagram)

;;; init-my-tool-diagram.el ends here
