;;; init-my-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ tex-mode ]


;;; [ latex-mode ]


;;; [ AUCTeX ] -- Integrated environment for TeX.

;;; Usage:
;;
;; - [C-c C-e] :: easy to enter environments \begin{...}  ...  \end{...}
;; - [C-c C-p] :: preview prefix.
;; - [C-c C-c] :: compile.
;; - [C-c C-v] :: view the paper.
;; - [C-c `] :: popup compile error.

(require 'tex-site)
(require 'latex)

;;; AUCTeX config
(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-master nil)

;; automatic detection of master file
(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (setq TeX-master (guess-TeX-master (buffer-file-name)))
             ))

;; enable RefTeX in AUCTeX (LaTeX-mode)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode

;; (setq TeX-macro-global '())
;; (setq TeX-outline-extra t)

;; preview-latex config
;; (setq preview-transparent-color '(highlight :background)
;;       preview-auto-reveal
;;       preview-auto-cache-preamble 'ask
;;       )
;;
;; fix iimage-mode auto open preview inline image with external program.
(add-hook 'latex-mode-hook 'turn-off-iimage-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-iimage-mode)


;; LaTeX source code block syntax highlighting.
;; [ minted ]
;; toggle shell escape using [C-c C-t x].
(defun TeX-toggle-escape ()
  "Toggle Shell Escape"
  (interactive)
  (setq-local LaTeX-command
              (if (string= LaTeX-command "latex") "latex -shell-escape"
                "latex"))
  (message (concat "shell escape "
                   (if (string= LaTeX-command "latex -shell-escape")
                       "enabled"
                     "disabled"))
           ))
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))


;;; [ company-auctex ]
;;; [ company-math ]

(dolist (hook '(tex-mode-hook
                latex-mode-hook
                LaTeX-mode-hook ; from AUCTeX
                ))
  (add-hook hook
            '(lambda ()
               ;; indent
               (aggressive-indent-mode)

               ;; fold
               (TeX-fold-mode)

               ;; complete
               (my-company-add-backends-to-mode
                '(company-auctex-macros
                  company-auctex-symbols
                  company-auctex-environments

                  company-auctex-labels
                  company-auctex-bibs
                  
                  ;; company-math
                  company-latex-commands
                  company-math-symbols-latex
                  company-math-symbols-unicode
                  ))

               ;; linter
               (flycheck-mode)


               ;; block
               (local-set-key (kbd "C-c C-i") 'tex-latex-block)
               
               ;; Section
               (setq LaTeX-section-hook
                     '(LaTeX-section-heading
                       LaTeX-section-title
                       LaTeX-section-toc
                       LaTeX-section-section
                       LaTeX-section-label))

               ;; Math
               ;; (LaTeX-math-mode)
               )))


;;; [ latex-pretty-symbols ]

(require 'latex-pretty-symbols)


;;; [ magic-latex-buffer ] -- magical syntax highlighting for LaTeX-mode buffers.

(use-package magic-latex-buffer
  :config
  ;; You can disable some features independently, if they’re too fancy.
  (setq magic-latex-enable-block-highlight nil
        magic-latex-enable-suscript        t
        magic-latex-enable-pretty-symbols  t
        magic-latex-enable-block-align     t
        magic-latex-enable-inline-image    t)

  ;; disable this, because `iimage-mode' auto open image in external program
  ;; caused `LaTeX-mode-hook' break.
  ;;
  ;; (add-hook 'latex-mode-hook 'magic-latex-buffer)
  ;; (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
  )


;;; [ latex-preview-pane ]

;; Usage:
;;
;; - [M-x latex-preview-mode]

;; To use LaTeX Preview Pane, simply open any TeX file and if latex-preview-pane
;; is set to be automatically enabled, it will open a preview pane and attempt
;; to generate your TeX preview. Otherwise you can activate it with M-x
;; latex-preview-pane-mode to open the preview pane. Note that there is also a
;; menu in this mode which contains the following functions:

;; - Refresh Preview (bound to M-p)
;; - Open in External Program (Bound to M-P)
;; - Disable LaTeX Preview Pane (turns the mode off, you can also use M-x
;;   latex-preview-pane-mode to toggle it off.
;; - Customize LaTeX Preview Pane (opens a customization buffer where you can
;;   set the command to use for generating previews)

(use-package latex-preview-pane
  :config
  (setq preview-orientation 'right)
  (latex-preview-pane-enable))


;;; [ px ]

;; Provides functions to preview LaTeX codes like $x^2$ in any
;; buffer/mode.

;; Use `px-preview-region' to preview LaTeX codes delimited by $ pairs
;; in the region.
;; Use `px-preview' to process the whole buffer.
;; Use `px-remove' to remove all images and restore the text back.
;; Use `px-toggle' to toggle between images and text on the whole
;; buffer.

;; Most of this code comes from weechat-latex.el which in turn uses
;; org-mode previewer.

(use-package px)


(provide 'init-my-prog-lang-tex)

;;; init-my-prog-lang-tex.el ends here
