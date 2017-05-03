;;; init-my-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ TeX-mode ]


;;; [ LaTeX-mode ]


;;; [ AUCTeX ] -- Integrated environment for TeX.

(use-package tex
  :ensure auctex
  :init
  (require 'tex-site)
  (require 'latex)
  :config
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


  ;; Fontification
  (require 'font-latex)
  ;; (setq TeX-install-font-lock)
  ;; macros
  (add-hook 'LaTeX-mode-hook
	          (lambda ()
	            (font-latex-add-keywords '(("citep" "*[[{")) 'reference)
	            (font-latex-add-keywords '(("citet" "*[[{")) 'reference)
	            ))
  ;; quotes
  ;; math
  ;; verbatim content


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

  ;; view generated PDF with `pdf-tools'. (this is built-in now.)
  (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
    (add-to-list 'TeX-view-program-list-builtin
	               '("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))
  ;; (setq-default TeX-PDF-mode t) ; enable by default since AUCTeX 11.88
  ;; [C-c C-g] switch between LaTeX source code and PDF positions.
  (setq TeX-source-correlate-start-server t)
  (TeX-source-correlate-mode t)
  ;; update PDF buffers after successful LaTeX runs.
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer)

  ;; (setq TeX-source-correlate-method)

  ;; LaTeX source code block syntax highlighting.
  ;; [ minted ]
  ;; toggle shell escape using [C-c C-t x].
  (defun TeX-toggle-shell-escape ()
    "Toggle Shell Escape"
    (interactive)
    (setq-local LaTeX-command
	              (if (string= LaTeX-command "latex") "latex -shell-escape"
		              "latex"))
    (setq-local shell-escape-mode "-shell-escape") ; should pdflatex command use shell escaping?
    
    (message (concat "shell escape "
		                 (if (string= LaTeX-command "latex -shell-escape")
		                     "enabled"
		                   "disabled"))
	           ))

  (add-hook 'LaTeX-mode-hook
	          '(lambda ()
	             (TeX-toggle-shell-escape)
	             (local-set-key (kbd "C-c C-t x") 'TeX-toggle-shell-escape)))


  ;; smart tie
  (defun electric-tie ()
    "Inserts a tilde at point unless the point is at a space
character(s), in which case it deletes the space(s) first."
  (interactive)
  (while (equal (char-after) ?\s) (delete-char 1))
  (while (equal (char-before) ?\s) (delete-char -1))
  (call-interactively 'self-insert-command))

(eval-after-load 'tex '(define-key TeX-mode-map "~" 'electric-tie))

(add-hook 'TeX-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("~" . 'font-latex-sedate-face)))))

;;; [C-c C-j] insert items smartly
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (add-to-list 'LaTeX-item-list
                '("frame" . (lambda () (TeX-insert-macro "pause"))))))
)

;;; [ company-auctex ] & [ company-math ]

(use-package company-math
  :ensure t)

(use-package company-auctex
  :ensure t
  :config
  (defun my-company-auctex-setup ()
    ;; indent
    (aggressive-indent-mode)
    
    ;; fold
    (TeX-fold-mode)

    (rainbow-delimiters-mode)
    (smartparens-mode)

    ;; complete
    (make-local-variable 'company-backends)
    ;; company-math
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-math-symbols-latex)
    (add-to-list 'company-backends 'company-latex-commands)

    ;; company-auctex
    (add-to-list 'company-backends 'company-auctex-labels)
    (add-to-list 'company-backends 'company-auctex-bibs)
    (add-to-list 'company-backends 'company-auctex-environments)
    (add-to-list 'company-backends 'company-auctex-symbols)
    (add-to-list 'company-backends 'company-auctex-macros)
    
    ;; linter
    (flycheck-mode 1)

    ;; Doc
    ;; (info-lookup-add-help
    ;;  :mode 'latex-mode
    ;;  :regexp ".*"
    ;;  :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
    ;;  :doc-spec '(("(latex2e)Concept Index" )
    ;;              ("(latex2e)Command Index")))

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
    )

  (dolist (hook '(tex-mode-hook
                  TeX-mode-hook
                  latex-mode-hook
                  LaTeX-mode-hook ; from AUCTeX
                  ))
    (add-hook hook #'my-company-auctex-setup))
  )


;;; [ RefTeX ] -- a specialized package for support of labels, references.

(use-package reftex
  :ensure t
  :config
  (setq reftex-cite-prompt-optional-args t) ; prompt for empty optional arguments in cite.

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)))
  )


;;; [ SyncTeX ] -- navigate from the source document to the typeset material and vice versa.

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (setq TeX-PDF-mode t)
;;             (setq TeX-source-correlate-method 'synctex)
;;             (setq TeX-source-correlate-start-server t))
;;           )


;;; [ CDLaTeX ] -- Fast input methods for LaTeX environments and math.

(use-package cdlatex
  :ensure t
  :init
  (add-hook 'LaTeX-mode-hook #'cdlatex-mode)
  ;; enable in Org-mode
  (add-hook 'org-mode-hook #'org-cdlatex-mode)
  )


;;; [ magic-latex-buffer ] -- magical syntax highlighting for LaTeX-mode buffers.

(use-package magic-latex-buffer
  :ensure t
  :config
  ;; You can disable some features independently, if they’re too fancy.
  (setq magic-latex-enable-block-highlight nil
        magic-latex-enable-suscript        t
        magic-latex-enable-pretty-symbols  t
        magic-latex-enable-block-align     t
        magic-latex-enable-inline-image    t)

  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
  ;; disable this, because `iimage-mode' auto open image in external program
  ;; caused `LaTeX-mode-hook' break.
  ;; (add-hook 'LaTeX-mode-hook 'turn-off-iimage-mode)
  )


;;; [ latex-preview-pane ]

(use-package latex-preview-pane
  :ensure t
  :config
  (setq preview-orientation 'right)

  (latex-preview-pane-enable)
  )


;;; [ px ] -- Provides functions to preview LaTeX codes like $x^2$ in any buffer/mode.

(use-package px
  :ensure t)


;;; tooltip preview for image with `image-tooltips.el'

(setq x-gtk-use-system-tooltips nil)

(defvar image-tooltip-re
  (concat  "\\\\includegraphics{\\(?1:.*\\."
           (regexp-opt '("png" "PNG" "JPG" "jpeg"
                         "jpg" "JPEG" "eps" "EPS" "pdf"))
           "\\)}")
  "Regexp to match included images")


(defun image-tooltip (window object position)
  (save-excursion
    (goto-char position)
    (let (beg end imgfile img s)
      (while (not (looking-at image-tooltip-re))
        (forward-char -1))
      (setq imgfile (match-string-no-properties 1))
      (if (file-exists-p imgfile)
          (progn
            (setq img (create-image (expand-file-name imgfile)
                                    'imagemagick nil :width 800))
            (propertize "Look in the minibuffer"
                        'display img))
        "No file found"))))

(defun img-match (limit)
  "Match bar in at the end of a word."
  (when (re-search-forward image-tooltip-re limit t)
    (flyspell-delete-region-overlays (match-beginning 1)
                                     (match-end 1))
    t))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '((img-match 1 '(face font-lock-keyword-face
                                   help-echo image-tooltip))))))


(provide 'init-my-prog-lang-tex)

;;; init-my-prog-lang-tex.el ends here
