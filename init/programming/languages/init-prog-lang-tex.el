;;; init-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ TeX-mode ]

;;; Use `xetex' engine for better TeX compilation for Chinese.
;; `TeX-engine-alist', `TeX-engine-in-engine-alist'
(setq-default TeX-engine 'xetex)
(with-eval-after-load 'tex-mode
  ;; "latexmk -shell-escape -bibtex -xelatex -g -f %f"
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))
(setq-default LaTeX-command  "latex -shell-escape")

;;; [ LaTeX-mode ]


;;; [ AUCTeX ] -- Integrated environment for *TeX*.

(use-package auctex
  :ensure t
  :no-require t
  :commands (TeX-latex-mode)
  :config
  ;; macros
  (defun latex-font-lock-add-macros ()
    (font-latex-add-keywords '(("citep" "*[[{")) 'reference)
	  (font-latex-add-keywords '(("citet" "*[[{")) 'reference))
  (add-hook 'LaTeX-mode-hook #'latex-font-lock-add-macros)

  ;; [ Preview ] -- [C-c C-p C-p]
  ;; (setq preview-transparent-color '(highlight :background)
  ;;       preview-auto-reveal
  ;;       preview-auto-cache-preamble 'ask
  ;;       )

  ;; increase TeX/LaTeX preview scale size.
  (setq preview-scale-function 1.7)
  
  ;; view generated PDF with `pdf-tools'. (this is built-in now.)
  (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
    (add-to-list 'TeX-view-program-list-builtin '("PDF Tools" TeX-pdf-tools-sync-view)))
  (unless (equalp "PDF Tools" (car (cdr (assoc 'output-pdf TeX-view-program-selection))))
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))
  
  ;; (setq-default TeX-PDF-mode t) ; enable by default since AUCTeX 11.88
  ;; [C-c C-g] switch between LaTeX source code and PDF positions.
  (setq TeX-source-correlate-start-server t)
  (TeX-source-correlate-mode t)
  ;; update PDF buffers after successful LaTeX runs.
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer)

  ;; (setq TeX-source-correlate-method)

  ;; auto close dollars
  (setq TeX-electric-math (cons "$" "$"))
  (setq TeX-electric-sub-and-superscript t) ; use _{} instead of _
  (setq font-latex-fontify-script 'multi-level) ; 2^2^2 as multiple scripts

  ;; smart tie
  (defun electric-tie ()
    "Inserts a tilde at point unless the point is at a space
character(s), in which case it deletes the space(s) first."
    (interactive)
    (while (equal (char-after) ?\s) (delete-char 1))
    (while (equal (char-before) ?\s) (delete-char -1))
    (call-interactively 'self-insert-command))
  (eval-after-load 'tex '(define-key TeX-mode-map "~" 'electric-tie))

  (defun TeX-font-lock-add-tie ()
    (font-lock-add-keywords nil '(("~" . 'font-latex-sedate-face))))
  (add-hook 'TeX-mode-hook #'TeX-font-lock-add-tie)

  ;; [C-c C-j] insert items smartly
  (defun LaTeX-insert-item-smartly ()
    (add-to-list 'LaTeX-item-list '("frame" . (lambda () (TeX-insert-macro "pause")))))
  (add-hook 'LaTeX-mode-hook #'LaTeX-insert-item-smartly)

  (defun my:tex-mode-setup ()
    ;; indent
    (aggressive-indent-mode)
    ;; fold: hide some boilerplate
    (TeX-fold-mode)
    ;; electric
    (rainbow-delimiters-mode)
    (if (featurep 'smartparens)
        (smartparens-mode))
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
    (add-hook hook #'my:tex-mode-setup))

  ;; Big faces for sections, chapters, etc.
  (set-face-attribute 'font-latex-sectioning-1-face nil
                      :height 1.5 :bold t)
  (set-face-attribute 'font-latex-sectioning-2-face nil
                      :height 1.2 :bold t)
  (set-face-attribute 'font-latex-sectioning-3-face nil
                      :height 1.2 :bold nil))

(use-package company-auctex
  :ensure t
  :defer t
  :config
  (defun my:company-auctex-setup ()
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
    (add-to-list 'company-backends 'company-auctex-macros))

  (dolist (hook '(tex-mode-hook TeX-mode-hook latex-mode-hook LaTeX-mode-hook))
    (add-hook hook #'my:company-auctex-setup)))

(use-package company-math
  :ensure t
  :defer t)

;;; [ RefTeX ] -- a specialized package for support of labels, references.

(use-package reftex
  :ensure t
  :defer t
  :init
  (setq reftex-cite-prompt-optional-args t) ; prompt for empty optional arguments in cite.
  ;; enable RefTeX in AUCTeX (LaTeX-mode)
  (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
  (add-hook 'LaTeX-mode-hook #'reftex-mode))


;;; [ SyncTeX ] -- navigate from the source document to the typeset material and vice versa.

;; (defun my/SyncTeX-setup ()
;;   (setq TeX-PDF-mode t)
;;   (setq TeX-source-correlate-method 'synctex)
;;   (setq TeX-source-correlate-start-server t))

;; (add-hook 'LaTeX-mode-hook #'my/SyncTeX-setup)


;;; [ CDLaTeX ] -- Fast input methods for LaTeX environments and math.

;; (use-package cdlatex
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'LaTeX-mode-hook #'cdlatex-mode)
;;   ;; enable in Org-mode
;;   (add-hook 'org-mode-hook #'org-cdlatex-mode)
;;   (add-to-list 'display-buffer-alist
;;                '("^\\*CDLaTeX Help\\*" (display-buffer-below-selected)))
;;   )


;;; [ magic-latex-buffer ] -- magical syntax highlighting for LaTeX-mode buffers.

(use-package magic-latex-buffer
  :ensure t
  :defer t
  :hook (LaTeX-mode-hook . magic-latex-buffer)
  :init
  ;; disable this, because `iimage-mode' auto open image in external program
  ;; caused `LaTeX-mode-hook' break.
  ;; (add-hook 'LaTeX-mode-hook 'turn-off-iimage-mode)

  ;; You can disable some features independently, if they’re too fancy.
  (setq magic-latex-enable-block-highlight nil
        magic-latex-enable-suscript        t
        magic-latex-enable-pretty-symbols  t
        magic-latex-enable-block-align     t
        magic-latex-enable-inline-image    t))


;;; [ latex-preview-pane ] -- Makes LaTeX editing less painful by providing a updatable preview pane.

(use-package latex-preview-pane
  :ensure t
  :defer t
  :init (setq-default shell-escape-mode "-shell-escape")
  (setq preview-orientation 'right)
  (latex-preview-pane-enable))

;;; [ px ] -- Provides functions to preview LaTeX codes like $x^2$ in any buffer/mode.

;; (use-package px
;;   :ensure t)


(provide 'init-prog-lang-tex)

;;; init-prog-lang-tex.el ends here
