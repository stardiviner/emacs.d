;;; init-my-org-document-structure.el --- init for Org Document Structure
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; * Outlines::                    Org is based on Outline mode

;; * Headlines::                   How to typeset Org tree headlines

;; * Plain Lists::

(require 'org-list)

(setq org-list-allow-alphabetical t)

;; (setq org-list-demote-modify-bullet
;;       '(("+" . "-")
;;         ;; ("-" . "+")
;;         ("*" . "-")
;;         ))

;;; List checkbox:

;;; Prettify List Checkbox.
;; (defun org-mode-list-checkbox-prettify ()
;;   (push '("[ ]" .  "☐") prettify-symbols-alist)
;;   (push '("[X]" . "☒" ) prettify-symbols-alist)
;;   (push '("[-]" . "☑" ) prettify-symbols-alist)
;;   (prettify-symbols-mode)
;;   )
;;
;; (add-hook 'org-mode-hook #'org-mode-list-checkbox-prettify)
;;
;; ;;; also enable in HTML export.
;; (setq org-html-checkbox-type 'unicode)
;; (setq org-html-checkbox-types
;;       '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
;;                  (off . "<span class=\"task-todo\">&#x2610;</span>")
;;                  (trans . "<span class=\"task-in-progress\">&#x25eb;</span>"))))

(setq org-columns-default-format
      "%25ITEM %TODO %3PRIORITY %TAGS %6effort(EFFORT){:}")

(setq org-catch-invisible-edits 'smart)

;;; Statistics [1/4]

(setq org-hierarchical-todo-statistics nil
      org-checkbox-hierarchical-statistics nil)

;; * Table

;; * Plotting

;; [ org-plot ] -- Plotting Tables in Org-mode.

(require 'org-plot)

;;; Org Table translator functions.
(add-to-list 'org-default-properties "ORGTBL") ; for Org-mode Table translator functions.

;; define a keybinding for org table translator functions
(define-key org-mode-map (kbd "C-c \" i") 'orgtbl-insert-radio-table)
(define-key org-mode-map (kbd "C-c \" s") 'orgtbl-send-table)

;; * Visibility cycling::          Show and hide, much simplified

;; * Motion::                      Jumping to other headlines

;; * Structure editing::           Changing sequence and level of headlines

(setq org-special-ctrl-a/e t
      org-special-ctrl-o t)

;; * Sparse trees::                Matches embedded in context

;; * Drawers::                     Tucking stuff away

;; (setq org-global-properties ; will be combined with constant `org-global-properties-fixed'
;;       '(("Effort" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
;;         ("Title" . nil)
;;         ("Author" . "stardiviner")
;;         ))

;; * Blocks::                      Folding blocks

;; * Footnotes::                   How footnotes are defined in Org's syntax

(setq org-footnote-auto-label 'confirm
      org-footnote-auto-adjust t
      org-footnote-define-inline nil ; t: define foot inline, instead of separate section.
      org-footnote-fill-after-inline-note-extraction t
      )

;; * Orgstruct mode::              Structure editing outside Org

;; * Org syntax::                  Formal description of Org's syntax

;;; [ Structure Templates ]

;; translate special block
(add-to-list 'org-structure-template-alist '(?t . "translate"))

;;; [ Entities ]

(defun ivy-insert-org-entity ()
  "Insert an org-entity using ivy."
  (interactive)
  (ivy-read "Entity: " (loop for element in (append org-entities org-entities-user)
			                       when (not (stringp element))
			                       collect
			                       (cons 
			                        (format "%10s | %s | %s | %s"
				                              (car element) ;name
				                              (nth 1 element) ; latex
				                              (nth 3 element) ; html
				                              (nth 6 element)) ;utf-8
			                        element))
	          :require-match t
	          :action '(1
		                  ("u" (lambda (element) (insert (nth 6 (cdr element)))) "utf-8")
		                  ("o" (lambda (element) (insert "\\" (cadr element))) "org-entity")
		                  ("l" (lambda (element) (insert (nth 1 (cdr element)))) "latex")
		                  ("h" (lambda (element) (insert (nth 3 (cdr element)))) "html"))))


(provide 'init-my-org-document-structure)

;;; init-my-org-document-structure.el ends here
