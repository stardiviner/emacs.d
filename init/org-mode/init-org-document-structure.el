;;; init-org-document-structure.el --- init for Org Document Structure
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Editing

(setq org-catch-invisible-edits 'smart)

;;; Navigation

(setq org-special-ctrl-a/e t
      org-special-ctrl-o t)

;;; [ headlines ]

(setq org-fontify-whole-heading-line t)
(setq org-fontify-done-headline t)

(add-hook 'org-mode-hook 'variable-pitch-mode)

;;; [ Org faces ]
(set-face-attribute 'org-document-title nil
                    :family "Gabriola"
                    :height 200)
(set-face-attribute 'org-level-1 nil
                    :inherit 'variable-pitch
                    :family "Comic Sans MS"
                    :weight 'bold :height 130
                    :background (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 5))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 3))))
(set-face-attribute 'org-level-2 nil
                    :inherit 'org-level-1
                    :family "Comic Neue"
                    :height 130)
(set-face-attribute 'org-level-3 nil
                    :inherit 'org-level-2
                    :family "Linux Biolinum"
                    :height 130)
(set-face-attribute 'org-level-4 nil
                    :inherit 'org-level-3
                    :family "Comic Neue")
(set-face-attribute 'org-level-5 nil
                    :inherit 'org-level-4)
(set-face-attribute 'org-level-6 nil
                    :inherit 'org-level-5)
(set-face-attribute 'org-level-7 nil
                    :inherit 'org-level-6)
(set-face-attribute 'org-headline-done nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 20))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 20)))
                    )
(set-face-attribute 'org-agenda-done nil
                    :foreground (cl-case (alist-get 'background-mode (frame-parameters))
                                  ('light
                                   (color-darken-name (face-background 'default) 20))
                                  ('dark
                                   (color-lighten-name (face-background 'default) 20)))
                    )
(set-face-attribute 'org-code nil
                    :inherit 'fixed-pitch)
(set-face-attribute 'org-verbatim nil
                    :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil
                    :inherit 'fixed-pitch)

;;; [ priority ]

(setq org-priority-faces
      '((?A .
            (:foreground "dark gray"
                         :background "OrangeRed"
                         :box '(:color "dark gray" :line-width -1)))
        (?B .
            (:foreground "dark gray"
                         :background "dark slate blue"
                         :box '(:color "dark gray" :line-width -1)))
        (?C .
            (:foreground "dim gray"
                         :background "gray"
                         :box '(:color "dark gray" :line-width -1)))
        ))

;; * Plain Lists::

(setq org-list-allow-alphabetical t)

;;; [ column view ]

;; (setq org-columns-default-format
;;       "%25ITEM %TODO %3PRIORITY %TAGS %6effort(EFFORT){:}")

;; setup column views for effort estimates
(setq org-columns-default-format
      "%50ITEM(Task) %8TODO %1PRIORITY %14TIMESTAMP_IA(TimeStamp) %Effort(Effort){:}"
      ;; Default column view headings
      ;; - %ITEM(Task) :: Headline (where (Task) is the column head)
      ;; - %PRIORITY :: Priority
      ;; - %TAGS :: tags
      ;; - %CLOCKSUM :: Clock Sum
      ;; - %ITEMSTAMP_IA :: Timestamp
      ;; - %Effort(Effort){:} :: Effort
      
      ;; org-global-properties
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-columns-compute-summary-properties t
      )

;;; Statistics [1/4]

(setq org-hierarchical-todo-statistics nil
      org-checkbox-hierarchical-statistics nil)

;;; TODO: does this work?
;; will be combined with constant `org-global-properties-fixed'
;; (add-to-list 'org-global-properties '("Effort" . "0:30"))
;; (add-to-list 'org-global-properties '("Title" . nil))
;; (add-to-list 'org-global-properties '("Author" . "stardiviner"))

;; * Blocks::                      Folding blocks

;; * Footnotes::                   How footnotes are defined in Org's syntax

(setq org-footnote-auto-label 'confirm
      org-footnote-auto-adjust t
      org-footnote-define-inline nil ; t: define foot inline, instead of separate section.
      org-footnote-fill-after-inline-note-extraction t
      )

;;; [ Structure Templates ] -- <[s] expand.

;; (require 'org-tempo) ; expand snippet <KEY
;; (add-to-list 'org-structure-template-alist '("?" . "..."))
;; `tempo-define-template'

;;; [ Entities ]

(defun org-insert-entity-with-ivy ()
  "Insert an org-entity using Ivy."
  (interactive)
  (ivy-read "Entity: "
            (loop for element in (append org-entities org-entities-user)
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

;;; [ Dynamic Blocks ] -- [C-c C-x i]

;;; insert the kbd tag
(defun my/org-insert-key (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((orgp (derived-mode-p 'org-mode))
         (tag (if orgp "@@html:<kbd>@@%s@@html:</kbd>@@" "<kbd>%s</kbd>")))
    (if (null (equal key (kbd "C-m")))
        (insert
         (format tag (help-key-description key nil)))
      ;; If you just hit RET.
      (insert (format tag ""))
      (forward-char (if orgp -1 -6)))))

(define-key org-mode-map (kbd "C-c k") #'my/org-insert-key)

;;; [ org-outline-numbering ] -- displays an outline numbering as overlays on Org mode headlines.

;; (use-package org-outline-numbering
;;   :ensure t
;;   :init (add-hook 'org-mode-hook #'org-outline-numbering-mode))

;;; [ org-lint ] -- Org-mode linter. [M-x org-lint]

;; (require 'org-lint)

(add-to-list 'display-buffer-alist
             '("^\\*Org Lint\\*" . (display-buffer-below-selected)))


(provide 'init-org-document-structure)

;;; init-org-document-structure.el ends here
