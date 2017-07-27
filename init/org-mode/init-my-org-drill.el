;;; init-my-org-drill.el --- init for Memory with Drill Sessions based on Org-mode.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-drill ] -- Begin an interactive "drill session" based on Org-mode.

(use-package org-drill
  :config
  ;; add org-drill topic property into default properties list.
  (add-to-list 'org-default-properties "DRILL_CARD_TYPE")
  (setq org-tag-alist
        (append '((:startgroup . nil) ("drill" . ?d) (:endgroup . nil))
                org-tag-alist))

  (setq org-drill-use-visible-cloze-face-p nil ; t will caused [] invalid headline fontify.
        org-drill-hide-item-headings-p nil
        org-drill-save-buffers-after-drill-sessions-p nil
        ;; org-drill-spaced-repetition-algorithm 'sm2
        )

  ;; [ record queried words to Org-mode drill files ]
  (require 'org-capture) ; load `org-capture-templates'

  (defvar my-org-drill-words-file (concat org-directory "/Tasks/Words.org"))

  (setq org-capture-templates
        (append '(("w" "org-drill [w]ords"
                   entry (file my-org-drill-words-file)
                   "* %^{word} :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: %^{Drill Difficulty|simple|twosided|multisided|hide1cloze}
:END:

%?

%c
"
                   :empty-lines 1
                   ))
                org-capture-templates))

  (define-key my-org-prefix (kbd "w") 'org-drill)

  (use-package stem-english
    :ensure t)

  (defun my-org-drill-check-not-exist (word)
    "Check word exist in Words.org file?"
    (let ((drill-words (org-map-entries
                        (lambda () (nth 4 (org-heading-components)))
                        nil
                        (list (expand-file-name my-org-drill-words-file)))))
      (not ; reverse word exist boolean
       (cl-some (lambda (drill) (string-equal drill word)) drill-words))))

  (defun my-org-drill-record-word ()
    "Record word to org-drill words file."
    (interactive)
    (let ((word (my-func/stem-word-at-point-with-format)))
      (unless (not (and (my-org-drill-check-not-exist word)
                        (yes-or-no-p (format "org-drill this word (%s): " word))))
        ;; copy original source sentence for `org-capture' template `%c'.
        (save-mark-and-excursion
          (ignore-errors (er/mark-sentence))
          (kill-ring-save (region-beginning) (region-end))
          (if (region-active-p) (deactivate-mark)))
        ;; region select word for `org-capture' template "`%i'".
        (unless (region-active-p) (er/mark-word))
        ;; call org-capture template programmatically.
        (org-capture nil "w")
        ;; disable region selection.
        ;; FIXME the current buffer is CAPTURE buffer.
        (if (region-active-p) (deactivate-mark))
        )))

  (declare-function 'goldendict-dwim "init-my-tool-dictionary")
  (advice-add 'goldendict-dwim :after #'my-org-drill-record-word)

  (define-key my-org-prefix (kbd "C-w") 'my-org-drill-record-word)
  )

;;; [ org-drill-table ] -- generate org-drill from org-mode tables.

;; (use-package org-drill-table
;;   :ensure t)

;;; [ pamparam ] -- Simple and fast flashcards for Emacs.

(use-package pamparam
  :ensure t
  :bind (:map my-org-prefix
              ("M-w" . hydra-pamparam/body))
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-org-drill)

;;; init-my-org-drill.el ends here
