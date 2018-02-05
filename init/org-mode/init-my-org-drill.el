;;; init-my-org-drill.el --- init for Memory with Drill Sessions based on Org-mode.

;;; Commentary:



;;; Code:

;;; [ org-drill ] -- Begin an interactive "drill session" based on Org-mode.

(use-package org-drill
  :config
  ;; add org-drill topic property into default properties list.
  (add-to-list 'org-default-properties "DRILL_CARD_TYPE")
  (setq org-tag-alist
        (append '((:startgroup . nil) ("drill" . ?d) (:endgroup . nil))
                org-tag-alist))
  (add-to-list 'org-tag-faces
               '("drill" :foreground "coral"))

  (setq org-drill-use-visible-cloze-face-p nil ; t will caused [] invalid headline fontify.
        org-drill-hide-item-headings-p nil
        org-drill-save-buffers-after-drill-sessions-p nil
        ;; org-drill-spaced-repetition-algorithm 'sm2
        )

  ;; [ record queried words to Org-mode drill files ]
  (require 'org-capture) ; load `org-capture-templates'

  (setq my-org-drill-words-file (concat org-directory "/Tasks/Words/Words.org"))

  (setq org-capture-templates
        (append '(("w" "org-drill [w]ords"
                   entry (file my-org-drill-words-file)
                   "* %i :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: %^{Drill Difficulty|simple|twosided|multisided|hide1cloze}
:END:

[%?]

%c
"
                   :empty-lines 1
                   ))
                org-capture-templates))

  (defun my-org-drill ()
    "My wrapper helper function around `org-drill'."
    (interactive)
    (find-file my-org-drill-words-file)
    (org-drill))
  
  (define-key Org-prefix (kbd "w") 'my-org-drill)

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
    (let ((word (downcase
                 (substring-no-properties
                  (if (region-active-p)
                      (buffer-substring-no-properties (mark) (point))
                    (thing-at-point 'word))))))
      (unless (not (and (my-org-drill-check-not-exist word)
                        (yes-or-no-p (format "org-drill record this word (%s): " word))))
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
        ;; FIXME: the current buffer is CAPTURE buffer.
        (if (region-active-p) (deactivate-mark))
        )))

  (autoload 'goldendict-dwim "goldendict")
  (advice-add 'goldendict-dwim :after #'my-org-drill-record-word)

  (define-key Org-prefix (kbd "C-w") 'my-org-drill-record-word)

  ;; auto pronounce the drill word.
  (defcustom org-drill-pronounce-command "espeak"
    "Specify word pronounce command."
    :type 'string
    :group 'org-drill)

  (defcustom org-drill-pronounce-command-args "-v en"
    "Specify word pronounce command arguments."
    :type 'string
    :group 'org-drill)

  (defun org-drill-pronounce-word (&optional word)
    "Pronounce `WORD' after querying."
    (interactive)
    (shell-command-to-string
     (format "%s %s %s &"
             org-drill-pronounce-command org-drill-pronounce-command-args
             (shell-quote-argument
              (if word
                  word
                (save-excursion
                  (unless (org-at-heading-p)
                    (org-back-to-heading))
                  (org-element-property :raw-value (org-element-context)))
                )))))

  (advice-add 'org-drill-entry :before #'org-drill-pronounce-word)
  )

;;; [ org-drill-table ] -- generate org-drill from org-mode tables.

;; (use-package org-drill-table
;;   :ensure t)

;;; [ pamparam ] -- Simple and fast flashcards for Emacs.

(use-package pamparam
  :ensure t
  :bind (:map Org-prefix
              ("M-w" . hydra-pamparam/body))
  )


(provide 'init-my-org-drill)

;;; init-my-org-drill.el ends here
