;;; init-my-org-drill.el --- init for Memory with Drill Sessions based on Org-mode.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-drill ] -- Begin an interactive "drill session" based on Org-mode.

(use-package org-drill
  :config
  ;; add org-drill topic property into default properties list.
  (add-to-list 'org-default-properties "DRILL_CARD_TYPE")

  (setq org-drill-use-visible-cloze-face-p nil ; t will caused [] invalid headline fontify.
        org-drill-hide-item-headings-p nil
        org-drill-save-buffers-after-drill-sessions-p nil
        ;; org-drill-spaced-repetition-algorithm 'sm2
        )
  )

;;; [ org-drill-table ] -- generate org-drill from org-mode tables.

;; (use-package org-drill-table
;;   :ensure t)

;;; [ record queried words to Org-mode drill files ]

(require 'org-capture) ; load `org-capture-templates'

(defvar my-org-drill-words-file (concat org-directory "/Tasks/Words.org"))

(setq org-capture-templates
      (append '(("w" "org-drill [w]ords"
                 entry (file my-org-drill-words-file)
                 "* %^{word} %^{tag :drill: }g
:PROPERTIES:
:DRILL_CARD_TYPE: %^{Difficulty|simple|twosided|multisided|hide1cloze}
:END:"
                 :empty-lines 1
                 ))
              org-capture-templates))


(define-key my-org-prefix (kbd "w") 'org-drill)

(defun my-org-drill-open ()
  "Open org-drill words file for review."
  (interactive)
  ;; (find-file my-org-drill-words-file)
  ;; or:
  ;; When `org-capture' called interactively with [C-u] will goto the target file/heading.
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-capture "w"))
  )

(define-key my-org-prefix (kbd "C-w") 'my-org-drill-open)

(use-package stem-english
  :ensure t)

(defun my-org-drill-check ()
  "Check word exist in Words.org file?"
  )

(defun my-org-drill-record-word ()
  "Record word to org-drill words file."
  (interactive)
  (let ((word (my-stem-english ; word stemmer
               (downcase
                (if (region-active-p)
                    (buffer-substring-no-properties (mark) (point))
                  (thing-at-point 'word))))))
    (if (and (my-org-drill-check)
             (yes-or-no-p (format "org-drill this word (%s): " word)))
        ;; call org-capture template programmatically.
        (org-capture nil "w")
      ;; (call-interactively #'org-capture)
      ;; (call-interactively (org-capture nil "w"))
      (message "without recording word to org-drill."))))

(declare-function 'goldendict-dwim "init-my-tool-dictionary")
;; (advice-add 'goldendict-dwim :after #'my-org-drill-record-word)


;;; [ pamparam ] -- Simple and fast flashcards for Emacs.

(use-package pamparam
  :quelpa (pamparam :fetcher github :repo "abo-abo/pamparam")
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-org-drill)

;;; init-my-org-drill.el ends here
