;;; init-org-drill.el --- init for Memory with Drill Sessions based on Org-mode.

;;; Commentary:



;;; Code:

;;; [ org-drill ] -- Begin an interactive "drill session" based on Org-mode.

(use-package org-drill
  :ensure t
  :ensure org-drill-table
  :commands (org-drill
             org-drill-table-generate org-drill-table-update org-drill-table-update-all
             org-drill-entries-pending-p org-drill-resume)
  :init
  (setq org-drill-use-visible-cloze-face-p nil ; t will caused [] invalid headline fontify.
        org-drill-hide-item-headings-p nil
        org-drill-save-buffers-after-drill-sessions-p nil
        ;; org-drill-spaced-repetition-algorithm 'sm2
        )
  (setq my-org-drill-words-file (concat org-directory "/Drills/Words.org"))
  
  ;; add org-drill topic property into default properties list.
  (add-to-list 'org-default-properties "DRILL_CARD_TYPE")
  (setq org-tag-alist
        (append '((:startgroup . nil) ("drill" . ?d) (:endgroup . nil))
                org-tag-alist))
  (add-to-list 'org-tag-faces
               '("drill" :foreground "coral"))

  (defun my-org-drill ()
    "My wrapper helper function around `org-drill'."
    (interactive)
    (find-file my-org-drill-words-file)
    (if (org-drill-entries-pending-p)
        (org-drill-resume)
      (org-drill)))
  (define-key Org-prefix (kbd "w") 'my-org-drill)

  ;; record (GoldenDict) queried words to Org-mode drill files for memory.
  (require 'org-capture) ; load `org-capture-templates'

  (add-to-list 'org-capture-templates
               `("w" ,(format "%s\torg-drill words"
                              (all-the-icons-material "check_box" :face 'all-the-icons-lyellow))
                 entry (file ,my-org-drill-words-file)
                 "* %(downcase \"%i\") :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: %^{Drill Difficulty|simple|twosided|multisided|hide1cloze}
:END:

- : [%?]

%c
"
                 :empty-lines 1)
               :append)

  (defun my-org-drill-word-exist-p (word)
    "Check word exist in Words.org file?"
    (let ((drill-words (reverse
                        (org-map-entries
                         (lambda () (nth 4 (org-heading-components)))
                         nil
                         (list (expand-file-name my-org-drill-words-file))))))
      (cl-some (lambda (drill) (string-equal (car (stem-english word)) drill)) drill-words)))

  (defun my-org-drill-record-word ()
    "Record word to org-drill words file."
    (interactive)
    (require 'expand-region)
    (let ((word (downcase
                 (substring-no-properties
                  (if (region-active-p)
                      (buffer-substring-no-properties (mark) (point))
                    (thing-at-point 'word))))))
      (unless (my-org-drill-word-exist-p word)
        (when (yes-or-no-p (format "org-drill record this word (%s): " word))
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
          (if (region-active-p) (deactivate-mark))))))  

  (autoload 'goldendict-dwim "goldendict")
  (advice-add 'goldendict-dwim :after #'my-org-drill-record-word)
  (define-key Org-prefix (kbd "C-w") 'my-org-drill-record-word))


(use-package stem-english
  :ensure t
  :defer t)

;; TODO: still invalid [s] keybinding setup in org-drill session.
;; (defun org-drill-setup-in-file ()
;;   (if (string= (buffer-name) (file-name-nondirectory my-org-drill-words-file))
;;       (local-set-key (kbd "s") 'org-drill-pronounce-word)))
;; (add-hook 'find-file-hook #'org-drill-setup-in-file)


;;; [ org-drill-table ] -- generate org-drill from org-mode tables.

;; (use-package org-drill-table
;;   :ensure t)

;;; [ pamparam ] -- Simple and fast flashcards for Emacs.

(use-package pamparam
  :ensure t
  :defer t
  :commands (hydra-pamparam/body)
  :bind (:map Org-prefix ("M-w" . hydra-pamparam/body)))

;;; [ guess-word ] -- Emacs guess word game for learning ESL. Emacs 背单词.

;; (use-package guess-word
;;   :quelpa (guess-word :fetcher github :repo "Qquanwei/emacs-guess-word-game")
;;   :commands (guess-word))

;;; [ mybigword ] -- Vocabulary builder using Zipf to extract English big words.

;; (use-package mybigword
;;   :ensure t
;;   :custom (mybigword-data-file "~/Org/Drills/mybigword-english.zipf")
;;   :commands (mybigword-show-big-words-from-file
;;              mybigword-show-big-words-from-current-buffer
;;              mybigword-play-video-of-word-at-point))

;;; [ anki ] -- Yet Another Anki Emacs Client.

(use-package anki
  :quelpa (anki :fetcher github :repo "chenyanming/anki.el")
  :defer t
  :commands (anki)
  :custom ((anki-collection-dir "~/.local/share/Anki2/stardiviner")))

;;; [ org-anki ] -- Synchronize Org Mode entries to Anki.

(use-package org-anki
  :ensure t
  :defer t
  :commands (org-anki-sync-entry org-anki-delete-entry))


(provide 'init-org-drill)

;;; init-org-drill.el ends here
