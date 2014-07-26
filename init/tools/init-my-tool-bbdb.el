;;; init-my-tool-bbdb.el --- init BBDB
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Help:
;; http://savannah.nongnu.org/projects/bbdb/
;; http://www.mail-archive.com/bbdb-info@lists.sourceforge.net/
;; http://www.emacswiki.org/cgi-bin/wiki/CategoryBbdb


;;; Code:

;;; [ BBDB ] -- The Insidious Big Brother Database (BBDB) is a contact management utility.

;;; Usage:
;; - [M-x bbdb-help]
;; - [M-x bbdb-info]
;; - [M-x bbdb] :: start BBDB
;; - [M-x bbdb-merge-file] :: merge databases
;;        If you want to merge two very different databases (one from your girlfriend, one from yourself), your best bet is to just concatenate the two. Then start the BBDB, and use M-x bbdb-show-duplicates. For every record that appears twice, put point on the second record, and use ‘r’ to merge it into the first record.
;; - Export
;;   http://www.emacswiki.org/emacs/BbdbExporters
;; - Import
;;   http://www.emacswiki.org/emacs/BbdbImporters

;;; TODO: BBDBv3

(require 'bbdb)
;; (bbdb-initialize)
(bbdb-initialize 'message 'sendmail 'supercite 'w3 'gnus)

;; If t then BBDB will not modify `bbdb-file'.
;; If you have more than one Emacs running at the same time, you might want to
;; set this to t in all but one of them.
;; (setq bbdb-read-only t )

(unless (boundp 'my-bbdb-prefix-map)
  (define-prefix-command 'my-bbdb-prefix-map))
(define-key my-tools-prefix-map (kbd "b") 'my-bbdb-prefix-map)

(define-key my-bbdb-prefix-map (kbd "b")
  (lambda ()
    (interactive)
    (my-func/open-and-switch-to-buffer 'bbdb "*BBDB*")))

(define-key my-bbdb-prefix-map (kbd "c") 'bbdb-create)
(define-key my-bbdb-prefix-map (kbd "a") 'bbdb-snarf) ; usage: region select name and email part in To: field. then press this keybinding.
(define-key my-bbdb-prefix-map (kbd "h") 'helm-bbdb)

(setq bbdb-file (expand-file-name "~/Org/BBDB/bbdb")
      bbdb-completion-display-record t
      bbdb-image t ; display records with an image.
      bbdb-image-path (expand-file-name "~/Org/BBDB/avatars/")
      ;; bbdb-image-suffixes '(".png" ".jpg" ".gif" ".xpm")
      ;; bbdb-sound-files
      ;; bbdb-xfields-sort-order '((notes . 0)
      ;;                           (url . 1)
      ;;                           (ftp . 2)
      ;;                           (gopher . 3)
      ;;                           (telnet . 4)
      ;;                           (mail-alias . 5)
      ;;                           (mail-folder . 6)
      ;;                           (lpr . 7)
      ;;                           (creation-date . 1000)
      ;;                           (timestamp . 1001))
      ;; bbdb-default-xfield 'notes
      ;; bbdb-xfield-label-list
      ;; bbdb-merge-xfield-function-alist '((creation-date . bbdb-merge-string-least)
      ;;                                    (timestamp . bbdb-merge-string-most))
      bbdb-default-label-list '("personal" "home" "work" "company" "organization" "other")
      bbdb-default-country "China"
      bbdb-dial-local-prefix "+86" ; TODO: is this right?
      bbdb-default-area-code "+86"
      ;; bbdb-init-forms
      ;; bbdb-mua-pop-up-window-size
      ;; bbdb-auto-notes-rules
      ;; bbdb-auto-notes-rules-expanded
      ;; bbdb-snarf-default-label-alist '((phone . "work") (address . "work"))
      ;; bbdb-address-format-list
      bbdb-add-mails 'query
      ;; bbdb-time-stamp-format "%Y-%m-%d %r %Z"
      ;; bbdb-address-label-list
      bbdb-need-to-sort t
      ;; bbdb-case-fold-search nil
      bbdb-completion-list t
      bbdb-complete-mail t
      bbdb-completion-display-record t
      bbdb-new-mails-primary 'query
      )


(dolist (hook '(message-setup-hook
                mu4e-compose-mode-hook
                ))
  (add-hook hook 'bbdb-mail-aliases))

;;; BBDB [M-TAB] conflicts with ispell.
(defun my-enable-bbdb-complete-key ()
  (define-key message-mode-map (kbd "M-TAB") 'bbdb-complete-name))
(dolist (hook '(message-mode-hook
                mu4e-compose-mode-hook
                ))
  (add-hook hook 'my-enable-bbdb-complete-key))

;;; [ bbdb- ] -- More easily search/choice than BBDB


;;; [ bbdb-vcard ]

;; (require 'bbdb-vcard)


;;; helm-bbdb.el




(provide 'init-my-tool-bbdb)

;;; init-my-tool-bbdb.el ends here
