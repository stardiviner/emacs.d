;;; init-my-tool-email-gnus.el --- init for Gnus
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Gnus ]

(require 'gnus)

;; files
(setq gnus-init-file "~/.emacs.d/Gnus/.gnus"
      gnus-startup-file "~/.emacs.d/Gnus/.newsrc")

;; directories
(setq gnus-default-directory "~/.emacs.d/Gnus/"
      gnus-home-directory "~/.emacs.d/Gnus/"
      gnus-dribble-directory "~/.emacs.d/Gnus/"
      ;; News
      gnus-directory "~/.emacs.d/Gnus/News/"
      gnus-article-save-directory "~/.emacs.d/Gnus/News/"
      gnus-kill-files-directory "~/.emacs.d/Gnus/News/trash/"
      ;; gnus-agent-directory
      gnus-cache-directory "~/.emacs.d/Gnus/News/cache/"
      gnus-cache-active-file (expand-file-name "active" gnus-cache-directory)
      ;; Mail
      message-directory "~/.emacs.d/Gnus/Mail/"
      message-auto-save-directory "~/.emacs.d/Gnus/Mail/drafts/"
      mail-source-directory "~/.emacs.d/Gnus/Mail/incoming/"
      nnmail-message-id-cache-file "~/.emacs.d/Gnus/.nnmail-cache"
      ;; nnmail-newsgroups-file "~/.emacs.d/Gnus/Mail/newsgroup"
      ;; nntp-marks-directory "~/.emacs.d/Gnus/News/marks"
      mm-default-directory "~/Downloads/" ; attachment save dir.
      )

;; user info
(setq user-full-name "stardiviner"
      user-mail-address "numbchild@gmail.com")

;; interface
(setq gnus-inhibit-startup-message t
      gnus-show-threads t
      gnus-interactive-exit t
      gnus-asynchronous t
      gnus-summary-ignore-duplicates t
      ;; gnus-treat-fill-long-lines t
      )

;; start
;; disable dribble file
(setq gnus-use-dribble-file nil
      gnus-always-read-dribble-file nil
      )

;; windows layout
(gnus-add-configuration
 '(article
   (vertical 1.0
             (summary .35 point)
             (article 1.0))))

;; visual thread `%B'
(setq gnus-summary-same-subject ""
      gnus-sum-thread-tree-indent "    "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-false-root "☆"
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf "╰─► "
      )

;; summary
(setq gnus-summary-gather-subject-limit 'fuzzy)
(setq gnus-summary-line-format "%4P %U%R%z%O %{%5k%} %{%14&user-date class="comment">;%}   %{%-20,20n%} %{%ua%} %B %(%I%-60,60s%)\n")
;; user format function `%ua'
(defun gnus-user-format-function-a (header)
  (let ((myself (concat "<" my-mail ">"))
        (references (mail-header-references header))
        (message-id (mail-header-id header)))
    (if (or (and (stringp references)
                 (string-match myself references))
            (and (stringp message-id)
                 (string-match myself message-id)))
        "X" "│")))
;; stripe lines
(setq gnus-summary-stripe-regexp
      (concat "^[^"
              gnus-sum-thread-tree-vertical
              "]*"))

;; date format
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "TD %H:%M")   ; today
        (604800 . "W%w %H:%M")                ; in week
        ((gnus-seconds-month) . "%d %H:%M")   ; in month
        ((gnus-seconds-year) . "%m-%d %H:%M") ; in year
        (t . "%y-%m-%d %H:%M")))              ; others

;; time
;; convert send mail time to local time.
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
;; trace thread's timeline.
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; display
(setq mm-text-html-renderer 'gnus-w3m
      mm-inline-large-images t)
(auto-image-file-mode) ; auto load image file
(add-to-list 'mm-attachment-override-types "image/*") ; attachment display image

;; newsgroup grouped by.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; auto jump to first unread newsgroup.
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group)

;; notify
(require 'gnus-notifications)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

;; NOTE: deprecated
;; (add-hook 'gnus-summary-exit-hook 'gnus-notify+)
;; (add-hook 'gnus-group-catchup-group-hook 'gnus-notify+)
;; (add-hook 'mail-notify-pre-hook 'gnus-notify+)

;; retrieve email & news
(setq gnus-nntp-server nil
      ;; gnus-refer-article-method 'current
      gnus-select-method '(nnmaildir "Mails" (directory "~/Mails/"))
      gnus-secondary-select-methods
      '(
        ;; Mail
        ;; read mails from local, fetch new mail with `getmail'.
        (nnmaildir "Mails"
                   (directory "~/Mails/"))
        ;; NNTP newsgroup
        ;; (nntp "localhost")
        
        ;; RSS
        )
      )

;; archive
(setq gnus-message-archive-group
      '((if (message-news-p)
            ;; News archive
            "nnfolder+archive:nnfolder"
          ;; Mail archive
          "nnmaildir+Mail:INBOX"
          )))

;; agent
;; enable agent for offline viewing
(gnus-agentize)

;; last set
;; (setq gnus-use-byte-compile t) ; compile to speed-up


;;; [ Mail ]

(setq message-confirm-send t
      message-kill-buffer-on-exit t
      message-from-style 'angles
      ;; message-syntax-checks '((sender . disabled))
      nnmail-expiry-wait 'never
      )

;; (setq mail-sources '((file)))
;; (setq mail-sources '((maildir :path "~/Mails/"
;;                               :subdirs ("cur" "new" "tmp"))))


;; [ Gmail ]

;; <Gmail - 1>
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "Gmail" ; key
;;                       (nnimap-address "imap.gmail.com")
;;                       (nnimap-server-port 993)
;;                       (nnimap-stream ssl)))
;; (setq smtpmail-smtp-service 587
;;       gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
;;       )

;; <Gmail - 2>
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "gmail"
;;                       (nnimap-address "imap.gmail.com")
;;                       (nnimap-server-port 993)
;;                       (nnimap-stream ssl)
;;                       (nnir-search-engine imap)
;;                       ;; press 'E' to expire email
;;                       (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
;;                       (nnmail-expiry-wait 90)))


;; (with-eval-after-load 'gnus-topic
;;   (setq gnus-topic-topology '(("Gnus" visible)
;;                               (("Gmail" visible nil nil))
;;                               (("QQ mail" visible nil nil))
;;                               ))
;;   (setq gnus-topic-alist '(("Gmail" ; the key of topic
;;                             "INBOX"
;;                             "[Gmail]/Sent Mail"
;;                             "[Gmail]/Trash"
;;                             "Drafts")
;;                            ("Gnus"))))

;; send email
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it
      ;; sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      )

;; mail visible headers
(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic")
                 "\\|"))

;; use `supercite' display multiple quote content styles.
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)

;; thread settings
(setq gnus-use-trees t
      gnus-tree-minimize-window nil
      gnus-fetch-old-headers 'some
      gnus-generate-tree-function 'gnus-generate-horizontal-tree
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      )
;; sort
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

;; search mail with various search engines
(require 'nnir)


;;; [ Newsgroup ]

(setq gnus-novice-user t
      gnus-expert-user nil
      gnus-large-newsgroup 150
      gnus-large-ephemeral-newsgroup nil
      )

;; (add-to-list 'gnus-secondary-select-methods
;;              ;; NNTP GMANE newsgroup
;;              '(nntp "news.gmane.org")
;;              )


;;; [ RSS ]

(require 'nnrss)

(with-eval-after-load "gnus-sum"
  (add-to-list 'gnus-newsgroup-variables
               '(mm-w3m-safe-url-regexp . "\\`cid:"))
  (add-to-list 'gnus-newsgroup-variables
               '(mm-inline-text-html-with-images . nil))
  )

;; Display images inline in `nnrss' groups.
(add-to-list 'gnus-parameters
             '("\\`nnrss:"
               (mm-inline-text-html-with-images t)
               (mm-w3m-safe-url-regexp nil))
             '("\\`nnrss:"
               (mm-discouraged-alternatives nil)))




;; keybindings
(define-key my-email-prefix (kbd "g") 'gnus)


(provide 'init-my-tool-email-gnus)

;;; init-my-tool-email-gnus.el ends here
