;;; init-gnus.el --- init for Gnus
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Gnus ]

;; user info
(setq user-full-name "stardiviner"
      user-mail-address "numbchild@gmail.com")

(use-package gnus
  :ensure t)

;; directories & files
(setq gnus-home-directory (expand-file-name "Gnus/" user-emacs-directory)
      gnus-default-directory gnus-home-directory
      gnus-init-file (nnheader-concat gnus-home-directory ".gnus")
      gnus-startup-file (nnheader-concat gnus-home-directory ".newsrc")
      ;; News
      gnus-directory (nnheader-concat gnus-home-directory "News/")
      gnus-article-save-directory (nnheader-concat gnus-home-directory "News/")
      gnus-kill-files-directory (nnheader-concat gnus-home-directory "News/trash")
      ;; gnus-agent-directory
      gnus-cache-directory (nnheader-concat gnus-home-directory "News/trash")
      ;; Mail
      message-directory (nnheader-concat gnus-home-directory "Email")
      message-auto-save-directory (expand-file-name "drafts/" message-directory)
      mail-source-directory (expand-file-name "incoming/" message-directory)
      nnmail-message-id-cache-file (nnheader-concat gnus-home-directory "Email/.nnmail-cache")
      mm-default-directory "~/Downloads/" ; attachment save dir.
      )

;; interface
(setq gnus-inhibit-startup-message t
      gnus-show-threads t
      gnus-interactive-exit t
      gnus-interactive-catchup t
      gnus-asynchronous t
      gnus-summary-ignore-duplicates t
      ;; gnus-treat-fill-long-lines t

      ;; group buffer
      ;; gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%g%)\n"
      ;; gnus-group-highlight

      ;; topic
      ;; gnus-topic-line-format "%i[ %(%{%n%}%) -- %A ]%v\n"

      ;; image
      ;; Gravatatrs
      gnus-treat-mail-gravatar 'head
      )

;; startup
;; (setq gnus-use-backend-marks nil)

;; auto save
;; disable dribble file
(setq gnus-use-dribble-file nil
      gnus-always-read-dribble-file nil
      gnus-dribble-directory nil
      )

;; the active file
(setq gnus-read-active-file 'some
      gnus-cache-active-file (expand-file-name "active" gnus-cache-directory)
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

(setq gnus-refer-article-method 'current)

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
      mm-inline-large-images t
      mm-inline-text-html-with-images t)
(auto-image-file-mode) ; auto load image file
(add-to-list 'mm-attachment-override-types "image/*") ; attachment display image

;; newsgroup grouped by.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; group score
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)

;; auto jump to first unread newsgroup.
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group)

;; notify
(require 'gnus-notifications)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

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


;; [ C-c C-y]
(with-eval-after-load 'gnus-group
  (defhydra hydra-gnus-group (:color blue)
    "Do?"
    ("a" gnus-group-list-active "REMOTE groups A A")
    ("l" gnus-group-list-all-groups "LOCAL groups L")
    ("c" gnus-topic-catchup-articles "Read all c")
    ("G" gnus-group-make-nnir-group "Search server G G")
    ("g" gnus-group-get-new-news "Refresh g")
    ("s" gnus-group-enter-server-mode "Servers")
    ("m" gnus-group-new-mail "Compose m OR C-x m")
    ("#" gnus-topic-mark-topic "mark #")
    ("q" nil "cancel"))
  ;; y is not used by default
  (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)
  )

(with-eval-after-load 'gnus-sum
  (defhydra hydra-gnus-summary (:color blue)
    "Do?"
    ("n" gnus-summary-insert-new-articles "Refresh / N")
    ("f" gnus-summary-mail-forward "Forward C-c C-f")
    ("!" gnus-summary-tick-article-forward "Mail -> disk !")
    ("p" gnus-summary-put-mark-as-read "Mail <- disk")
    ("c" gnus-summary-catchup-and-exit "Read all c")
    ("e" gnus-summary-resend-message-edit "Resend S D e")
    ("R" gnus-summary-reply-with-original "Reply with original R")
    ("r" gnus-summary-reply "Reply r")
    ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
    ("w" gnus-summary-wide-reply "Reply all S w")
    ("#" gnus-topic-mark-topic "mark #")
    ("q" nil "cancel"))
  ;; y is not used by default
  (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)
  )

(with-eval-after-load 'gnus-art
  (defhydra hydra-gnus-article (:color blue)
    "Do?"
    ("f" gnus-summary-mail-forward "Forward")
    ("R" gnus-article-reply-with-original "Reply with original R")
    ("r" gnus-article-reply "Reply r")
    ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
    ("o" gnus-mime-save-part "Save attachment at point o")
    ("w" gnus-article-wide-reply "Reply all S w")
    ("q" nil "cancel"))
  ;; y is not used by default
  (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)
  )

(with-eval-after-load 'message
  (defhydra hydra-message (:color blue)
    "Do?"
    ("ca" mml-attach-file "Attach C-c C-a")
    ("cc" message-send-and-exit "Send C-c C-c")
    ("q" nil "cancel"))
  (define-key message-mode-map (kbd "C-c C-y") 'hydra-message/body)
  )


;;; [ Mail ]

(setq message-confirm-send t
      message-kill-buffer-on-exit t
      message-from-style 'angles
      ;; message-syntax-checks '((sender . disabled))
      nnmail-expiry-wait 'never
      mail-source-delete-incoming t
      )

;; (add-to-list 'gnus-select-method
;;              '(nnmaildir "Mails"
;;                          (directory "~/Mails/")))

;; (add-to-list 'gnus-select-method
;;              '(maildir :path "~/Mails/"
;;                        :subdirs ("cur" "new" "tmp")))

;; (setq mail-sources '((file)))
;; (setq mail-sources '((maildir :path "~/Mails/"
;;                               :subdirs ("cur" "new" "tmp"))))


;; [ Gmail ]

(setq user-mail-address "numbchild@gmail.com"
      mml2015-signers '("5AE89AC3")
      ;; This tells Gnus to get email from Gmail via IMAP.
      gnus-select-method
      '(nnimap "gmail"
               ;; It could also be imap.googlemail.com if that's your server.
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)

               (nnir-search-engine imap)
               
               ;; press 'E' to expire email
               (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait 90)
               )
      ;; This tells Gnus to use the Gmail SMTP server. This
      ;; automatically leaves a copy in the Gmail Sent folder.
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; Tell message mode to use SMTP.
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'smtpmail-send-it
      ;; This is where we store the password.
      nntp-authinfo-file "~/.authinfo.gpg"
      ;; Gmail system labels have the prefix [Gmail], which matches
      ;; the default value of gnus-ignored-newsgroups. That's why we
      ;; redefine it.
      ;; gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      ;; The agent seems to confuse nnimap, therefore we'll disable it.
      gnus-agent nil
      ;; We don't want local, unencrypted copies of emails we write.
      gnus-message-archive-group nil
      ;; We want to be able to read the emails we wrote.
      mml2015-encrypt-to-self t)

;; Attempt to encrypt all the mails we'll be sending.
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; Add two key bindings for your Gmail experience.
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'gmail-archive)
  (local-set-key "$" 'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

;; auto linebreaking
(defun my-message-mode-setup ()
  (setq fill-column 80)
  (turn-on-auto-fill))

(add-hook 'message-mode-hook 'my-message-mode-setup)


;; envelope when sending email
(setq mail-specify-envelope-from t
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
;; thread sort
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

;; search mail with various search engines
(require 'nnir)


;;; GnuPG Agent

(defun gpg-agent-restart ()
  "This kills and restarts the gpg-agent.

To kill gpg-agent, we use killall. If you know that the agent is
OK, you should just reload the environment file using
`gpg-agent-reload-info'."
  (interactive)
  (shell-command "killall gpg-agent")
  (shell-command "gpg-agent --daemon --enable-ssh-support --write-env-file")
  ;; read the environment file instead of parsing the output
  (gpg-agent-reload-info))

(defun gpg-agent-reload-info ()
  "Reload the ~/.gpg-agent-info file."
  (interactive)
  (with-temp-buffer
    (insert-file (expand-file-name "~/.gpg-agent-info"))
    (goto-char (point-min))
    (while (re-search-forward "\\([A-Z_]+\\)=\\(.*\\)" nil t)
      (setenv (match-string 1) (match-string 2)))))

(defun gpg-agent-startup ()
  "Initialize the gpg-agent if necessary.

Note that sometimes the gpg-agent can be up and running and still
be useless, in which case you should restart it using
`gpg-agent-restart'."
  (gpg-agent-reload-info)
  (unless (member (string-to-number (getenv "SSH_AGENT_PID"))
                  (list-system-processes))
    (gpg-agent-restart)))

(gpg-agent-startup)


;;; [ Newsgroup ]

(setq gnus-novice-user t
      gnus-expert-user nil
      gnus-large-newsgroup 150
      gnus-large-ephemeral-newsgroup nil
      gnus-check-new-newsgroups 'ask-server
      )

;; (setq gnus-nntpserver-file "/etc/nntpserver")

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nntp "news.gwene.org"))
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nntp "news.gmane.org"))


;;; [ RSS ]

(require 'nnrss)

(setq nnrss-directory (nnheader-concat gnus-home-directory "RSS")
      nnrss-file-coding-system 'utf-8-emacs
      nnrss-use-local t
      )

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

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnrss "RSS"))


;;; [ Chinese ]

;; (setq gnus-default-charset 'cn-gb-2312
;;       gnus-group-name-charset-group-alist (quote ((".*" . gb2312)))
;;       gnus-group-posting-charset-alist
;;       '(("^\\(cn\\)\\.[^,]*\\(,[ 	\n]*\\(cn\\)\\.[^,]*\\)*$" gb2312 (gb2312))
;;         (message-this-is-mail nil nil)
;;         (message-this-is-news nil t))
;;       )


(provide 'init-gnus)

;;; init-gnus.el ends here
