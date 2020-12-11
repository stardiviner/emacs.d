;;; init-mu4e.el --- init for mu4e.

;;; Commentary:




;;; Code:

(use-package mu4e
  :load-path "~/Code/Emacs/mu/mu4e/"    ; compile from source code
  :demand t
  :commands (mu4e)
  ;; :preface (setq mu4e-mu-debug t)
  :custom ((mail-user-agent 'mu4e-user-agent) ; use mu4e as default for compose [C-x m].
           ;; Maildir
           (mu4e-sent-folder "/Send")
           (mu4e-drafts-folder "/Drafts")
           ;; (mu4e-refile-folder "/Archives")
           (mu4e-trash-folder "/Trash")
           (mu4e-completing-read-function 'completing-read)
           ;; my personal email
           (mu4e-compose-reply-to-address "numbchild@gmail.com")
           (user-mail-address "numbchild@gmail.com")
           (user-full-name  "stardiviner"))
  :bind (:map tools-prefix ("m" . mu4e))
  :init
  (add-to-list 'display-buffer-alist '("^ \\*mu4e-main\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^ \\*mu4e-proc\\*" . (display-buffer-below-selected)))
  ;; support `org-store-link' in mu4e
  (require 'mu4e-org)                   ; for [[mu4e:..]] links.
  (setq mu4e-org-link-query-in-headers-mode t)

  :config
  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq mu4e-maildir-shortcuts
        '((:maildir   "/INBOX"                  :key ?i)
          (:maildir   "/Send"                   :key ?s)
          (:maildir   "/Drafts"                 :key ?d)
          (:maildir   "/Trash"                  :key ?t)
          (:maildir   "/Work"                   :key ?w)
          (:maildir   "/Emacs/help"             :key ?e)
          (:maildir   "/Emacs/Org-mode"         :key ?O)
          (:maildir   "/Lisp/"                  :key ?l)
          (:maildir   "/Clojure"                :key ?c)
          (:maildir   "/ClojureScript"          :key ?C)
          (:maildir   "/Newsletter/Clojure"     :key ?a)
          (:maildir   "/JavaScript"             :key ?j)
          (:maildir   "/SQL/PostgreSQL/general" :key ?p)
          (:maildir   "/NoSQL/Neo4j"            :key ?n)))

  ;; main-view
  (setq mu4e-main-hide-fully-read t)

  ;; better customized unicode symbols for headers
  (setq mu4e-use-fancy-chars t
        ;; email prefix
        mu4e-headers-new-mark '("N" . " ")
        mu4e-headers-unread-mark '("u" . "○") ; · • ∘ ∘ ⋄
        mu4e-headers-seen-mark '("S" . " ")
        mu4e-headers-signed-mark    '("s" . "✍") ; ✩
        mu4e-headers-encrypted-mark '("x" . "✡")
        mu4e-headers-draft-mark '("D" . "✍")
        mu4e-headers-attach-mark '("a" . "▣") ; ◇
        mu4e-headers-passed-mark '("P" . "⇉") ; ❯ (my email in thread)
        mu4e-headers-flagged-mark '("F" . "⚑")
        mu4e-headers-replied-mark '("R" . "↵") ; ↻ ◫
        mu4e-headers-trashed-mark   '("T" . "×")
        ;; thread prefix marks
        mu4e-headers-default-prefix '("|" . "│ ")
        mu4e-headers-has-child-prefix '("+" . "◼ ")     ; "Parent" ╰
        mu4e-headers-empty-parent-prefix '("-" . "◽ ") ; "Orphan"
        mu4e-headers-first-child-prefix '("\\" . "↳ ")
        mu4e-headers-duplicate-prefix '("=" . "≡ "))

  ;; mailing list thread fancy threading characters
  ;; (setq mu4e-headers-thread-child-prefix         '("├>" . "├─➤ ")
  ;;       mu4e-headers-thread-last-child-prefix    '("└>" . "└─➤ ")
  ;;       mu4e-headers-thread-orphan-prefix        '("┬>" . "┬─➤ ")
  ;;       mu4e-headers-thread-single-orphan-prefix '("─>" . "──➤ ")
  ;;       ;; The following two should have the same width.
  ;;       mu4e-headers-thread-connection-prefix    '("│" . "│ ")
  ;;       mu4e-headers-thread-blank-prefix         '(" " . "  "))

  ;; (setq mu4e-headers-thread-child-prefix '("├>" . "├▶ ")
  ;;       mu4e-headers-thread-last-child-prefix '("└>" . "└▶ ")
  ;;       mu4e-headers-thread-connection-prefix '("│" . "│ ")
  ;;       mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
  ;;       mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶ "))

  ;; only show thread subject once
  (setq mu4e-headers-fields
        '((:flags .  6)
          (:human-date  . 12)
          (:from  . 22)
          ;; (:mailing-list  .   10)
          ;; (:thread-subject . 30)
          (:subject . nil)))
  
  ;; [ Get/Update Mail ] -- [C-c C-u]
  ;; program to get mail: alternatives are 'fetchmail', 'getmail',
  ;; 'isync' or your own shellscript.

  ;; disable mu4e auto fetch and update if you use systemd timer unit.
  (unless (string-equal (string-trim
                         (shell-command-to-string
                          "systemctl --user is-enabled getmail.timer"))
                        "enabled")
    (setq mu4e-get-mail-command
          "proxychains getmail --rcfile numbchild@gmail.com --rcfile stardiviner@qq.com"
          mu4e-update-interval (* 60 30)
          mu4e-display-update-status-in-modeline t
          mu4e-hide-index-messages t))

  ;; [ Index ]
  ;; speed-up mu4e (re)index
  (setq mu4e-index-lazy-check t)
  ;; (setq mu4e-index-cleanup nil)
  
  ;; [ Compose ]
  (add-hook 'mu4e-compose-mode-hook #'turn-on-auto-fill)
  (add-hook 'mu4e-compose-mode-hook #'turn-on-flyspell)
  (add-hook 'mu4e-compose-mode-hook #'display-fill-column-indicator-mode)

  ;; Message signature
  (setq mu4e-compose-signature
        "[ stardiviner ]
       I try to make every word tell the meaning that I want to express.

       Blog: https://stardiviner.github.io/
       IRC(freenode): stardiviner, Matrix: stardiviner
       GPG: F09F650D7D674819892591401B5DF1C95AE89AC3")

  (setq mu4e-compose-keep-self-cc t)    ; keep myself on the Cc: list.
  ;; don't include self (that is, any member of `mu4e-user-mail-address-list') in replies.
  (setq mu4e-compose-dont-reply-to-self t)

  (define-key mu4e-headers-mode-map (kbd "r") 'mu4e-compose-reply)
  (define-key mu4e-headers-mode-map (kbd "R") 'mu4e-headers-mark-for-refile)
  (define-key mu4e-view-mode-map (kbd "r") 'mu4e-compose-reply)
  (define-key mu4e-view-mode-map (kbd "R") 'mu4e-view-mark-for-refile)

  (require 'mml)
  (require 'mml2015)
  (require 'epg-config)
  (setq mml2015-use 'epg
        epg-user-id "5AE89AC3"
        mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t)
  ;; auto sign email
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpauto)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-pgpauto)
  ;; (add-hook 'mu4e-compose-mode-hook #'mml-secure-message-sign-pgpmime)
  ;; auto encrypt outgoing message
  ;; (add-hook 'message-send-hook 'mml-secure-message-encrypt-pgpauto)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-encrypt-pgpauto)

  (define-key mu4e-headers-mode-map (kbd "N") 'mu4e-headers-next-unread)

  ;; [ Search ] -- [s/S]  [:references [regexp]] in search query.
  (add-to-list 'mu4e-header-info-custom
               '(:references
                 :name "References"
                 :shortname "References"
                 :help "Reference of this thread"
                 :function (lambda (msg) (format "%s" (mu4e-message-field msg :References)))))

  ;; [ spam filtering ]
  (require 'mu4e-contrib)
  (setq mu4e-register-as-spam-cmd "/usr/bin/bogofilter -Ns < %s"
        mu4e-register-as-ham-cmd "/usr/bin/bogofilter -Sn < %s")
  (add-to-list 'mu4e-headers-actions
               '("SMark as spam" . mu4e-register-msg-as-spam) t)
  (add-to-list 'mu4e-headers-actions
               '("HMark as ham" . mu4e-register-msg-as-ham) t)
  (add-to-list 'mu4e-view-actions
               '("SMark as spam" . mu4e-view-register-msg-as-spam) t)
  (add-to-list 'mu4e-view-actions
               '("HMark as ham" . mu4e-view-register-msg-as-ham) t)
  
  ;; enable Org Mode for editing in `mu4e-compose-mode'.
  (require 'org-mu4e)
  (add-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)

  (add-hook 'mu4e-compose-mode-hook
            (lambda () (define-key org-mode-map (kbd "C-c M-m") 'message-mark-inserted-region)))

  ;; [ HTML Email ]
  ;; use #=begin_export html ... #+end_export
  ;; (setq org-mu4e-convert-to-html t)

  ;; add find relative on same thread search to mu4e actions. [a/A]
  (defun my/mu4e-view-related-search (msg)
    "Search `MSG' for related messages to the current one."
    (let* ((msgid (mu4e-msg-field msg :message-id)))
      (switch-to-buffer "*mu4e-headers*")
      (setq mu4e-headers-include-related t)
      (mu4e-headers-search (concat "\"msgid:" msgid "\""))))
  (add-to-list 'mu4e-headers-actions '("related thread" . my/mu4e-view-related-search) t)
  (add-to-list 'mu4e-view-actions '("relative thread" . my/mu4e-view-related-search) t)

  ;; [ Message view ]
  (setq mu4e-view-fields '(:from :to :cc
                                 :subject
                                 :date
                                 :flags
                                 :maildir
                                 :attachments
                                 ;; :mailing-list
                                 :references
                                 ;; :tags
                                 :signature
                                 :decryption)
        ;; show e-mail address after names of contacts
        ;; From: field. or press [M-RET] to view.
        mu4e-view-show-addresses t)
  (setq mu4e-view-scroll-to-next nil) ; don't open next email when SPC scroll to bottom of message.

  ;; [ Cite ]
  ;; (add-hook 'mu4e-view-mode-hook 'mu4e-view-toggle-hide-cited)
  (setq message-cite-style message-cite-style-gmail)

  (add-hook 'mu4e-view-mode-hook #'turn-on-visual-line-mode)

  ;; [ viewing images inline ]
  (setq mu4e-view-show-images t)

  ;; [ displaying rich-text messages ]
  (add-to-list 'mu4e-view-actions
               '("Browser HTML Message" . mu4e-action-view-in-browser) t)

  ;; Attachments
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))

  ;; Refiling -- [r]
  (setq mu4e-refile-folder              ; dynamic refiling
        (lambda (msg)
          (cond
           ;; mu discuss Google Groups
           ((mu4e-message-contact-field-matches msg
                                                :to "mu-discuss@googlegroups.com")
            "/Emacs/mu")
           ;; delete all Cron getmail error messages which is network
           ;; unavailable error.
           ((string-match "Cron .* getmail.*"
                          (or (mu4e-message-field msg :subject) ""))
            ;; (mu4e-message-contact-field-matches msg :from "Cron Daemon")
            "/Trash")
           ;; everything else goes to /archive
           ;; *important* to have a catch-all at the end!
           (t "/archive"))))

  ;; Bookmarks -- [b]
  (setq mu4e-bookmarks
        `((:name  "Unread messages" :key ?u
                  :query "flag:unread AND NOT flag:trashed")
          (:name "Today's messages" :key ?t
                 :query "date:today..now")
          ;; (:name "Last 7 days" :key ?w
          ;;        :query "date:7d..now"
          ;;        :show-unread t)
          (:name "Messages with images" :key ?p
                 :query "mime:image/*"
                 :hide t)
          ;;=====================================================================
          (:name "My participated threads" :key ?b
                 :query (concat "maildir:\"/Send\" AND flag:unread "
                                " OR contact:/.*stardiviner/ "
                                " OR contact:/.*numbchild@gmail.com/ "))
          (:name "Replied messages" :key ?r
                 :query "flag:replied")
          (:name "Passed  messages" :key ?d
                 :query "flag:passed")
          (:name "Flagged messages" :key ?f
                 :query "flag:flagged")
          (:name "Today's new messages" :key ?n
                 :query "date:today..now flag:new")
          (:name "Today's messages" :key ?d
                 :query "date:today..now")
          (:name "This week's messages" :key ?w
                 :query "date:1w..now")
          (:name "Big messages" :key ?B
                 :query "size:5M..500M")
          (:name "Emacs mailbox" :key ?e
                 :query "maildir:/Emacs/help")
          (:name "Emacs mu4e mailbox" :key ?m
                 :query "maildir:/Emacs/mu")))

  ;; Marking
  (define-key mu4e-headers-mode-map (kbd "f") 'mu4e-headers-mark-for-flag)
  (define-key mu4e-headers-mode-map (kbd "m") 'mu4e-headers-mark-for-something)
  (define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-for-move)

  ;; maintaining an address-book with org-contacts
  ;; Usage: [a o] in headers view & message view :: using the `org-capture' mechanism.
  (require 'init-org-contacts)
  (autoload 'org-contacts-setup-completion-at-point "org-contacts")
  (setq mu4e-org-contacts-file (car org-contacts-files))
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          '(mu4e~compose-complete-contact
                            mail-completion-at-point-function
                            message-completion-function))))
  ;; For mail completion, only consider emails that have been seen in the last 6
  ;; months. This gets rid of legacy mail addresses of people.
  (setq mu4e-compose-complete-only-after
        (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 150))))
  )


;;; [ mu4e-overview ] -- show overview of maildirs.

(use-package mu4e-overview
  :ensure t
  :defer t
  :commands (mu4e-overview)
  :bind (:map tools-prefix ("M-m" . mu4e-overview))
  :custom-face (mu4e-overview-unread ((t (:foreground "lime green"))))
  :config
  ;; auto enable `mu4e-overview' when open `mu4e'. This is working with `mu4e-overview-action' function.
  (defun mu4e-enable-mu4e-overview (&optional args)
    (if (not (mu4e-running-p)) (mu4e))
    (sleep-for 3)
    (mu4e-headers-search "maildir:/INBOX flag:unread")
    ;; (delete-window (get-buffer-window " *mu4e-main*"))
    (mu4e-overview))
  (advice-add 'mu4e :after #'mu4e-enable-mu4e-overview))

;;; [ mu4e-views ] -- View emails in mu4e using xwidget-webkit.

;; (use-package mu4e-views
;;   :ensure t
;;   :after mu4e
;;   :bind (:map mu4e-headers-mode-map
;; 	            ("v" . mu4e-views-mu4e-select-view-msg-method)
;; 	            ("M-n" . mu4e-views-cursor-msg-view-window-down)
;; 	            ("M-p" . mu4e-views-cursor-msg-view-window-up))
;;   :custom ((mu4e-views-completion-method 'ivy)
;;            (mu4e-views-default-view-method "html")
;;            (mu4e-views-next-previous-message-behaviour 'stick-to-current-window))
;;   :init (mu4e-views-mu4e-use-view-msg-method "html"))



(provide 'init-mu4e)

;;; init-mu4e.el ends here
