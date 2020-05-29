;;; init-mu4e.el --- init for mu4e.

;;; Commentary:




;;; Code:

(use-package mu4e
  :load-path "~/Code/Emacs/mu/mu4e/"    ; compile from source code
  :defer t
  :commands (mu4e mu4e-org-open mu4e-org-store-link)
  :custom ((mail-user-agent 'mu4e-user-agent) ; use mu4e as default for compose [C-x m].
           (mu4e-completing-read-function 'completing-read)
           (message-send-mail-function 'message-send-mail-with-sendmail)
           ;; my personal email
           (mu4e-compose-reply-to-address "numbchild@gmail.com")
           (user-mail-address "numbchild@gmail.com")
           (user-full-name  "stardiviner"))
  :preface (if (fboundp 'org-link-set-parameters)
               (org-link-set-parameters "mu4e"
                                        :follow #'mu4e-org-open
                                        :store  #'mu4e-org-store-link)
             (org-add-link-type "mu4e" 'mu4e-org-open)
             (add-hook 'org-store-link-functions 'mu4e-org-store-link))
  :bind (:map tools-prefix ("m" . mu4e))
  :init
  ;; [ View ]
  (setq mu4e-use-fancy-chars t
        ;; email prefix
        mu4e-headers-new-mark '("N" . " ")
        mu4e-headers-unread-mark '("u" . "∘") ; · • ∘ ⋄
        mu4e-headers-seen-mark '("S" . " ")
        mu4e-headers-signed-mark '("s" . "✩")
        mu4e-headers-encrypted-mark '("x" . "✡")
        mu4e-headers-draft-mark '("D" . "✍")
        mu4e-headers-attach-mark '("a" . "▣")
        mu4e-headers-passed-mark '("P" . "❯") ; my email in thread.
        mu4e-headers-flagged-mark '("F" . "⚑")
        mu4e-headers-replied-mark '("R" . "◫")
        mu4e-headers-trashed-mark '("T" . "↻")
        ;; thread prefix marks
        mu4e-headers-default-prefix '("|" . "│ ")
        mu4e-headers-has-child-prefix '("+" . "◼ ")     ; "Parent" ╰
        mu4e-headers-empty-parent-prefix '("-" . "◽ ") ; "Orphan"
        mu4e-headers-first-child-prefix '("\\" . "↳ ")
        mu4e-headers-duplicate-prefix '("=" . "≡ "))

  ;; only show thread subject once
  (setq mu4e-headers-fields
        '((:flags .  6)
          (:human-date  . 12)
          (:from  . 22)
          ;; (:mailing-list  .   10)
          ;; (:thread-subject . 30)
          (:subject . nil)))

  :config
  ;; [ Maildir ]
  (setq mu4e-sent-folder "/Send"
        mu4e-drafts-folder "/Drafts"
        ;; mu4e-refile-folder "/Archives"
        mu4e-trash-folder "/Trash")

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
          (:maildir   "/JavaScript"             :key ?j)
          (:maildir   "/SQL/PostgreSQL/general" :key ?p)))

  ;; Get Mail, Update -- [U]
  ;; program to get mail; alternatives are 'fetchmail', 'getmail'
  ;; isync or your own shellscript.
  (setq mu4e-get-mail-command
        "getmail --rcfile numbchild@gmail.com --rcfile stardiviner@qq.com"
        ;; mu4e-update-interval 1800
        mu4e-hide-index-messages t)

  ;; Auto Update Database [C-c C-u]
  (setq mu4e-update-interval (* 60 10))
  
  ;; [ Compose ]
  ;; (add-hook 'mu4e-compose-mode-hook #'visual-fill-column-mode)
  (add-hook 'mu4e-compose-mode-hook #'turn-on-auto-fill)
  (add-hook 'mu4e-compose-mode-hook #'turn-on-flyspell)
  ;; enable `company-ispell' backend in `mu4e-compose-mode'.
  (defun mu4e-enable-company-ispell ()
    "Enable company-ispell backend in company-backends for mu4e-compose-mode."
    (add-to-list 'company-backends 'company-ispell 'append))
  (add-hook 'mu4e-compose-mode-hook #'mu4e-enable-company-ispell)

  ;; Message signatures
  (setq mu4e-compose-signature
        "[ stardiviner ]
       I try to make every word tell the meaning that I want to express.

       Blog: https://stardiviner.github.io/
       IRC(freenode): stardiviner, Matrix: stardiviner
       GPG: F09F650D7D674819892591401B5DF1C95AE89AC3
      ")

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
  (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-pgpauto)
  ;; auto encrypt outgoing message
  ;; (add-hook 'message-send-hook 'mml-secure-message-encrypt-pgpauto)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-encrypt-pgpauto)

  (define-key mu4e-headers-mode-map (kbd "N") 'mu4e-headers-next-unread)

  ;; [ Search ] -- [s/S]  [:references [regexp]] in search query.
  (add-to-list 'mu4e-header-info-custom
               '(:references :name "References"
                             :shortname "References"
                             :help "Reference of this thread"
                             :function
                             (lambda (msg)
                               (format "%s" (mu4e-message-field msg :References)))))

  ;; (add-to-list 'mu4e-header-info-custom
  ;;              '(:reply2mythread :name "My thread smart choose depend on References"
  ;;                                :shortname "My Thread"
  ;;                                :help "Emails which reply to the thread I created"
  ;;                                :function
  ;;                                (lambda
  ;;                                  (msg)
  ;;                                  (string-match-p ".*@[HOSTNAME]"
  ;;                                                  (format "%s"
  ;;                                                          (mu4e-message-field msg :References)))
  ;;                                  )))

  ;; `mu4e-headers-custom-markers'

  ;; example
  ;; (add-to-list 'mu4e-headers-custom-markers
  ;;              '("More than n recipients"
  ;;                (lambda (msg n)
  ;;                  (> (+ (length (mu4e-message-field msg :to))
  ;;                        (length (mu4e-message-field msg :cc))) n))
  ;;                (lambda ()
  ;;                  (read-number "Match messages with more recipients than: ")))
  ;;              t)

  ;; reply to my thread
  (add-to-list 'mu4e-headers-custom-markers
               '("Reply to my thread"
                 (lambda (msg reply2mythread)
                   (string-match-p ".*stardiviner"
                                   (mu4e-msg-field msg :References)))
                 (lambda nil
                   (message "Messages replied to your thread.")))
               t)

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

  ;; `org-store-link' in mu4e
  (require 'org-mu4e)                   ; for [[mu4e:..]] links.
  (setq mu4e-org-link-query-in-headers-mode t)
  ;; enable Org Mode for editing in `mu4e-compose-mode'.
  (add-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c M-m") 'message-mark-inserted-region)))

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
        ;; show e-mail address after names of contacts From: field. or press [M-RET] to view.
        mu4e-view-show-addresses t)
  (setq mu4e-view-scroll-to-next nil) ; don't open next email when SPC scroll to bottom of message.

  ;; [ Cite ]
  ;; (add-hook 'mu4e-view-mode-hook 'mu4e-view-toggle-hide-cited)
  (setq message-cite-style message-cite-style-gmail)

  ;; [ viewing images inline ]
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; [ displaying rich-text messages ]
  (add-to-list 'mu4e-view-actions
               '("Browser HTML Message" . mu4e-action-view-in-browser) t)

  ;; Attachments [C-u] + [e]
  ;;
  ;; - A -- action to pick some custom action to perform on current message/attachment.
  ;; - w -- open-with
  ;; - | -- pipe
  ;; - e -- open in emacs
  ;; By default, mu4e uses the xdg-open-program 1 or (on MacOS) the open program
  ;; for opening attachments. If you want to use another program, you do so by
  ;; setting the MU_PLAY_PROGRAM environment variable to the program to be used.
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))
  ;; (add-to-list 'mu4e-view-attachment-actions '("bbrowse-with-browser" . mu4e-view-browse-with-browser))

  ;; Actions
  ;;
  ;; - a -- (for messages)
  ;; - A -- mu4e-headers-action (for attachments)
  ;;
  ;; mu4e lets you define custom actions for messages in the Headers view and for
  ;; both messages and attachments in the Message view. Custom actions allow you
  ;; to easily extend mu4e for specific needs – for example, marking messages as
  ;; spam in a spam filter or applying an attachment with a source code patch.

  ;; (setq mu4e-headers-actions
  ;;       '(("capture message" . mu4e-action-capture-message)
  ;;         )
  ;;       )
  ;; (setq mu4e-view-actions
  ;;       '(("capture message" . mu4e-action-capture-message)
  ;;         ("view as pdf" . mu4e-action-view-as-pdf)))
  ;; (setq mu4e-view-attachment-actions
  ;;       '(("wopen-with" . mu4e-view-open-attachment-with)
  ;;         ("ein-emacs" . mu4e-view-open-attachment-emacs)
  ;;         ("|pipe" . mu4e-view-pipe-attachment)))

  ;; adding an action to actions list.
  (defun show-number-of-recipients (msg)
    "Display the number of recipients for the MSG at point."
    (message "Number of recipients: %d"
             (+ (length (mu4e-message-field msg :to))
                (length (mu4e-message-field msg :cc)))))
  ;; (add-to-list 'mu4e-headers-actions
  ;;              '("Number of recipients" . show-number-of-recipients) t)
  ;; search for messages by the sender of the message at point:
  (defun search-for-sender (msg)
    "Search for messages sent by the sender of the MSG at point."
    (mu4e-headers-search
     (concat "from:" (cdar (mu4e-message-field msg :from)))))
  ;; define 'x' as the shortcut
  ;; (add-to-list 'mu4e-view-actions
  ;;              '("xsearch for sender" . search-for-sender) t)

  ;; Refiling -- [r] -- refiling, mu4e-refile-folder
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

  ;; Bookmarks
  ;;
  ;; - [b] :: bookmark
  ;; - [B] :: edit bookmark before jump/invoking.
  ;;          mu4e-headers-search-bookmark-edit, which lets you edit the bookmarked query before invoking it.
  ;;
  ;; - check out following variables and functions source code to find out search query keywords you want.
  ;; - `mu4e-headers-visible-flags'
  ;; - `mu4e~headers-field-apply-basic-properties'
  ;; - `$ man mu-query'

  (setq mu4e-bookmarks
        '((:name  "Unread messages" :key ?u
                  :query "flag:unread AND NOT flag:trashed")
          (:name "Today's messages" :key ?t
                 :query "date:today..now")
          (:name "Last 7 days" :key ?w
                 :query "date:7d..now"
                 :show-unread t)
          (:name "Messages with images" :key ?p
                 :query "mime:image/*"
                 :hide t)
          ;;=====================================================================
          (:name "My participated Threads" :key ?t
                 :query "flag:unread AND contact:/.*stardiviner/ OR contact:/.*numbchild@gmail.com/")
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
          (:name "Org Mode mailbox" :key ?o
                 :query "maildir:/Emacs/Org-mode")
          (:name "Clojure mailbox" :key ?c
                 :query "maildir:/Clojure")
          (:name "ClojureScript mailbox" :key ?C
                 :query "maildir:/ClojureScript")
          (:name "PostgreSQL general" :key ?p
                 :query "/SQL/PostgreSQL/general")))

  (defun mu4e-new-mail-alert ()
    "The mu4e new email alert."
    (make-process
     :name "mu4e new mail alert 1"
     :command (list "mpv" (expand-file-name "~/Music/Sounds/Ingress/SFX/sfx_sonar.wav")))
    (make-process
     :name "mu4e new mail alert 2"
     :command (list "mpv" (expand-file-name "~/Music/Sounds/Ingress/Speech/speech_zoom_lockon.wav"))))
  ;; (add-hook 'mu4e-index-updated-hook 'mu4e-new-mail-alert)

  (defun mu4e-open-mail-sound ()
    "The mu4e open email sound."
    (make-process
     :name "mu4e open mail sound"
     :command (list "mpv" (expand-file-name "~/Music/Sounds/Ingress/SFX/sfx_sonar.wav"))))
  ;; (add-hook 'mu4e-view-mode-hook 'mu4e-open-mail-sound 'append)

  ;; (add-hook 'mu4e-headers-found-hook 'mu4e-open-mail-sound 'append)
  ;; (add-hook 'mu4e-index-updated-hook 'mu4e-open-mail-sound 'append)

  ;; Marking
  (define-key mu4e-headers-mode-map (kbd "f") 'mu4e-headers-mark-for-flag)
  (define-key mu4e-headers-mode-map (kbd "m") 'mu4e-headers-mark-for-something)
  (define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-for-move)

  (add-hook 'mu4e-view-mode-hook #'turn-on-visual-line-mode)

  ;; [ mu4e-speedbar ]
  ;; (add-hook 'mu4e-main-mode-hook 'sr-speedbar-open)


  ;; maintaining an address-book with org-contacts
  ;; Usage: <a o> in headers view & message view :: using the org-capture mechanism.
  (require 'init-org-contacts)
  (autoload 'org-contacts-setup-completion-at-point "org-contacts")
  (setq mu4e-org-contacts-file (car org-contacts-files))
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-hook 'mu4e-compose-mode-hook
            (lambda () (setq-local completion-at-point-functions
                              '(mu4e~compose-complete-contact
                                mail-completion-at-point-function
                                message-completion-function)))))

;;; [ mu4e-overview ] -- show overview of maildirs.

(use-package mu4e-overview
  :ensure t
  :defer t
  :commands (mu4e-overview)
  :bind (:map tools-prefix ("M-m" . mu4e-overview))
  :config
  ;; auto enable `mu4e-overview' when open `mu4e'. This is working with `mu4e-overview-action' function.
  (defun mu4e-enable-mu4e-overview (&optional args)
    (if (not (mu4e-running-p)) (mu4e))
    (sleep-for 3)
    (mu4e-headers-search "maildir:/INBOX flag:unread")
    ;; (delete-window (get-buffer-window " *mu4e-main*"))
    (mu4e-overview))

  (advice-add 'mu4e :after #'mu4e-enable-mu4e-overview)

  (set-face-attribute 'mu4e-overview-unread nil
                      :foreground "lime green"))



(provide 'init-mu4e)

;;; init-mu4e.el ends here
