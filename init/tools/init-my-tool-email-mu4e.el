;;; init-my-tool-email-mu4e.el --- an e-mail client for Emacs
;;
;;; Commentary:

;; # ------ concepts ------
;; #
;; #       +---------+
;; #       | emacs   |
;; #       |    +------+
;; #       +----| mu4e | --> send mail (smtpmail)
;; #            +------+
;; #            |  A
;; #            V  |  ---/ search, view, move mail
;; #       +---------+    \
;; #       |   mu    |
;; #       +---------+
;; #         |    A
;; #         V    |
;; #       +---------+
;; #       | Maildir |  <--- receive mail (fetchmail,
;; #                 +---------+                     offlineimap, ...)
;;
;;
;; For your orientation, the diagram below shows how the views relate to each
;; other, and the default key-bindings to navigate between them.
;; #         [C]     +--------+   [RFCE]
;; #       --------> | editor | <--------
;; #      /          +--------+          \
;; #     /         [RFCE]^                \
;; #    /                |                 \
;; # +-------+ [sjbB]+---------+  [RET] +---------+
;; # | main  | <---> | headers | <----> | message |
;; # +-------+  [q]  +---------+ [qbBjs]+---------+
;; #                   [sjbB]                ^
;; #                                     [.] | [q]
;; #                                         V
;; #                                       +-----+
;; #                                       | raw |
;; #                                       +-----+
;;
;;     Default bindings
;;     ----------------
;;     R: Reply      s: search            .: raw view
;;     F: Forward    j: jump-to-maildir
;;     C: Compose    b: bookmark-search
;;     E: Edit       q: quit
;;
;; # ----- Main View ----
;; Basics
;;
;;   * [j]ump to some maildir
;;   * enter a [s]earch query
;;   * [C]ompose a new message
;;
;; Bookmarks
;;
;;   * [bu] Unread messages
;;   * [bt] Today's messages
;;   * [bw] Last 7 days
;;   * [bp] Messages with images
;; Misc
;;
;;   * [U]pdate email & database
;;   * toggle [m]ail sending mode (direct)
;;   * [f]lush queued mail
;;
;;       * [H]elp
;;   * [q]uit mu4e
;;


;;; Flags:

;; - D -- draft
;; - F -- flagged (i.e., starred)
;; - N -- new
;; - P -- passed (i.e., forwarded)
;; - R -- replied
;; - S -- seen
;; - T -- trashed
;; - a -- has-attachement
;; - x -- encrypted
;; - s -- signed
;; - u -- unread


;;; [ Maildir ]

;; Indexing your messages
;; $ mu index --maildir=~/Mails ; first time build database.
;; $ mu index --rebuild --maildir=~/Mails ; rebuild/repair database.
;; $ mu find hello


;;; [ queuing mails ]

;; $ mu mkdir ~/Maildir/queue
;; $ touch ~/Maildir/queue/.noindex


;;; Key bindings

;; # ----- main ----
;; - [U] -- fetch emails, update email & database.
;; - [C-S-u] -- update mail & reindex
;; # ----- mu4e Message mode ----
;; - [C-c C-c] -- send message
;; - [C-c C-d] -- save to drafts and leave
;; - [C-c C-k] -- kill the message
;; - [C-c C-a] -- attach a file (pro-tip: drag & drop works as well)
;; # ----- composing -------
;; # address auto completion
;; Emacs 24 also supports cycling through the alternatives. When there are more
;; than 5 matching addresses, they are shown in a *Completions* buffer. Once the
;; number of matches gets below this number, one is inserted in the address
;; field and you can cycle through the alternatives using <TAB>.
;;
;; # Default bindings
;; # ----------------
;; # R: Reply      s: search            .: raw view (toggle)
;; # F: Forward    j: jump-to-maildir   q: quit
;; # C: Compose    b: bookmark-search
;; # E: Edit       B: edit bookmark-search
;;


;;; keybindings

;; Using the below key bindings, you can do various things with these messages; these actions are also listed in the Headers menu in the emacs menu bar.

;; key          description
;; ===========================================================
;; n,p          go to next, previous message
;; y            select the message view (if it's visible)
;; RET          open the message at point in the message view

;; searching
;; ---------
;; s            search
;; S            edit last query
;; /            narrow the search
;; \            takes you back to the previous query
;; b            search bookmark
;; B            edit bookmark before search
;; j            jump to maildir
;; M-left       previous query in queries
;; M-right      next query in queries

;; O            change sort order
;; P            toggle threading
;; Q            toggle full-search
;; V            toggle skip-duplicates
;; W            toggle include-related

;; marking
;; -------
;; d            mark for moving to the trash folder
;; DEL,D        mark for complete deletion
;; m            mark for moving to another maildir folder
;; r            mark for refiling
;; +,-          mark for flagging/unflagging (similar with mark as important)
;; ?,!          mark message as unread, read
;;
;; u            unmark message at point
;; U            unmark *all* messages
;;
;; %            mark based on a regular expression
;; T,t          mark whole thread, subthread
;;
;; <insert>     mark for 'something' (decide later)
;; #            resolve deferred 'something' marks
;;
;; x            execute actions for the marked messages

;; composition
;; -----------
;; R,F,C        reply/forward/compose
;; E            edit (only allowed for draft messages)

;; misc
;; ----
;; a            execute some custom action on a header
;; |            pipe message through shell command
;; C-+,C--      increase / decrease the number of headers shown
;; H            get help
;; C-S-u        update mail & reindex
;; q,z          leave the headers buffer

;;; URL
;; - g -- go to URL, you also can specify a range of URL like: 1 3-6 8, and "a" is a shortcut for all.

;;; Marking messages
;; - x -- mu4e-mark-execute-all
;; - % -- mark all messages that matches a certain pattern.
;; - T -- mark whole thread
;; - t -- mark sub-thread
;; - <insert> -- mark 'something' now, decide later
;; - D, <delete> -- delete
;; - + -- mark as 'flagged' (``starred'')
;; - m -- move to some maildir
;; - ! -- mark as read
;; - ? -- mark as unread
;; - r -- mark for refiling
;; - d -- move to the trash folder
;; - - -- remove 'flagged' mark
;; - u -- remove mark at the point
;; - U -- remove all marks


;;; [ Queries ]

;; mu4e queries are the same as the ones that mu find understands1. Let's look at some examples here, please refer to the mu-find and mu-easy man pages for details and even more examples.
;;
;; # get all messages regarding bananas:
;; bananas
;;
;; # get all messages regarding bananas from John with an attachment:
;; from:john flag:attach bananas
;;
;; # get all messages with subject wombat in June 2009
;; subject:wombat date:20090601..20090630
;;
;; # get all messages with PDF attachments in the /projects folder
;; maildir:/projects mime:application/pdf
;;
;; # get all messages about Rupert in the Sent Items folder
;; maildir:"/Sent Items" rupert
;; # note: terms with spaces need quoting
;;
;; # get all important messages which are signed:
;; flag:signed prio:high
;;
;; # get all messages from Jim without an attachment:
;; from:jim AND NOT flag:attach
;;
;; # get all message with Alice in one of the contacts fields (to, from, cc,
;; # bcc):
;; contact:alice
;;
;; # get all unread messages where the subject mentions Angstrom:
;; # (search is case-insensitive and accent-insensitive)
;; subject:angstrom flag:unread
;;
;; # get all unread messages between Mar-2002 and Aug-2003 about some bird:
;; date:20020301..20030831 nightingale flag:unread
;;
;; # get today's messages:
;; date:today..now
;;
;; # get all messages we got in the last two weeks regarding emacs:
;; date:2w..now emacs
;;
;; # get messages from the the Mu mailing list:
;; mu find list:mu-discuss.googlegroups.com
;;
;; # get messages with a subject soccer, Socrates, society...:
;; subject:soc*
;; # note: the '*' wildcard can only appear as the term's rightmost character
;;
;; # get all mails with attachment with filenames starting with 'pic':
;; file:pic*
;; # note: the '*' wildcard can only appear as the term's rightmost character
;;
;; # get all messages with PDF attachments:
;; mime:application/pdf
;;
;; # get all messages with image attachments:
;; mime:image/*
;; # note: the '*' wildcard can only appear as the term's @emph{rightmost}
;; # character


;;; Usage:
;; - [M-x mu4e]


;;; Installation

;;; # get from git (alternatively, use a github tarball)
;;; $ git clone git://github.com/djcb/mu.git
;;;
;;; $ cd mu
;;; $ autoreconf -i && ./configure && make
;;; # On the BSDs: use gmake instead of make
;;; $ sudo make install



;;; Code:

;;; from Ubuntu package "maildir-utils"
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;
;;; compile from git
(add-to-list 'load-path (expand-file-name "~/compile/Emacs/mu/mu/mu4e"))

(require 'mu4e)

(require 'mu4e-main)
(require 'mu4e-vars)
(require 'mu4e-view)
(require 'mu4e-proc)
;; (require 'mu4e-speedbar)
(require 'mu4e-contrib)

(if (featurep 'mu4e)
    (progn
      (define-key my-tools-prefix-map (kbd "m") 'mu4e)
      ;; FIXME: let (setq mail-user-agent 'mu4e-user-agent)
      (if (eq 'mail-user-agent 'mu4e-user-agent)
          ;; there is upper set default mail-user-agent, so default [C-x m] will be change for mu4e
          (global-set-key (kbd "C-x m") 'mu4e-compose-new)
        )
      )
  )


(setq mu4e-mu-home nil ; nil for default
      ;; mu4e-mu-binary "/usr/bin/mu"
      mu4e-mu-binary "~/compile/Emacs/mu/mu/mu/mu"
      ;; mu4e-mu-binary "/home/chris/.emacs.d/el-get/mu4e/mu/mu"
      )

;; a list of user's e-mail addresses
(setq mu4e-user-mail-address-list
      '("numbchild@gmail.com" "348284894@qq.com"))


;;; Maildir
;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
(setq mu4e-maildir "~/Mails"       ; top-level Maildir
      mu4e-sent-folder "/Send"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      ;; mu4e-refile-folder "/Archives"
      )
;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts '(("/INBOX"       . ?i)
                                 ("/Send"        . ?s)
                                 ("/Drafts"      . ?d)
                                 ("/Trash"       . ?t)
                                 ("/Work"        . ?w)
                                 ("/Emacs/help"  . ?e)
                                 ("/Emacs/Org-mode" . ?a)
                                 ("/Emacs/mu"       . ?m)
                                 ("/Lisp/comp-lang" . ?l)
                                 ("/Ruby/talk"      . ?r)
                                 ("/Ruby/Rails"     . ?R)
                                 ("/Python/help"    . ?p)
                                 ("/C"              . ?c)
                                 ("/Go"          . ?g)
                                 ;; ("/R"           . ?R)
                                 ("/SQL/SQLite"      . ?q)
                                 ;; others . ?o (mu4e default)
                                 ))


;;; Get Mail, Update
;; - U -- update, get mail.
;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.
(setq mu4e-get-mail-command "getmail"
      mu4e-update-interval 1800
      mu4e-hide-index-messages t)


;;; Send Mail

;; SMTP
;; (require 'smtpmail)
;; (setq
;;  message-send-mail-function   'smtpmail-send-it
;;  smtpmail-default-smtp-server "smtp.example.com"
;;  smtpmail-smtp-server         "smtp.example.com"
;;  smtpmail-local-domain        "example.com")


;; send mail program
;; tell message-mode how to send mail
;; - `smtpmail-send-it'
;; - `message-send-mail-with-mailclient'
;; - `message-send-mail-with-sendmail'

;;; one: [smtp] for Gmail
;; (require 'smtpmail)

;;; 1.
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "numbchild@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       ;; queue offline mode.
;;       smtpmail-queue-mail nil             ; start in non-queuing mode.
;;       smtpmail-queue-dir "~/Mails/queue/cur")

;;; 2.
;;; alternatively, for emacs-24 you can use:
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)


;;; two: [sendmail]
;; (setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-send-mail-function 'smtpmail-send-queued-mail)
;; 1: msmtp
;; (setq sendmail-program "/usr/bin/msmtp")
;; $ msmtp -C $HOME/.mutt/msmtprc
;; 2: sendmail
;; (setq sendmail-program "/usr/sbin/sendmail")
;; $ sendmail -oem -oi


;;; Queuing mail
;; you can queue the mail, and send it when you have restored your internet connection.
(setq smtpmail-queue-mail t  ;; start in non-queuing mode
      smtpmail-queue-dir  "~/Mails/queue/cur" ; send with `smtpmail-send-queued-mail'
      smtpmail-queue-index "~/Mails/queue/index"
      smtpmail-queue-index-file "index"
      )


;;; View

(setq mu4e-split-view 'horizontal ; 'vertical, 'horizontal
      mu4e-headers-visible-lines 13
      mu4e-headers-visible-columns 115
      mu4e-headers-show-threads t
      mu4e-headers-auto-update t
      mu4e-use-fancy-chars t
      ;; email prefix marks
      mu4e-headers-new-mark '("N" . " ") ; •
      mu4e-headers-unread-mark '("u" . "·")
      mu4e-headers-seen-mark '("S" . " ")
      mu4e-headers-signed-mark '("s" . "★")
      mu4e-headers-encrypted-mark '("x" . "⚴")
      mu4e-headers-draft-mark '("D" . "⚒")
      mu4e-headers-attach-mark '("a" . "▢")
      mu4e-headers-passed-mark '("P" . "❯")
      mu4e-headers-flagged-mark '("F" . "⚑")
      mu4e-headers-replied-mark '("R" . "⇦")
      mu4e-headers-trashed-mark '("T" . "✗")
      ;; thread prefix marks
      mu4e-headers-default-prefix '("|" . "┝")
      mu4e-headers-has-child-prefix '("+" . "»")
      mu4e-headers-empty-parent-prefix '("-" . "∘")
      mu4e-headers-first-child-prefix '("\\" . "┗▶")
      mu4e-headers-duplicate-prefix '("=" . "‡")
      )

;; (mu4e-headers-change-sorting 't 'descending)


;;; Message
;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
;; TODO add some separator symbols like | + etc between those fields.

(setq mu4e-headers-date-format "%x %X")

(setq mu4e-headers-fields '((:flags   .  5)
                            (:subject . 65)
                            (:from    . 15)
                            (:date    . 20))
      )

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "numbchild@gmail.com"
      user-mail-address "numbchild@gmail.com"
      user-full-name  "stardiviner")


;;; Compose
;;; Compose hooks [mu4e-compose-pre-hook, mu4e-compose-mode-hook]
;;;
;;; When replying to an email I want to use the address I received this message
;;; to as the sender of the reply. This is fairly trivial:
;;;
;; - mu4e-compose-pre-hook:
;;       this hook is run before composition starts; if you are composing a reply, forward a message, or edit an existing message, the variable mu4e-compose-parent-message points to the message being replied to, forwarded or edited, and you can use mu4e-message-field to get the value of various properties
;; - mu4e-compose-mode-hook:
;;       this hook is run just before composition starts, when the whole buffer has already been set up. This is a good place for editing-related settings. mu4e-compose-parent-message (see above) is also at your disposal. mu4e-compose-mode-hook is especially useful for editing-related settings.

;; suppose we want to set the From:-address for a reply message based on the
;; receiver of the original:

;; (add-hook 'mu4e-compose-pre-hook
;;           (defun my-mu4e-set-from-address ()
;;             "Set the From: address based on the To: address of the original."
;;             (let ((msg mu4e-compose-parent-message)) ; msg is shorter...
;;               (setq user-mail-address
;;                     (cond
;;                      ((mu4e-message-contact-field-matches msg :to "348284894@qq.com")
;;                       "348284894@qq.com")
;;                      ((mu4e-message-contact-field-matches msg :to "blackredwhore@gmail.com")
;;                       "blackredwhore@gmail.com")
;;                      (t "numbchild@gmail.com")))
;;               )))

(defun my-mu4e-compose-setting ()
  "My settings for message composition."
  (set-fill-column 72)
  (flyspell-mode)
  (auto-complete-mode)
  )

;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-compose-setting)
(add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-setting)

;; include in message with C-c C-w
(setq mu4e-compose-signature-auto-include nil
      mu4e-compose-signature
      "[ stardiviner ] I want to save myself from this world.
       IRC(freenode): stardiviner     \\ Google+:  numbchild \\
       https://stardiviner.github.io/
      "
      )

;;; compose address complete with [M-Tab].
;; FIXME: [M-Tab] is very slow for completion.
(setq mu4e-compose-complete-addresses t ; e-mail address auto completion
      ;; to limit completion pool, filter mailing list addresses and like.
      mu4e-compose-complete-only-personal nil
      mu4e-compose-complete-ignore-address-regexp "no-?reply"
      mu4e-compose-keep-self-cc t ; keep myself on the Cc: list.
      ;; mu4e-compose-complete-only-after
      )

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 80)
            (flyspell-mode-on))
          )


;; Signing and encrypting It's possible using emacs-mime, most easily accessed
;; through the Attachments-menu while composing a message, or with M-x
;; mml-secure-message-encrypt-pgp, M-x mml-secure-message-sign-pgp.  The support
;; for encryption and signing is independent of the support for their
;; counterparts, decrypting and signature verification. Even if your mu4e does
;; have support for the latter two, you can still sign/encrypt messages.

;;; message inline pgp sign.
;; `message-send-hook' or `mu4e-compose-mode-hook'
;; `mml-secure-message-sign-pgpauto' or `mml-secure-message-sign-pgpmime'
(add-hook 'message-send-hook 'mml-secure-message-sign-pgpauto)


;;; Headers

;;; `mu4e-header-info'.

(setq mu4e-headers-auto-update t
      mu4e-headers-skip-duplicates t
      )

;;; `mu4e-header-info-custom'

;; (add-to-list 'mu4e-header-info-custom
;;              '(:recipnum :name "Number of recipients" :shortname "Recip#" :help "Number of recipients for this message" :function
;;                          (lambda
;;                            (msg)
;;                            (format "%d"
;;                                    (+
;;                                     (length
;;                                      (mu4e-message-field msg :to))
;;                                     (length
;;                                      (mu4e-message-field msg :cc)))))))

;; TODO: make use of flag: list.

;;; press [s] -> [:references [regexp]] in search query.

(add-to-list 'mu4e-header-info-custom
             '(:references :name "References: "
                           :shortname "References"
                           :help "Reference of this thread"
                           :function
                           (lambda
                             (msg)
                             (format "%s"
                                     (mu4e-message-field msg :References)))))

;; (add-to-list 'mu4e-header-info-custom
;;              '(:reply2mythread :name "My thread smart choose depend on References"
;;                                :shortname "My Thread"
;;                                :help "Emails which reply to the thread I created"
;;                                :function
;;                                (lambda
;;                                  (msg)
;;                                  (string-match-p ".*@[HOSTNAME]" ; TODO use a code to get current hostname
;;                                                  (format "%s"
;;                                                          (mu4e-message-field msg :References)))
;;                                  )))

;;; `mu4e-headers-custom-markers'

;; (add-to-list 'mu4e-headers-custom-markers
;;              '("More than n recipients"
;;                (lambda (msg n)
;;                  (> (+ (length (mu4e-message-field msg :to))
;;                        (length (mu4e-message-field msg :cc))) n))
;;                (lambda ()
;;                  (read-number "Match messages with more recipients than: ")))
;;              t)

(add-to-list 'mu4e-headers-custom-markers
             '("Reply to my thread"
               (lambda (msg reply2mythread)
                 (string-match-p ".*stardiviner"
                  (mu4e-msg-field msg :References)))
               (lambda nil
                 (message "Messages replied to your thread.")))
             t)



;; creating org-mode links
;;
;; - [M-x org-store-link] -- store link to org-mode.
;; - [C-c C-l] -- in org file, you can insert upper stored link into org file.
;;
;; It can be useful to include links to e-mail messages or even search queries
;; in your org-mode files. mu4e supports this with the org-mu4e module; you can
;; set it up by adding it to your configuration:
;;
(require 'org-mu4e)
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (org-mu4e-compose-org-mode) ; edit with org-mode in e-mail body.
            ;; NOTE: you need to exit the email body part, enter mu4e-compose part to exit.
            ))
;;
;; After this, you can use the normal org-mode mechanisms to store links: M-x
;; org-store-link stores a link to a particular message when you're in Message
;; view, and a link to a query when you are in Headers view.
;;
;; You can insert this link later with M-x org-insert-link. From org-mode, you
;; can go to the query or message the link points to with either M-x
;; org-agenda-open-link in agenda buffers, or M-x org-open-at-point elsewhere -
;; both typically bound to C-c C-o.


;;; Sort order and threading
;; - O -- sort. mu4e-headers-change-sorting
;; - P -- toggle threading
;; By default, mu4e sorts messages by date, in descending order: the most recent
;; messages are shown at the top.
;; In addition, the messages are threaded, i.e., shown in the context of a
;; discussion thread; this also affects the sort order.

;; (mu4e-headers-toggle-threading) ; toggle threading. (P)


;;; Message view

;; (setq mu4e-split-view 'horizontal)

(setq mu4e-view-fields '(:from :to :cc
                               :subject
                               :date
                               :flags
                               :maildir
                               :attachments
                               ;; FIXME:
                               ;; :mailing-list
                               :references
                               ;; :tags
                               :signature
                               )
      mu4e-view-show-addresses t ; show e-mail address after names of contacts From: field. or press [M-RET] to view.
      )
(setq mu4e-view-scroll-to-next nil
      mu4e-split-view 'horizontal ; split view
      )


;;; mu-cite

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/init/extensions/mu-cite/"))
(require 'mu-cite)

(setq message-cite-function 'mu-cite-original
      mu-cite-top-format '("On " date ", " from " wrote:\n\n")
      mu-cite-prefix-format '(" > "))
;; (add-hook 'mu4e-view-mode-hook 'mu4e-view-toggle-hide-cited) ; [C-c h] to toggle hide cited.
;; (define-key mu4e-view-mode-map (kbd "C-c h") 'mu4e-view-toggle-hide-cited)


;;; message-cite

;; (setq message-cite-style
;;       '((posting-from-work-p)
;;         (eval
;;          (set (make-local-variable 'message-cite-style) message-cite-style-thunderbird))))

(setq message-cite-style message-cite-style-thunderbird)

;; (setq message-cite-style-gmail
;;       '((message-cite-function 'message-cite-original)
;;         (message-citation-line-function 'message-insert-formatted-citation-line)
;;         (message-cite-reply-position 'above)
;;         (message-yank-prefix "    ")
;;         (message-yank-cited-prefix "    ")
;;         (message-yank-empty-prefix "    ")
;;         (message-citation-line-format "On %e %B %Y %R, %f wrote:\n")))


;; viewing images inline
;;
;; It is possible to show images inline in the message view buffer if you run
;; emacs in GUI-mode. You can enable this by setting the variable
;; mu4e-view-show-images to t. Since emacs does not always handle images
;; correctly, this is not enabled by default. If you are using emacs 24 with
;; ImageMagick1 support, make sure you call imagemagick-register-types in your
;; configuration, so it is used for images.
;; enable inline images
;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 400)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


;; Displaying rich-text messages
;;
;; mu4e normally prefers the plain-text version for messages that consist of
;; both a plain-text and html (rich-text) versions of the body-text. You change
;; this by setting mu4e-view-prefer-html to t.
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "html2text -utf8 -width 72")


;;; Attachments

;; - A -- action to pick some custom action to perform on current message/attachment.
;; - w -- open-with
;; - | -- pipe
;; - e -- open in emacs
;; By default, mu4e uses the xdg-open-program 1 or (on MacOS) the open program
;; for opening attachments. If you want to use another program, you do so by
;; setting the MU_PLAY_PROGRAM environment variable to the program to be used.

(setq mu4e-attachment-dir "~/Downloads")


;;; Actions

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

;;; adding an action to actions list.
(defun show-number-of-recipients (msg)
  "Display the number of recipients for the MSG at point."
  (message "Number of recipients: %d"
           (+ (length (mu4e-message-field msg :to))
              (length (mu4e-message-field msg :cc)))))
;; FIXME void variable
;; (add-to-list 'mu4e-headers-actions
;;              '("Number of recipients" . show-number-of-recipients) t)
;; search for messages by the sender of the message at point:
(defun search-for-sender (msg)
  "Search for messages sent by the sender of the MSG at point."
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))
;; define 'x' as the shortcut
;; FIXME void variable
;; (add-to-list 'mu4e-view-actions
;;              '("xsearch for sender" . search-for-sender) t)



;;; Searching
;; - Q -- search. mu4e-headers-toggle-full-search.
;;
;; mu4e is fully search-based: even if you 'jump to a folder', you are executing
;; a query for messages that happen to have the property of being in a certain
;; folder.
;;
;; Normally, queries return up to mu4e-headers-results-limit (default: 500)
;; results. That is usually more than enough, and makes things significantly
;; faster. Sometimes, however, you may want to show all results; you can enable
;; this with M-x mu4e-headers-toggle-full-search, or by customizing the variable
;; mu4e-headers-full-search. This applies to all search commands.

(setq mu4e-headers-full-search nil) ; whether show all search results. or depend on `mu4e-headers-results-limit'.
(setq mu4e-headers-results-limit 1000)

;;; Including related messages
;;
;; It can be useful to not only show the messages that directly match a certain
;; query, but also include messages that are related to these messages. That is,
;; messages that belong to the same discussion thread are included in the
;; results, just like e.g. Gmail does it. You can enable this behavior by
;; setting mu4e-headers-include-related to t, and you can toggle between
;; including/not-including with <W>.
(setq mu4e-headers-include-related t
      mu4e-headers-skip-duplicates t) ; skip duplicates


;;; Compose

;; - `org-mu4e-compose-org-mode' :: 


;;; Send

(setq mu4e-sent-messages-behavior 'sent)


;;; Crypto (signing, encrypting, verifying, decrypting)
;;; - v -- see the details of the signature verification by activating the Details.
;; start gpg-agent manually:
;; $ eval $(gpg-agent --daemon)
(setq mu4e-decryption-policy t) ; auto decrypt.


;;; Refiling

;; - r -- refiling, mu4e-refile-folder

(setq mu4e-refile-folder                ; dynamic refiling
      (lambda (msg)
        (cond
         ;; mu discuss Google Groups
         ((mu4e-message-contact-field-matches msg :to "mu-discuss@googlegroups.com")
          "/Emacs/mu")
         ;; TODO delete all Cron getmail error messages which is network unavailable error.
         ((string-match "Cron .* getmail.*" (or (mu4e-message-field msg :subject) ""))
          ;; (mu4e-message-contact-field-matches msg :from "Cron Daemon")
          "/Trash")
         ;; everything else goes to /archive
         ;; *important* to have a catch-all at the end!
         (t "/archive")
         )))


;;; Bookmarks

;; - [b] :: bookmark
;; - [B] :: edit bookmark before jump/invoking.

;; - B -- mu4e-headers-search-bookmark-edit, which lets you edit the bookmarked query before invoking it.
;; (setq mu4e-bookmarks
;;       '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
;;         ("date:today..now" "Today's messages" ?t)
;;         ("date:7d..now" "Last 7 days" ?w)
;;         ("mime:image/*" "Messages with images" ?p)))
(setq mu4e-bookmarks
             '(("size:5M..500M"       "Big messages"       ?b) ; big
               ("date:today..now"     "Today's messages"     ?d) ; today
               ("date:today..now flag:unread" "Today's unread messages" ?n) ; new messages.
               ("date:1w..now"        "This week's messages" ?w) ; week
               ;; TODO: ("references:.*@stardiviner" "The mail reply to my thread" ?m) ; reply to my thread.
               ("maildir:/Emacs/help"  "Emacs mailbox" ?e) ; Email mailbox.
               ))

;; (add-hook 'mu4e-index-updated-hook
;;           (defun mu4e-new-mail-alert ()
;;             ;; FIXME (shell-command "mplayer /home/chris/Music/Sounds/Hacking Game/voice-complete.wav &>/dev/null")
;;             ))


;;; Faces

;; (set-face-attribute 'mu4e-faces nil)
(set-face-attribute 'mu4e-header-highlight-face nil ; current select line
                    :background "#004A5D" :foreground "white"
                    :box '(:color "cyan" :line-width 1)
                    :weight 'normal :underline nil
                    )
;;; highlighted email, main view key color like "[q]uit mu4e".
(set-face-attribute 'mu4e-highlight-face nil
                    :foreground "cyan" :background "#073642")
;;; face for things that are ok.
(set-face-attribute 'mu4e-ok-face nil
                    :foreground "forest green"
                    :box '(:color "forest green" :line-width 1 :style nil)
                    )
;;; moved
(set-face-attribute 'mu4e-moved-face nil
                    :foreground "dark gray" :background "dim gray"
                    )
;;; draft (my draft), my sent mail.
(set-face-attribute 'mu4e-draft-face nil
                    :foreground "sky blue"
                    )
;;; Emacs mu4e window top title.
(set-face-attribute 'mu4e-title-face nil
                    :foreground "yellow")
;;; readed mail line in index.
(set-face-attribute 'mu4e-header-face nil
                    :foreground "dim gray")
;;; footer
(set-face-attribute 'mu4e-footer-face nil
                    :foreground "deep sky blue")
;;; unread email
(set-face-attribute 'mu4e-unread-face nil
                    :foreground "forest green")
(set-face-attribute 'mu4e-system-face nil
                    :foreground "white")
;;; trash email
(set-face-attribute 'mu4e-trashed-face nil
                    :strike-through "black")
;;; level 1 cited (quoted email content)
(set-face-attribute 'mu4e-cited-1-face nil
                    :foreground "cyan")
(set-face-attribute 'mu4e-cited-2-face nil
                    :foreground "dark cyan")
(set-face-attribute 'mu4e-cited-3-face nil
                    :foreground "forest green")
(set-face-attribute 'mu4e-cited-4-face nil
                    :foreground "gold")
(set-face-attribute 'mu4e-cited-5-face nil
                    :foreground "white")
(set-face-attribute 'mu4e-cited-6-face nil
                    :foreground "light blue")
(set-face-attribute 'mu4e-cited-7-face nil
                    :foreground "light green")
;;; warning
(set-face-attribute 'mu4e-warning-face nil
                    :box '(:color "red" :line-width 1)
                    :background "dark red"
                    :foreground "white")
;;; flagged email
(set-face-attribute 'mu4e-flagged-face nil
                    :foreground "green yellow" :background "black"
                    :weight 'bold
                    )
;;; replied email
(set-face-attribute 'mu4e-replied-face nil
                    :foreground "orange"
                    :overline "slate blue")
;; forwarded email
(set-face-attribute 'mu4e-forwarded-face nil
                    :foreground "dark orange"
                    :overline "magenta")
;; compose
(set-face-attribute 'mu4e-compose-header-face nil
                    :foreground "cyan"
                    :weight 'bold)
;; -- text follows this line -- where following Org-mode message body.
(set-face-attribute 'mu4e-compose-separator-face nil
                    :foreground "red")
;;; link
(set-face-attribute 'mu4e-link-face nil
                    :underline '(:style line))
;;; contact: e.g. Christopher Miles, help-gnu-emacs@gnu.org
(set-face-attribute 'mu4e-contact-face nil
                    :foreground "yellow")
;;; header-
;;; header field keys: e.g. From:, To:, Subject:,
;; some keys.
(set-face-attribute 'mu4e-header-key-face nil
                    :foreground "magenta"
                    )
(set-face-attribute 'mu4e-header-marks-face nil
                    :foreground "light blue"
                    )
(set-face-attribute 'mu4e-header-title-face nil
                    :foreground "white"
                    )
(set-face-attribute 'mu4e-header-value-face nil
                    :foreground "#444444"
                    )
(set-face-attribute 'mu4e-special-header-value-face nil
                    :foreground "magenta"
                    )

;; header names: like From: Subject: etc.
(set-face-attribute 'message-header-name nil
                    :foreground "cyan" :background "black"
                    :box '(:color "#333333" :line-width -1)
                    :weight 'bold)
;; Subject: value
(set-face-attribute 'message-header-subject nil
                    :foreground "cyan"
                    :weight 'bold
                    :underline t)
;; To: value
(set-face-attribute 'message-header-to nil
                    :foreground "white")
;; other header values
(set-face-attribute 'message-header-other nil
                    :foreground "#888888")



;;; Marking

(define-key mu4e-headers-mode-map (kbd "m") 'mu4e-headers-mark-for-something)
(define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-for-move)


;;; Add Org-mode structure support for Emails

(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (turn-on-orgstruct) ; Org-struct minor mode
            (turn-on-orgstruct++)
            ;; enable Orgtbl minor mode in message-mode.
            (turn-on-orgtbl)))


;;; Gmail
;; (setq mu4e-maildir "~/Maildir")
;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; (setq mu4e-trash-folder  "/[Gmail].Trash")
;; (setq mu4e-send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq mu4e-sent-messages-behavior 'delete)
;; (setq mu4e-maildir-shortcuts
;;       '( ("/INBOX"               . ?i)
;;          ("/[Gmail].Sent Mail"   . ?s)
;;          ("/[Gmail].Trash"       . ?t)
;;          ("/[Gmail].All Mail"    . ?a)))
;; (setq mu4e-get-mail-command "offlineimap")
;; (require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)
;; ;; alternatively, for emacs-24 you can use:
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)



(provide 'init-my-tool-email-mu4e)

;;; init-my-tool-email-mu4e.el ends here
