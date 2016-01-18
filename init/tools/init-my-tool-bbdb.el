;;; init-my-tool-bbdb.el --- init BBDB
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Help:
;; http://savannah.nongnu.org/projects/bbdb/
;; http://www.mail-archive.com/bbdb-info@lists.sourceforge.net/
;; http://www.emacswiki.org/cgi-bin/wiki/CategoryBbdb


;;; Code:

;;; [ BBDB ] -- The Insidious Big Brother Database (BBDB) is a contact management utility.

(use-package bbdb
  ;; :ensure t
  :config
  ;; initialize
  ;; (bbdb-initialize)
  (bbdb-initialize 'mail 'message 'gnus 'mu4e 'anniv 'pgp)


  ;; If t then BBDB will not modify `bbdb-file'.
  ;; If you have more than one Emacs running at the same time, you might want to
  ;; set this to t in all but one of them.
  ;; (setq bbdb-read-only t )

  (setq bbdb-file (expand-file-name "~/Org/BBDB/bbdb")
        ;; bbdb-image 'name ; display records with an image.
        bbdb-image-path (expand-file-name "~/Org/BBDB/avatars/")
        ;; bbdb-image-suffixes '(".png" ".jpg" ".gif" ".xpm")
        ;; bbdb-sound-files
        bbdb-default-label-list '("personal"
                                  "home" "work" "company" "organization" "other")
        bbdb-default-country "China"
        bbdb-dial-local-prefix "86" ; "+86" TODO: is this right?
        bbdb-default-area-code "86"
        ;; bbdb-default-xfield 'notes
        ;; bbdb-xfield-label-list
        ;; bbdb-init-forms
        ;; bbdb-mua-pop-up-window-size
        ;; bbdb-auto-notes-rules
        ;; bbdb-auto-notes-rules-expanded

        bbdb-add-mails 'query
        ;; bbdb-address-label-list
        bbdb-need-to-sort t
        bbdb-ignore-message-alist
        '(("From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")
          ;; (("To" "CC") . "mailing-list-1\\|mailing-list-2")
          )

        ;; completion
        ;; `bbdb-complete-mail'
        bbdb-completion-list t
        bbdb-completion-display-record nil
        
        ;; Snarf
        bbdb-snarf-rule-alist '((us
                                 bbdb-snarf-surrounding-space
                                 ;; bbdb-snarf-phone-nanp bbdb-snarf-url
                                 bbdb-snarf-mail
                                 bbdb-snarf-empty-lines
                                 bbdb-snarf-name
                                 ;; bbdb-snarf-address-us
                                 bbdb-snarf-empty-lines
                                 ;; bbdb-snarf-notes
                                 bbdb-snarf-name-mail)
                                (mail
                                 bbdb-snarf-mail-address))
        bbdb-snarf-default-label-alist '((phone . "work") (address . "work") (company . "company"))
        bbdb-snarf-rule-default 'mail
        )

  ;; keybindings
  (unless (boundp 'my-bbdb-map)
    (define-prefix-command 'my-bbdb-map))
  (define-key my-tools-prefix (kbd "B") 'my-bbdb-map)

  (define-key my-bbdb-map (kbd "B") 'my-bbdb-open-or-switch) ; or 'bbdb.

  (defun my-bbdb-open-or-switch ()
    (interactive)
    (if (get-buffer "*BBDB*")
        (switch-to-buffer "*BBDB*")
      (bbdb "")
      ;; (bury-buffer)
      ;; (switch-to-buffer "*BBDB*")
      ))
  (define-key my-bbdb-map (kbd "b") 'bbdb)

  (define-key my-bbdb-map (kbd "c") 'bbdb-create)
  ;; usage: region select name and email part in To: field. then press this keybinding.
  (define-key my-bbdb-map (kbd "a") 'bbdb-snarf)


  ;; (bbdb-mua-auto-update-init 'gnus 'message)
  ;; (setq bbdb-update-records-p 'create)

  ;; Faces
  (set-face-attribute 'bbdb-name nil
                      :inherit 'font-lock-function-name-face
                      ;; :foreground "cyan"
                      :weight 'bold
                      )
  (set-face-attribute 'bbdb-organization nil
                      :inherit 'font-lock-comment-face
                      ;; :foreground ""
                      :slant 'italic
                      )
  (set-face-attribute 'bbdb-field-name nil
                      :inherit 'font-lock-variable-name-face
                      :foreground "sky blue"
                      )

  ;; Setup
  (dolist (hook '(message-setup-hook
                  mu4e-compose-mode-hook
                  ))
    (add-hook hook 'bbdb-mail-aliases))


  ;; BBDB [M-TAB] conflicts with ispell.
  (dolist (hook '(message-mode-hook
                  mu4e-compose-mode-hook
                  ))
    (add-hook hook (lambda ()
                     (define-key message-mode-map (kbd "M-TAB")
                       'bbdb-complete-mail))))
  )



;;; define variant faces for variant xfields

;; TODO: improve it
(setq bbdb-name-face-alist '((mail . bbdb-field-mail)
                             (mail-alias . bbdb-field-mail-alias)))

(defface bbdb-field-mail
  '((t (:inherit bbdb-field-name :foreground "green yellow")))
  "Face used for BBDB fields."
  :group 'bbdb-faces)

(defface bbdb-field-mail-alias
  '((t (:inherit bbdb-field-name :foreground "yellow")))
  "Face used for BBDB fields."
  :group 'bbdb-faces)


;;; --- Auto-creation of all messages addressed to me ---
;;
;; (setq bbdb/mail-auto-create-p 'bbdb-prune-not-to-me)
;; (setq bbdb/news-auto-create-p 'bbdb-prune-not-to-me)
;; (defun bbdb-prune-not-to-me ()
;;   "defun called when bbdb is trying to automatically create a record.  Filters out
;; anything not actually adressed to me then passes control to 'bbdb-ignore-some-messages-hook'.
;; Also filters out anything that is precedense 'junk' or 'bulk'  This code is from
;; Ronan Waide < waider @ waider . ie >."
;;   (let ((case-fold-search t)
;;         (done nil)
;;         (b (current-buffer))
;;         (marker (bbdb-header-start))
;;         field regexp fieldval)
;;     (set-buffer (marker-buffer marker))
;;     (save-excursion
;;       ;; Hey ho. The buffer we're in is the mail file, narrowed to the
;;       ;; current message.
;;       (let (to cc precedence)
;;         (goto-char marker)
;;         (setq to (bbdb-extract-field-value "To"))
;;         (goto-char marker)
;;         (setq cc (bbdb-extract-field-value "Cc"))
;;         (goto-char marker)
;;         (setq precedence (bbdb-extract-field-value "Precedence"))
;;         ;; Here's where you put your email information.
;;         ;; Basically, you just add all the regexps you want for
;;         ;; both the 'to' field and the 'cc' field.
;;         (if (and (not (string-match "doug@" (or to "")))
;;                  (not (string-match "doug@" (or cc ""))))
;;             (progn
;;               (message "BBDB unfiling; message to: %s cc: %s"
;;                        (or to "noone") (or cc "noone"))
;;               ;; Return nil so that the record isn't added.
;;               nil)
;;
;;           (if (string-match "junk" (or precedence ""))
;;               (progn
;;                 (message "precedence set to junk, bbdb ignoring.")
;;                 nil)
;;
;;             ;; Otherwise add, subject to filtering
;;             (bbdb-ignore-some-messages-hook)))))))



;;; -- Dial --
;; (setq bbdb-dial-function
;;       '(lambda (phone-number)
;;          (do-applescript
;;           (concat
;;            "tell application \"Skype\"\n"
;;            "send command \"CALL +" phone-number "\" script name \"Call from BBDB\"\n"
;;            "end tell"))))


;;; [ bbdb- ] -- More easily search/choice than BBDB

;;; Usage:
;;
;; - Start
;;
;; Execute `bbdb-:open' or `bbdb-:start-completion'.
;; `bbdb-:start-completion' is better in the buffer that you write mail on.
;; In default, you can execute `bbdb-:start-completion' by pushing the key that is bound for `bbdb-complete-mail'.
;; Alternatively, set `bbdb-:start-completion-key'.
;; Otherwise, do key binding by yourself.
;;
;; - Keymap of bbdb-
;;
;; The bbdb- buffer, which is named *bbdb-* and shown at the start, have the following keymap.
;;
;; j ... Go to next record
;; k ... Go to previous record
;; h ... Go to previous char
;; l ... Go to next char
;; J ... Scroll down
;; K ... Scroll up
;; s ... Start incremental search
;; S ... Start incremental search with the reverse configuration about using migemo
;; a ... Show all record
;; t ... Mark current record as To
;; c ... Mark current record as Cc
;; b ... Mark current record as Bcc
;; u ... Unmark current record
;; * t ... Mark all listed record as To
;; * c ... Mark all listed record as Cc
;; * b ... Mark all listed record as Bcc
;; * u ... Unmark all listed record
;; R ... Reload the latest record of BBDB
;; q ... Finish with doing nothing
;; RET ... Finish with the update of To/Cc/Bcc header (if necessary, open the mail buffer)

;; (require 'bbdb-)
;;
;; (setq bbdb-:mail-modes '(message-mode mu4e-compose-mode)
;;       ;; bbdb-:rcpt-header-format 'multi-line
;;       bbdb-:replace-complete-mail-command t ; Whether substitute `bbdb-:start-completion' for `bbdb-complete-mail'.
;;       bbdb-:start-completion-key nil
;;       bbdb-:use-migemo nil ; TODO: set to t
;;       )
;;
;; (set-face-attribute 'bbdb-:bcc-face nil
;;                     :foreground "red"
;;                     :weight 'bold
;;                     )
;; (set-face-attribute 'bbdb-:cc-face nil
;;                     :foreground "gray"
;;                     )
;; (set-face-attribute 'bbdb-:to-face nil
;;                     :foreground "cyan"
;;                     )
;;
;; (bbdb-:setup)
;;
;; (if (featurep 'bbdb-)
;;     (define-key my-bbdb-map (kbd "B") 'bbdb-:open)
;;   )


;;; [ bbdb-vcard ]

;;; Usage:
;;
;;; import:
;;
;; - `bbdb-vcard-import-file'
;; - `bbdb-vcard-import-buffer'
;; - `bbdb-vcard-import-region'
;;
;; export: (in bbdb buffer)
;;
;; - [v]  :: to export the record under the point.
;; - [* v] :: to export all records in buffer into one vCard file.
;; - [* C-u v] :: to export them into one file each.
;;
;; - [V] or [* V] :: to put one or all vCard(s) into the kill ring.
;;
;;; vCard Media Objects
;;
;; The importer stores inline base46-encoded images, sounds, and cryptographic
;; keys to the local disk under the `bbdb-vcard-directory' directory. The
;; relative filenames for these objects are stored in the following BBDB
;; xfields, respectively:
;;
;; - image-filename: "media/image-<sha1sum>.<suffix>"
;; - sound-filename: "media/sound-<sha1sum>.<suffix>"
;; - gpg-key-filename: "media/key-<sha1sum>.<suffix>"
;;
;; if the variable `bbdb-image' is uncustomized when bbdb-vcard is initialized,
;; it will be set to `bbdb-vcard-image-basename'. This will allow to BBDB to
;; locate images when displaying records.

(use-package bbdb-vcard
  :config
  (setq bbdb-vcard-directory "~/Org/BBDB/vCards"
        bbdb-vcard-media-directory "media/"
        )
  ;; TODO:
  ;; (setq bbdb-vcard-skip-on-import
  ;;       bbdb-vcard-skip-on-export
  ;;       bbdb-vcard-skip-valueless)

  (setq bbdb-vcard-use-fullname t ; use full-name can reduce merge conflicts.
        ;; bbdb-vcard-name-imported-priority '(first-last formated-name bbdb-vcard-generate-bbdb-name)
        ;; bbdb-vcard-x-bbdb-candidates
        )
  )


;;; [ bbdb-csv-import ]


;;; [ gmail2bbdb ]

;;; Usage:
;;
;; - `gmail2bbdb-import-file'

;; (autoload 'gmail2bbdb-import-file "gmail2bbdb" nil t nil)

;; (add-to-list 'gmail2bbdb-excluded-email-regex-list "noreply@*.com")



(provide 'init-my-tool-bbdb)

;;; init-my-tool-bbdb.el ends here
