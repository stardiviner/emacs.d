;;; init-my-org-capture.el --- init for Org Capture-Refile-Archive
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Capture - Refile - Archive ]

(require 'org-capture)

(setq org-default-notes-file
      (concat org-directory "/Capture/notes.org"))

(setq org-capture-templates
      '(("c" "Capture"
         entry (file+headline "~/Org/Capture/Capture.org" "Capture")
         "\n* TODO %^{prompt}\n\n%i\n\n%a\n\n%?"
         :prepend t
         :empty-lines 1
         )

        ;; Tasks
        ("t" "Add a task into Tasks"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{prompt} [/]\n\n%?\n\n"
         :empty-lines 1
         )
        ("i" "Clock in a New Task"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{prompt} [/]\n\n%?\n\n"
         :empty-lines 1
         :clock-in t :clock-resume t
         )

        ;; Bookmark
        ("m" "Add an URL to bookmarks database"
         entry (file+headline "~/Org/Wiki/Data/Bookmarks/Bookmarks.org" "Capture")
         "\n* %^{prompt}\n\n%A\n\n%?\n\n"
         :empty-lines 1
         )

        ;; TODO: Contacts
        ("c" "Contacts"
         entry (file+headline "~/Org/Contacts/Contacts.org")
         "* %(org-contacts-template-name) %^g
%(org-contacts-template-email)
:PROPERTIES:
:NAME:
:NICK-NAME:
:BIRTHDAY: %:date
:URL: %:url
:EMAIL: %?
:MOBILE:
:WORK:
:SKILLS:
:HOME:
:COMPANY:
:ADDRESS:
:NOTE:
:END:")
        
        ;; org-passwords
        ;; FIXME:
        ("p" "password"
         entry (file+headline "~/Git/dotfiles/passwords.gpg" "Accounts")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines 1
         )

        ;; Issues, Bugs, Features
        ("b" "Bug"
         entry (file+olp "~/Org/Projects/Programming.org" "Computer" "Bugs")
         "\n* BUG %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("i" "Issue"
         entry (file+olp "~/Org/Projects/Programming.org" "Computer" "Issues")
         "\n* ISSUE %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("f" "Feature"
         entry (file+olp "~/Org/Projects/Programming.org" "Computer" "Features")
         "\n* FEATURE %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ))


;; To define special keys to capture to a particular template without
;; going through the interactive template selection, you can create your
;; key binding like this:
;;
;; (global-set-key (kbd "C-c x")
;;                 '(lambda () (interactive) (org-capture nil "x")))


(if (featurep 'helm)
    (define-key my-org-prefix (kbd "c") 'helm-org-capture-templates)
  (define-key my-org-prefix (kbd "c") 'org-capture)
  (define-key org-mode-map (kbd "C-c c") 'org-capture))


;;;_* Refile

(setq org-refile-keep nil
      org-refile-markers nil
      ;;; Refile targets include this file and any file contributing to the
      ;;; agenda - up to 5 levels deep
      org-refile-targets '((nil :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5))
      ;;; Targets start with the file name - allows creating level 1 tasks
      ;; org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps t
      ;; org-refile-use-outline-path ; TODO
      org-refile-target-verify-function t
      org-refile-use-outline-path nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      )

;;;_* Archive

(setq org-archive-location "%s_archive::"
      org-archive-save-context-info '(time file olpath category todo itags ltags)
      org-archive-mark-done nil
      org-archive-stamp-time t
      org-archive-reversed-order nil
      )


(provide 'init-my-org-capture)

;;; init-my-org-capture.el ends here
