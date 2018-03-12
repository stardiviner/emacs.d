;;; init-org-capture.el --- init for Org Capture-Refile-Archive
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Capture - Refile - Archive ]

(require 'org-capture)

(setq org-default-notes-file
      (concat org-directory "/Tasks/Tasks.org"))

(setq org-capture-templates
      '(("c" "[C]apture"
         entry (file "")
         ;; select todo keyword interactively from `org-todo-keywords'.
         "* %(completing-read \"Todo keyword: \" org-todo-keywords-for-agenda nil t) %^{Capture} \n:PROPERTIES:\n:TIME: %U\n:END: \n%i\n%a\n\n%?"
         ;; :time-prompt t
         :empty-lines-before 1
         :empty-lines-after 1
         )

        ;; Tasks
        ("t" "Add a [t]ime scheduled task into Tasks"
         entry (file "~/Org/Tasks/Computer Todos.org")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
         :empty-lines 1
         )

        ;; Diary
        ("d" "Write [d]iary"
         entry (file+olp+datetree "~/Org/Diary/Diary.org")
         "* %^{Diary Title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d-%R>.org][On %<%Y-%m-%d %R>]]\n\nEvent: %?\n\n%i\n\n"
         ;; :time-prompt t
         :empty-lines-before 1
         :empty-lines-after 1
         :jump-to-captured t
         )
        
        ;; Bookmark
        ("k" "Add an URL to bookmar[k]s database"
         entry (file "~/Org/Bookmarks/Bookmarks.org")
         "* %^{Bookmark URL}\n\n%A\n\n%?\n\n"
         :empty-lines 1
         :jump-to-captured t
         )

        ;; org-passwords
        ("A" "[A]ccount passwords"
         entry (file "~/Org/Accounts/accounts.org.gpg")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines-before 1
         :empty-lines-after 1
         )

        ;; code snippets
        ("s" "code [s]nippet" entry
         (file (lambda () (concat org-directory "/Programming Code/Code Snippets/snippets.org")))
         ;; Prompt for tag and language
         "* %?%^g\n#+begin_src %^{language}\n\n#+end_src")
        ))

;;; Context org-capture templates.
;; TODO:
;; (setq org-capture-templates-contexts
;;       '(("p" (in-mode . "message-mode"))))


;;;_* Refile

;; Refile targets include this file and any file contributing to the
;; agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 1))
      ;; org-refile-use-outline-path t
      ;; org-outline-path-complete-in-steps t
      ;; org-refile-target-verify-function nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      org-refile-use-outline-path t
      )

;;;_* Archive

(add-to-list 'org-tag-persistent-alist '("ARCHIVE" . ?A))

;;; [ org-protocol capture ]

(require 'org-protocol)

(setq org-capture-templates
      (append `(("P" "Org-[P]rotocol")
                ("PP" "Protocol"
                 entry (file ,(concat org-directory "/Tasks/Tasks.org"))
                 "* %^{Title}\nSource: %u, %c\n #+begin_quote\n%i\n#+end_quote\n\n\n%?"
                 :prepend t
                 :empty-lines 1
                 )
                ("PL" "Protocol Link"
                 entry (file ,(concat org-directory "/Tasks/Tasks.org"))
                 "* %? [[%:link][%:description]] \nCaptured On: %U"
                 :prepend t
                 :empty-lines 1
                 )
                )
              org-capture-templates))

;;; [ org-protocol-capture-html ] -- Capture HTML from the browser selection into Emacs as org-mode content.

(use-package org-protocol-capture-html
  :quelpa (org-protocol-capture-html
           :fetcher github :repo "alphapapa/org-protocol-capture-html"
           :upgrade nil)
  :config
  (setq org-capture-templates
        (append '(("PH" "org-[p]rotocol-capture-[h]tml" entry
                   (file "")
                   "* %a :website:\n\n%U %?\n\n%:initial"))
                org-capture-templates))
  )


(provide 'init-org-capture)

;;; init-org-capture.el ends here
