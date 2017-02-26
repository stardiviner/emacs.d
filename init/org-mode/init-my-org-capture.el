;;; init-my-org-capture.el --- init for Org Capture-Refile-Archive
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Capture - Refile - Archive ]

(require 'org-capture)

(setq org-default-notes-file
      (concat org-directory "/Capture/notes.org"))

(setq org-capture-templates
      '(("c" "[C]apture"
         entry (file+headline "~/Org/Capture/Capture.org" "Capture")
         "\n* TODO %^{Capture}\nSCHEDULED: %t\n%i\n\n%a\n\n%?"
         :prepend t
         :empty-lines 1
         )

        ;; Tasks
        ("t" "Add a [t]ask into Tasks"
         entry (file "~/Org/Projects/Computer Todos.org")
         ;; "\n* TODO %^{Task} [/]\n\n%?\n\n"
         "\n* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
         :empty-lines 1
         )
        ("n" "Next Task"
         entry (file+headline "~/Org/Tasks/Tasks.org" "Tasks")
         "** NEXT %? \nDEADLINE: %t\n"
         :empty-lines 1)
        ("T" "New [T]ask with Clock"
         entry (file+headline "~/Org/Tasks/Tasks.org" "Tasks")
         "\n* TODO %^{Clock Task} [/]\n\n%?\n\n"
         :empty-lines 1
         :clock-in t :clock-resume t
         )

        ;; Meeting
        ("m" "Meeting"
         entry (file "~/Org/Tasks/Meeting.org")
         "\n* MEETING %?\n\n"
         :empty-lines 1)

        ;; Diary
        ("d" "Write [D]iary"
         entry (file+datetree "~/Org/Diary/Diary.org")
         "\n* %^{Diary Title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d-%R>.org][On %<%Y-%m-%d %R>]]\n\nEvent: %?\n\n%i\n"
         :empty-lines 1
         :jump-to-captured t
         )
        
        ;; Bookmark
        ("k" "Add an URL to bookmar[k]s database"
         entry (file+headline "~/Org/Bookmarks/Bookmarks.org" "Uncategoried")
         "\n* %^{Bookmark URL}\n\n%A\n\n%?\n\n"
         :empty-lines 1
         )

        ;; org-passwords
        ("A" "[A]ccount passwords"
         entry (file "~/Org/Accounts/accounts.org.gpg")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines 1
         )

        ;; Issues, Bugs, Features
        ("b" "[B]ug"
         entry (file+olp "~/Org/Projects/Code.org" "Bugs")
         "\n* BUG %^{Bug}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("i" "[I]ssue"
         entry (file+olp "~/Org/Projects/Code.org" "Issues")
         "\n* ISSUE %^{Issue}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("f" "[F]eature"
         entry (file+olp "~/Org/Projects/Code.org" "Features")
         "\n* FEATURE %^{Feature}\n\n%i\n\n%?\n\n"
         :empty-lines 1)

        ;; code snippets
        ("s" "[S]ource Code Snippet" entry
         (file (concat org-directory "/Programming/Code Snippets/snippets.org"))
         ;; Prompt for tag and language
         "* %?%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ))


(define-key my-org-prefix (kbd "c") 'org-capture)


;;;_* Refile

;; Refile targets include this file and any file contributing to the
;; agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      ;; org-refile-use-outline-path nil
      ;; org-outline-path-complete-in-steps t
      ;; org-refile-target-verify-function nil
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


;;; [ org-protocol capture ]

(require 'org-protocol)

(setq org-capture-templates
      (append '(("P" "[P]rotocol")
                ("Pp" "[p]rotocol"
                 entry (file+headline
                        (concat org-directory "Capture/Capture.org") "Capture")
                 "* %^{Title}\nSource: %u, \n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t
                 :empty-lines 1
                 )
                ("Pl" "[l]ink"
                 entry (file+headline
                        (concat org-directory "Capture/Capture.org") "Capture")
                 "* %? [[%:link][%:description]] \nCaptured On: %U"
                 :prepend t
                 :empty-lines 1
                 )
                )
              org-capture-templates))

;;; [ org-protocol-capture-html ] -- Capture HTML from the browser selection into Emacs as org-mode content.

(use-package org-protocol-capture-html
  :quelpa (org-protocol-capture-html
           :fetcher github :repo "alphapapa/org-protocol-capture-html")
  :config
  (setq org-capture-templates
        (append '(("w" "[W]eb site: (org-protocol-capture-html)" entry
                   (file "")
                   "* %a :website:\n\n%U %?\n\n%:initial"))
                org-capture-templates))
  )


(provide 'init-my-org-capture)

;;; init-my-org-capture.el ends here
