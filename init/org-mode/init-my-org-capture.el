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
         "\n* TODO %^{Capture}\n\n%i\n\n%a\n\n%?"
         :prepend t
         :empty-lines 1
         )

        ;; Tasks
        ("t" "Add a task into Tasks"
         entry (file "~/Org/Projects/Computer Todos.org")
         ;; "\n* TODO %^{Task} [/]\n\n%?\n\n"
         "\n* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
         :empty-lines 1
         )
        ("T" "Clock in a New Task"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{Clock Task} [/]\n\n%?\n\n"
         :empty-lines 1
         :clock-in t :clock-resume t
         )

        ;; Diary
        ("D" "Write Diary"
         entry (file+datetree "~/Org/Diary/Diary.org")
         "\n* %^{Diary Title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d-%R>.org][On %<%Y-%m-%d %R>]]\n\nEvent: %?\n\n%i"
         :empty-lines 1
         :jump-to-captured t
         )
        
        ;; Bookmark
        ("k" "Add an URL to bookmarks database"
         entry (file+headline "~/Org/Bookmarks/Bookmarks.org" "Uncategoried")
         "\n* %^{Bookmark URL}\n\n%A\n\n%?\n\n"
         :empty-lines 1
         )

        ;; org-passwords
        ("P" "password"
         entry (file "~/Org/Accounts/accounts.org.gpg")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines 1
         )

        ;; Issues, Bugs, Features
        ("b" "Bug"
         entry (file+olp "~/Org/Projects/Code.org" "Bugs")
         "\n* BUG %^{Bug}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("i" "Issue"
         entry (file+olp "~/Org/Projects/Code.org" "Issues")
         "\n* ISSUE %^{Issue}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("f" "Feature"
         entry (file+olp "~/Org/Projects/Code.org" "Features")
         "\n* FEATURE %^{Feature}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ))


(define-key my-org-prefix (kbd "c") 'org-capture)


;;;_* Refile

;; Refile targets include this file and any file contributing to the
;; agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5))
      ;; Targets start with the file name - allows creating level 1 tasks
      ;; org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps nil
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
      (append '(("P" "Protocol")
                ("Pp" "protocol"
                 entry (file+headline
                        (concat org-directory "Capture/Capture.org") "Capture")
                 "* %^{Title}\nSource: %u, \n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t
                 :empty-lines 1
                 )
                ("Pl" "link"
                 entry (file+headline
                        (concat org-directory "Capture/Capture.org") "Capture")
                 "* %? [[%:link][%:description]] \nCaptured On: %U"
                 :prepend t
                 :empty-lines 1
                 )
                )
              org-capture-templates))


(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))


(provide 'init-my-org-capture)

;;; init-my-org-capture.el ends here
