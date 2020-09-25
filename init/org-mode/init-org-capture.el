;;; init-org-capture.el --- init for Org Capture-Refile-Archive
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Capture - Refile - Archive ]

(require 'org-capture)

(setq org-default-notes-file (concat org-directory "/Tasks/Tasks.org"))

(setq org-capture-templates
      `(("c" ,(format "%s\tsticky note"
                      (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
         entry (file "")
         ;; select todo keyword interactively from `org-todo-keywords'.
         ;; The `org-todo-keywords-for-agenda' variable is fullfilled with value AFTER generated Agenda.
         "* %(completing-read \"Todo keyword: \" org-todo-keywords-for-agenda nil t) %^{Capture} [/] \n:PROPERTIES:\n:TIME: %U\n:END: \n%i\n%a\n\n%?\n"
         ;; :time-prompt t
         :empty-lines-before 1
         :empty-lines-after 1
         :jump-to-captured t)
        
        ;; Clock task
        ("t" ,(format "%s\tstart a clock task"
                      (all-the-icons-faicon "hourglass-start" :face 'all-the-icons-red :v-adjust 0.05)
                      ;; (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust -0.1)
                      )
         entry (file "~/Org/Tasks/Tasks.org")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
         :clock-in t :clock-resume t :clock-keep t
         :empty-lines 1)
        
        ;; Diary
        ("d" ,(format "%s\twrite diary"
                      (all-the-icons-faicon "book" :face 'all-the-icons-cyan))
         entry (file+olp+datetree "~/Org/Diary/Diary.org")
         "* %^{Diary Title} %^G\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d-%R>.org][On %<%Y-%m-%d %R>]]\n\nEvent: %?\n\n%i\n\n"
         ;; :time-prompt t
         :empty-lines-before 1
         :empty-lines-after 1
         :jump-to-captured t)
        
        ;; Bookmark
        ("b" ,(format "%s\tAdd an URL to Bookmarks database"
                      (all-the-icons-faicon "bookmark" :face 'all-the-icons-yellow :v-adjust 0.05))
         entry (file "~/Org/Bookmarks/Bookmarks.org")
         "* %^{bookmark description}\n:PROPERTIES:\n:URL: %^C\n:DATE: %t\n:END: \n\n%?\n\n"
         :empty-lines 1
         :jump-to-captured t)
        
        ;; org-passwords
        ("A" ,(format "%s\trecord new account"
                      (all-the-icons-faicon "expeditedssl" :face 'all-the-icons-silver :v-adjust 0.05))
         entry (file "~/Org/Accounts/accounts.org.gpg")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines-before 1
         :empty-lines-after 1)
        
        ("W" ,(format "%s\tRecord to Beauty"
                      (all-the-icons-faicon "picture-o" :face 'all-the-icons-pink :v-adjust 0.05))
         entry (file "~/Org/Beauty/Beauty.org")
         "* %^{Name}\n   %^{DATE}p %^{GENDER}p %^{NAME(Chinese)}p %^{Name(English)}p %^{Constellation}p %^{Birthday}p %^{Address(Birth)}p %^{IMDb}p %^{Douban}p"
         :empty-lines 1
         :jump-to-captured t)
        
        ;; current buffer: in file logging
        ("L" ,(format "%s\tAdd Changelog into current file"
                      (all-the-icons-faicon "file-text-o" :face 'all-the-icons-blue :v-adjust 0.05))
         entry (file+headline (lambda () (buffer-file-name)) "Change Log")
         "* %^{Header of Changelog item}\n:PROPERTIES:\n:LOGGED: %U \n:LINK: %a \n:AUTHOR: stardiviner, email: numbchild@gmail.com\n :END:\n %?")))


;;; Context org-capture templates.
;; TODO:
;; (setq org-capture-templates-contexts
;;       '(("p" (in-mode . "message-mode"))))


;;;_* Refile

;; Refile targets include this file and any file contributing to the
;; agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 3) ; current buffer headlies
                           (org-agenda-files :maxlevel . 2) ; agenda files headlines
                           (org-buffer-list :maxlevel . 2)  ; all opened Org buffer files headlines
                           )
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      ;; org-refile-target-verify-function nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      org-refile-use-outline-path t)

;;;_* Archive

(add-to-list 'org-tag-persistent-alist '("ARCHIVE" . ?A))


(provide 'init-org-capture)

;;; init-org-capture.el ends here
