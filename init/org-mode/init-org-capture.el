;;; init-org-capture.el --- init for Org Capture-Refile-Archive
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Capture - Refile - Archive ]

(require 'org-capture)

(setq org-default-notes-file
      (concat org-directory "/Tasks/Tasks.org"))

(setq org-capture-templates
      '(("c" "[c]apture"
         entry (file "")
         ;; select todo keyword interactively from `org-todo-keywords'.
         "* %(completing-read \"Todo keyword: \" org-todo-keywords-for-agenda nil t) %^{Capture} \n:PROPERTIES:\n:TIME: %U\n:END: \n%i\n%a\n\n%?"
         ;; :time-prompt t
         :empty-lines-before 1
         :empty-lines-after 1)

        ;; Tasks
        ("t" "Add a [t]ime scheduled task into Tasks"
         entry (file "~/Org/Tasks/Computer Todos.org")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
         :clock-in t :clock-resume t :clock-keep t
         :empty-lines 1)

        ;; Diary
        ("d" "Write [d]iary"
         entry (file+olp+datetree "~/Org/Diary/Diary.org")
         "* %^{Diary Title}\n:PROPERTIES:\n:TIME: %U\n:END: \n\n[[file:%<%Y-%m-%d-%R>.org][On %<%Y-%m-%d %R>]]\n\nEvent: %?\n\n%i\n\n"
         ;; :time-prompt t
         :empty-lines-before 1
         :empty-lines-after 1
         :jump-to-captured t)
        
        ;; Bookmark
        ("u" "Add an [U]RL to bookmarks database"
         entry (file "~/Org/Bookmarks/Bookmarks.org")
         "* [[%^C][%^{link description}]]\n:PROPERTIES:\n:URL: %^C\n:DATE: %t\n:END: \n\n%?\n\n"
         :empty-lines 1
         :jump-to-captured t)

        ;; org-passwords
        ("A" "[A]ccount passwords"
         entry (file "~/Org/Accounts/accounts.org.gpg")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines-before 1
         :empty-lines-after 1)

        ("B" "[B]eauty"
         entry (file "~/Org/Beauty/Beauty.org")
         "* %^{Name}\n   %^{DATE}p %^{GENDER}p %^{NAME(Chinese)}p %^{Name(English)}p %^{Constellation}p %^{Birthday}p %^{Address(Birth)}p %^{IMDb}p %^{Douban}p"
         :empty-lines 1
         :jump-to-captured t)

        ;; current buffer: in file logging
        ("L" "Add Change[L]og into current buffer file"
         entry (file+headline (lambda () (buffer-file-name)) "Change Log")
         "* %^{Header of Changelog item}\n:PROPERTIES:\n:LOGGED: %U \n:LINK: %a \n:AUTHOR: stardiviner, email: numbchild@gmail.com\n :END:\n %?")))

;;; code snippets capture template
(defun my/org-capture-get-src-block-string (major-mode)
  "Given a major mode symbol, return the associated org-src block
string that will enable syntax highlighting for that language

E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

(defun my/org-capture-code-snippet (f)
  (with-current-buffer (find-buffer-visiting f)
    (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
          (func-name (read-from-minibuffer "Function name: "))
          (file-name (buffer-file-name))
          (line-number (line-number-at-pos (region-beginning)))
          (org-src-mode (my/org-capture-get-src-block-string major-mode)))
      (format
       "file:%s::%s
In ~%s~:

#+begin_src %s
%s
#+end_src"
       file-name
       line-number
       func-name
       org-src-mode
       code-snippet))))

;; use region select to capture.
(add-to-list 'org-capture-templates
             '("s" "code [s]nippet" entry
               (file (lambda () (concat org-directory "/Programming Code/Code Snippets/snippets.org")))
               "* %?\n%(my/org-capture-code-snippet \"%F\")"))


;;; Context org-capture templates.
;; TODO:
;; (setq org-capture-templates-contexts
;;       '(("p" (in-mode . "message-mode"))))


;;;_* Refile

;; Refile targets include this file and any file contributing to the
;; agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 2))
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      ;; org-refile-target-verify-function nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t
      org-refile-use-outline-path t)

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

(leaf org-protocol-capture-html
  :doc "Capture HTML from the browser selection into Emacs as org-mode content."
  :el-get (org-protocol-capture-html :url "https://github.com/alphapapa/org-protocol-capture-html.git")
  :init (setq org-capture-templates (append
                                     '(("PH" "org-[p]rotocol-capture-[h]tml" entry
                                        (file "")
                                        "* %a :website:\n\n%U %?\n\n%:initial"))
                                     org-capture-templates)))


(provide 'init-org-capture)

;;; init-org-capture.el ends here
