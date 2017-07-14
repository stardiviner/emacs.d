;;; init-my-org-tag.el --- init for Org Tags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; Tags

(setq org-auto-align-tags t
      org-export-with-tags t
      org-tags-column -77
      ;; inheritance
      org-use-tag-inheritance t
      org-tags-match-list-sublevels t
      org-fast-tag-selection-single-key nil ; multiple different group tags selecting.
      ;; org-tags-exclude-from-inheritance '(("knowledge" . nil))
      )

;; enable group tags
(setq org-group-tags t)

(setq org-tag-persistent-alist
      '( ;; personal
        (:startgroup . nil)
        ("types")
        (:grouptags . nil)
        ;; types
        ("Wiki" . ?k) ("Org" . ?o) ("Idea" . ?i)
        ("appointment" . ?a) ("meeting" . ?m) ("SEX" . ?X)
        ;; time
        ("Today" . nil) ("Tomorrow" . nil) ("Future" . nil)
        ;; places
        ("Company" . ?C) ("home" . ?H) ("computer" . ?P) ("phone" . ?P)
        (:endgroup . nil)
        ;; Work
        (:startgroup . nil)
        ("Work" . ?w)
        (:grouptags . nil)
        ;; work task types
        ("urgent" . nil)
        (:endgroup . nil)
        ;; task types
        (:startgroup . nil)
        ("Task" . nil)
        (:grouptags . nil)
        ("fragment" . nil)
        (:endgroup . nil)
        ))

(setq org-tag-alist
      '(;; Knowledge aspects
        (:startgroup . nil)
        ("knowledge" . nil)
        (:grouptags . nil)
        ("Thought" . nil) ("Philosophy" . nil) ("Psychology" . nil) ("Literature" . nil)
        ("Computer" . nil) ("Math" . nil)
        ("Strategies" . nil)
        ("Science" . nil) ("Finance" . nil) ("Business" . nil) ("Economy" . nil)
        ("History" . nil) ("Politics" . nil) ("Society" . nil)
        ("Medicine" . nil)
        (:endgroup . nil)
        ;; Programming
        (:startgroup . nil)
        ("@Programming" . ?p)
        (:grouptags . nil)
        ("code" . ?c)
        ("Linux" . ?l)
        ("Emacs" . ?e)
        ("Git" . ?g)
        (:endgroup . nil)
        ;; Family
        (:startgroup . nil)
        ("@Family" . nil)
        (:grouptags . nil)
        ("Sister" . ?S)
        ("Father" . ?F)
        ("Mother" . ?M)
        ("Relatives" . ?R)
        ("GirlFriend" . ?G)
        (:endgroup . nil)
        ;; Projects
        (:startgroup . nil)
        ("knowledge" . nil)
        (:grouptags . nil)
        ("Agriculture" . ?A)
        (:endgroup . nil)
        ))

(setq org-tag-faces
      '(("Org" :foreground "green yellow")
        ("Computer" :foreground "green" :background "black")
        ("Life" :foreground "black" :background "DimGray")
        ("SEX" :foreground "deep pink" :weight bold)
        ("code" :foreground "lawn green" :weight bold)
        ("Linux" :foreground "yellow" :weight bold)
        ("Mac" :foreground "#444444" :background "black" :weight bold)
        ("Emacs" :foreground "dodger blue" :weight bold)
        ("Lisp" :foreground "deep pink" :weight bold)
        ("Ruby" :foreground "red" :weight bold)
        ("Python" :foreground "yellow" :weight bold)
        ("C/C++" :foreground "gold" :weight bold)
        ("Go" :foreground "gold" :weight bold)
        ("Clojure" :foreground "sky blue" :weight bold)
        ("Elixir" :foreground "dark magenta" :weight bold)
        ("Shell" :foreground "sea green")
        ))



(provide 'init-my-org-tag)

;;; init-my-org-tag.el ends here
