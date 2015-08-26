;;; init-my-org-tag.el --- init for Org Tags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; Tags

(setq org-auto-align-tags t
      org-export-with-tags t
      org-tags-history t
      org-tags-column -77
      ;; inheritance
      org-use-tag-inheritance t
      org-tags-match-list-sublevels t
      org-fast-tag-selection-single-key nil
      ;; org-tags-exclude-from-inheritance '(("knowledge" . nil))
      )

(setq org-tag-persistent-alist
      '( ;; personal
        (:startgroup . nil)
        ("types")
        (:grouptags . nil)
        ;; types
        ("Wiki" . ?k) ("Org" . ?o) ("Idea" . ?i)
        ("appointment" . ?a) ("meeting" . ?m) ("SEX" . ?X)
        ;; time
        ("Today" . ?D) ("tomorrow" . ?T) ("future" . ?F)
        ;; places
        ("Company" . ?C) ("Home" . ?H) ("Computer" . ?P) ("Phone" . ?P)
        (:endgroup . nil)
        ;; Work
        (:startgroup . nil)
        ("Work" . ?w)
        (:grouptags . nil)
        ;; work task types
        ("Urgent" . nil)
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
        ))

;; enable group tags
(setq org-group-tags t)

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

(set-face-attribute 'org-tag-group nil
                    :foreground "white" :background "dim gray"
                    :box '(:color "black" :line-width 2))
(set-face-attribute 'org-tag nil
                    :foreground "gray")


;; tag changes that should be triggered by TODO state changes.
;; TODO: search github `org-todo-state-tags-triggers' for examples.
;; (setq org-todo-state-tags-triggers
;;       '(("" ("Task" . t))
;;         ('todo . ("Doing" . t))
;;         ('done . ("Done" . nil))
;;         ))



(provide 'init-my-org-tag)

;;; init-my-org-tag.el ends here
