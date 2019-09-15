;;; init-org-tag.el --- init for Org Tags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; Tags

;; suitable tag position length for 12' inch screen size split window.
;; 11.2' screen size use -65
(setq org-tags-column -65)
;; (setq org-tags-column (- (- (/ (/ (display-pixel-width) 2) 10) 3)))


(setq org-tag-persistent-alist
      '(("noexport" . ?E)
        ("deprecated" . ?D)
        ("outdated" . ?O)))

(setq org-tag-alist
      '((:startgroup) ("Org" . ?o) ("Idea" . ?i) (:endgroup)
        ;; Task
        (:startgrouptag) ("@task" . ?t) (:grouptags) ("fragment" . ?f) ("@life" . nil) (:endgrouptag)
        ;; SEX
        (:startgrouptag) ("@SEX" . ?X) (:grouptags) ("PickUp" . nil) (:endgrouptag)
        ;; Time
        (:startgroup) ("today" . nil) ("tomorrow" . nil) ("future" . nil) (:endgroup)
        ;; Places
        (:startgroup) ("company" . ?Y) ("home" . ?H) (:endgroup)
        ;; Devices
        (:startgroup) ("computer") ("phone") ("iPad") (:endgroup)

        (:startgrouptag)
        ("@Work" . ?w)
        (:grouptags)
        ("appointment" . ?a) ("meeting" . ?m) ("urgent" . ?u)
        (:endgrouptag)

        (:startgrouptag)
        ("@wiki" . ?k)
        (:grouptags)
        ("@thought") ("@philosophy") ("@psychology") ("@literature")
        ("@computer") ("@math")
        ("@strategies")
        ("@science") ("@finance") ("@business") ("@economy")
        ("@history") ("@politics") ("@society")
        ("@medicine")
        (:endgrouptag)
        
        (:startgrouptag)
        ("@Programming")
        (:grouptags) ("@Code" . ?C) ("@Linux" . ?L) ("@Emacs" . ?e) ("Git" . ?G)
        (:startgroup)
        ("@Lisp" . ?l) ("Clojure" . ?c)
        ("Python" . ?p) ("Ruby" . ?r) ("Shell" . ?s)
        ("JavaScript" . ?j)
        ("Java" . "?J") ("C" . nil) ("Go" . ?g) ("Rust" . nil)
        (:endgroup)
        (:endgrouptag)
        
        (:startgroup)
        (:startgrouptag) ("@family") (:grouptags) ("sister") ("father") ("mother") (:endgrouptag)
        (:startgrouptag) ("@relatives") (:endgrouptag)
        (:startgrouptag) ("@girlfriend" . ?x) (:endgrouptag)
        (:startgrouptag) ("@workmate" . ?W) (:endgrouptag)
        (:endgroup)
        
        (:startgrouptag)
        ("@Project" . ?P)
        (:grouptags) ("Agriculture") (:endgrouptag)

        (:startgrouptag)
        ("Love")
        (:grouptags) ("陈晓影") ("芈兰") (:endgrouptag)
        ))

(setq org-tag-faces
      '(("noexport" :foreground "DimGray" :weight bold :underline t :strike-through t)
        ("deprecated" :foreground "DimGray" :strike-through t)
        ("fragment" :foreground "LightGray" :weight bold)
        ("computer" :foreground "green")
        ("@life" :foreground "black")
        ("@work" :foreground "DeepSkyBlue")
        ("@SEX" :foreground "deep pink" :weight bold)
        ("@Code" :foreground "lawn green" :weight bold)
        ("@Linux" :foreground "yellow" :weight bold)
        ("@Mac" :foreground "#444444" :weight bold)
        ("@Emacs" :foreground "dodger blue" :weight bold)
        ("Org" :foreground "green yellow" :weight bold)
        ("@Lisp" :foreground "deep pink" :weight bold)
        ("Clojure" :foreground "sky blue" :weight bold)
        ("Python" :foreground "yellow" :weight bold)
        ("Ruby" :foreground "red" :weight bold)
        ("Shell" :foreground "sea green")
        ("Java" :foreground "royal blue" :weight bold)
        ("C" :foreground "SaddleBrown" :weight bold)
        ("Go" :foreground "gold" :weight bold)
        ("Rust" :foreground "WhiteSmoke" :weight bold)
        ("JavaScript" :foreground "yellow" :weight bold)
        ))

;; ;;; `org-archive-tag', `org-archived'
;; (defconst org-deprecated-tag "deprecated"
;;   "The tag that marks a subtree as archived.
;; An archived subtree does not open during visibility cycling, and does
;; not contribute to the agenda listings.")
;;
;; (defface org-deprecated '((t :inherit shadow))
;;   "Face for headline with the deprecated tag."
;;   :group 'org-faces)


(provide 'init-org-tag)

;;; init-org-tag.el ends here
