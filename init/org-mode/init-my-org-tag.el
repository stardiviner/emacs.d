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
      '((:startgroup . nil)
        ("Export options")
        (:grouptags . nil)
        ("noexport" . ?E)
        (:endgroup . nil)

        (:startgroup . nil)
        ("Task Types")
        (:grouptags . nil)
        ("fragment" . nil)
        ;; types
        ("wiki" . ?k) ("Org" . ?o) ("idea" . ?i)
        ("appointment" . ?a) ("meeting" . ?m) ("SEX" . ?X)
        ("deprecated" . ?D)
        ;; time
        ("today" . nil) ("tomorrow" . nil) ("future" . nil)
        ;; places
        ("company" . nil) ("home" . ?H)
        ("computer" . nil) ("phone" . nil)
        (:endgroup . nil)

        (:startgroup . nil)
        ("Work types" . nil)
        (:grouptags . nil)
        ("urgent" . nil)
        (:endgroup . nil)
        ))

(setq org-tag-alist
      '((:startgroup . nil)
        ("Wiki types" . nil)
        (:grouptags . nil)
        ("thought" . nil) ("philosophy" . nil) ("psychology" . nil) ("literature" . nil)
        ("computer" . nil) ("math" . nil)
        ("strategies" . nil)
        ("science" . nil) ("finance" . nil) ("business" . nil) ("economy" . nil)
        ("history" . nil) ("politics" . nil) ("society" . nil)
        ("medicine" . nil)
        (:endgroup . nil)
        
        (:startgroup . nil)
        ("Programming" . nil)
        (:grouptags . nil)
        ("code" . ?P)
        ("Linux" . ?L)
        ("Emacs" . ?e)
        ("Git" . ?G)
        (:endgroup . nil)

        (:startgroup . nil)
        ("Programming Languages" . nil)
        (:grouptags . nil)
        ("Lisp" . ?l)
        ("Clojure" . ?c)
        ("Python" . ?p)
        ("Ruby" . ?r)
        ("Shell" . ?s)
        ("JavaScript" . ?j)
        ("Go" . ?g)
        ("Rust" . nil)
        ("C" . nil)
        (:endgroup . nil)
        
        (:startgroup . nil)
        ("People" . nil)
        (:grouptags . nil)
        ("sister" . ?S)
        ("father" . ?F)
        ("mother" . ?M)
        ("relatives" . ?R)
        ("girlfriend" . ?G)
        ("workmate" . ?W)
        (:endgroup . nil)
        
        (:startgroup . nil)
        ("Projects" . nil)
        (:grouptags . nil)
        ("Agriculture" . ?A)
        (:endgroup . nil)
        ))

;; (set-face-attribute 'org-tag nil
;;                     :background (cl-case (alist-get 'background-mode (frame-parameters))
;;                                   ('light
;;                                    (color-darken-name (face-background 'default) 4))
;;                                   ('dark
;;                                    (color-lighten-name (face-background 'default) 5)))
;;                     :underline nil :weight 'normal :slant 'normal
;;                     :height 0.8
;;                     )
;;
;; (set-face-attribute 'org-tag-group nil
;;                     :background (cl-case (alist-get 'background-mode (frame-parameters))
;;                                   ('light
;;                                    (color-darken-name (face-background 'default) 4))
;;                                   ('dark
;;                                    (color-lighten-name (face-background 'default) 5)))
;;                     ;; :box '(:color "black" :line-width 2)
;;                     )

(setq org-tag-faces
      '(("noexport" :foreground "DimGray" :weight bold :underline t)
        ("deprecated" :foreground "DimGray" :weight bold :strike-through t)
        ("fragment" :foreground "white" :weight bold)
        ("computer" :foreground "green" :background "black")
        ("life" :foreground "black" :background "DimGray")
        ("SEX" :foreground "deep pink" :weight bold)
        ("code" :foreground "lawn green" :weight bold)
        ("Linux" :foreground "yellow" :weight bold)
        ("Mac" :foreground "#444444" :background "black" :weight bold)
        ("Emacs" :foreground "dodger blue" :weight bold)
        ("Org" :foreground "green yellow")
        ("Lisp" :foreground "deep pink" :weight bold)
        ("Clojure" :foreground "sky blue" :weight bold)
        ("Python" :foreground "yellow" :weight bold)
        ("Ruby" :foreground "red" :weight bold)
        ("Shell" :foreground "sea green")
        ("C" :foreground "SaddleBrown" :weight bold)
        ("Go" :foreground "gold" :weight bold)
        ("Rust" :foreground "WhiteSmoke" :weight bold)
        ("JavaScript" :foreground "yellow")
        ))


(provide 'init-my-org-tag)

;;; init-my-org-tag.el ends here
