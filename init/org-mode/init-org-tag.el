;;; init-org-tag.el --- init for Org Tags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; Tags

(setq org-auto-align-tags nil)

(setq org-tag-persistent-alist
      '(("ARCHIVE" . ?A)
        ("noexport" . ?E)
        ("private" . ?P)
        ("deprecated" . ?D)
        ("outdated" . ?O)))

;; auto add tag "LOG" for [C-c C-z] add log note.
(add-to-list 'org-tag-persistent-alist '("LOG" . ?z))
(defun my/org-add-note--auto-add-tag ()
  "Auto add tag 'NOTE' when add note log."
  (org-back-to-heading)
  ;; remove multiple same "LOG" tags
  (autoload 'seq-uniq "seq.el")
  (org-set-tags (seq-uniq (cons "LOG" (org-get-tags nil t)))))
(advice-add 'org-add-note :after #'my/org-add-note--auto-add-tag)

;;; for Org Book reading journal. Mark an headline as a book
(add-to-list 'org-tag-persistent-alist '("book" . ?h))
(add-to-list 'org-default-properties "AUTHOR")
(add-to-list 'org-default-properties "PUBLISH_DATE")
(add-to-list 'org-default-properties "PRESS")

(setq org-tag-alist
      '((:startgroup) ("Org" . ?o) ("idea" . ?i) (:endgroup)

        ;; Task
        (:startgrouptag) ("@task" . ?t) (:grouptags)
        ("event") ("check") ("alert") ("flag") ("tag") ("question") ("info") ("quote")
        ("star") ("heart") ("smile") ("like") ("trash")
        ("lock") ("unlock") ("key")
        ("refresh") ("repeat") ("inprogress") ("fragment")
        (:endgrouptag)

        ;; Life
        (:startgrouptag) ("@life" . nil) (:grouptags)
        ("talk") ("call") ("shopping") ("express")
        ("finance") ("money") ("credit_card") ("btc")
        ("bee") ("coffee")
        ("hospital") ("medical") ("health")
        ("law") ("government") ("censorship") ("complaint")
        ("watch") ("book" . ?h) ("bookmark") ("note") ("paperclip")
        ("file") ("archive") ("document") ("word") ("excel") ("powerpoint") ("pdf")
        ("image") ("video") ("audio")
        ("search") ("download") ("email") ("reply") ("share") ("rss")
        ("map") ("location")
        ("picture") ("music") ("film") ("headphone") ("game")
        ("camera") ("vlog") ("record")
        ("chart")
        ("bicycle") ("car") ("bus") ("subway") ("plane")
        ("travel")
        ("child")
        (:endgrouptag)
        
        ;; SEX
        (:startgrouptag) ("@SEX" . ?X) (:grouptags)
        ("date") ("pickup")
        (:endgrouptag)
        ;; Time
        (:startgroup) ("today" . nil) ("tomorrow" . nil) ("future" . nil) (:endgroup)
        ;; Places
        (:startgroup) ("company") ("home") (:endgroup)
        ;; Devices
        (:startgroup)
        ("computer") ("laptop") ("mobile")
        ("firefox") ("chrome")
        (:endgroup)
        ;; Work
        (:startgrouptag) ("@work" . ?w) (:grouptags)
        ("appointment" . ?a) ("meeting" . ?m) ("urgent" . ?u)
        (:endgrouptag)

        (:startgrouptag) ("@wiki" . ?k) (:grouptags)
        ("thought") ("philosophy") ("psychology") ("literature")
        ("computer_science") ("math")
        ("strategies")
        ("science") ("finance") ("business") ("economy")
        ("history") ("politics") ("society")
        ("medicine")
        (:endgrouptag)
        
        (:startgrouptag) ("@programming") (:grouptags)
        ("code" . ?C) ("source_code") ("bug") ("Emacs" . ?e) ("git" . ?G) ("github")
        (:startgroup)
        ("hardware") ("usb") ("terminal") ("cloud") ("DevOps") ("security") ("disk")
        ("Linux" . ?L) ("Apple") ("macOS") ("Windows") ("Android") ("iOS")
        ("LISP" . nil) ("Common_Lisp" . ?l) ("Clojure" . ?c) ("ClojureScript" . ?s)
        ("Shell" . ?S) ("Python" . ?p) ("Ruby" . ?r)
        ("JavaScript" . ?J) ("HTML") ("HTML5") ("CSS") ("CSS3")
        ("Java" . ?j) ("C") ("cpp") ("Go") ("Rust") ("Swift") ("C#")
        ("database" . ?d) ("SQL") ("PostgreSQL") ("MySQL") ("MariaDB") ("Oracle")
        ("TeX") ("LaTeX")
        ;; Internet
        ("Internet") ("Google") ("Facebook") ("Tencent")
        (:endgroup)
        (:endgrouptag)
        
        (:startgroup)
        (:startgrouptag) ("@family") (:grouptags)
        ("sister") ("father") ("mother")
        (:endgrouptag)
        (:startgrouptag) ("@relatives") (:endgrouptag)
        (:startgrouptag) ("@girlfriend") (:endgrouptag)
        (:startgrouptag) ("@workmate") (:endgrouptag)
        (:startgrouptag) ("@friend") (:grouptags)
        ("good_friend") ("friend") ("person_who_knows")
        (:endgrouptag)
        (:endgroup)
        
        (:startgrouptag) ("@project") (:grouptags)
        ("agriculture")
        (:endgrouptag)

        (:startgrouptag) ("@company") (:grouptags)
        ("users") ("promotion") ("copyright")
        (:endgrouptag)
        ))

(setq org-tag-faces
      '(("noexport" :foreground "DimGray" :weight bold :underline t :strike-through t)
        ("deprecated" :foreground "DimGray" :strike-through t)
        ("LOG" :foreground "DeepSkyBlue")
        ("private" :foreground "deep pink")
        ("book" :foreground "deep pink")
        ("fragment" :foreground "LightGray" :weight bold)
        ("computer" :foreground "green")
        ("@life" :foreground "black")
        ("@work" :foreground "DeepSkyBlue")
        ("@SEX" :foreground "deep pink" :weight bold)
        ("@Programming" :foreground "lawn green" :weight bold)
        ("Linux" :foreground "yellow" :weight bold)
        ("Mac" :foreground "#444444" :weight bold)
        ("Emacs" :foreground "dodger blue" :weight bold)
        ("Org" :foreground "green yellow" :weight bold)
        ("Hacker" :foreground "OrangeRed" :weight bold)
        ("LISP" :foreground "deep pink" :weight bold)
        ("Common_Lisp" :foreground "sky blue" :weight bold)
        ("Clojure" :foreground "sky blue" :weight bold)
        ("ClojureScript" :foreground "sky blue" :weight bold)
        ("Python" :foreground "yellow" :weight bold)
        ("Ruby" :foreground "red" :weight bold)
        ("Shell" :foreground "sea green")
        ("Java" :foreground "royal blue" :weight bold)
        ("C" :foreground "SaddleBrown" :weight bold)
        ("Go" :foreground "gold" :weight bold)
        ("Rust" :foreground "WhiteSmoke" :weight bold)
        ("JavaScript" :foreground "yellow" :weight bold)))

;; ;;; `org-archive-tag', `org-archived'
;; (defconst org-deprecated-tag "deprecated"
;;   "The tag that marks a subtree as archived.
;; An archived subtree does not open during visibility cycling, and does
;; not contribute to the agenda listings.")
;;
;; (defface org-deprecated '((t :inherit shadow))
;;   "Face for headline with the deprecated tag."
;;   :group 'org-faces)

;;; [ org-pretty-tags ] -- Surrogates for Org tags.

(use-package org-pretty-tags
  :ensure t
  :defer t
  :commands (org-pretty-tags-mode org-pretty-tags-global-mode)
  :init (org-pretty-tags-global-mode t)
  (setq org-tag-faces nil)
  (setq org-pretty-tags-surrogate-strings
        `(("ARCHIVE" . ,(all-the-icons-faicon "archive"))
          ("noexport" . ,(all-the-icons-faicon "ban"))
          ("deprecated" . ,(all-the-icons-faicon "bell-slash-o"))
          ("private" . ,(all-the-icons-faicon "lock"))
          ;; -----------------------------------------------------
          ("Org" . ,(all-the-icons-fileicon "org"))
          ("Emacs" . ,(all-the-icons-fileicon "emacs"))
          ;; -----------------------------------------------------
          ("LOG" . ,(all-the-icons-faicon "comment-o"))
          ("event" . ,(all-the-icons-faicon "calendar-check-o"))
          ("check" . ,(all-the-icons-faicon "check-square-o"))
          ("alert" . ,(all-the-icons-faicon "bell-o"))
          ("flag" . ,(all-the-icons-faicon "flag"))
          ("tag" . ,(all-the-icons-faicon "tag"))
          ("question" . ,(all-the-icons-faicon "question-circle-o"))
          ("info" . ,(all-the-icons-faicon "info-circle"))
          ("quote" . ,(all-the-icons-faicon "quote-left"))
          ("lock" . ,(all-the-icons-faicon "lock"))
          ("unlock" . ,(all-the-icons-faicon "unlock"))
          ("key" . ,(all-the-icons-faicon "key"))
          ("refresh" . ,(all-the-icons-faicon "refresh"))
          ("repeat" . ,(all-the-icons-faicon "repeat"))
          ("star" . ,(all-the-icons-faicon "star"))
          ("heart" . ,(all-the-icons-faicon "heart-o"))
          ("smile" . ,(all-the-icons-faicon "smile-o"))
          ("like" . ,(all-the-icons-faicon "thumbs-o-up"))
          ("trash" . ,(all-the-icons-faicon "trash-o"))
          ("inprogress" . ,(all-the-icons-faicon "spinner"))
          ;; -----------------------------------------------------
          ;; Life
          ;; ("@life" . ,(all-the-icons-faicon ""))
          ("talk" . ,(all-the-icons-faicon "comments-o"))
          ("call" . ,(all-the-icons-faicon "phone-square"))
          ("@SEX" . ,(all-the-icons-faicon "female"))
          ("home" . ,(all-the-icons-faicon "home"))
          ("shopping" . ,(all-the-icons-faicon "shopping-cart"))
          ("express" . ,(all-the-icons-faicon "truck"))
          ("finance" . ,(all-the-icons-faicon "money"))
          ("money" . ,(all-the-icons-faicon "money"))
          ("credit_card" . ,(all-the-icons-faicon "credit-card"))
          ("visa" . ,(all-the-icons-faicon "cc-visa"))
          ("mastercard" . ,(all-the-icons-faicon "cc-mastercard"))
          ("btc" . ,(all-the-icons-faicon "btc"))
          ("stripe" . ,(all-the-icons-faicon "cc-stripe"))
          ("watch" . ,(all-the-icons-faicon "eye"))
          ("hospital" . ,(all-the-icons-faicon "hospital-o"))
          ("medical" . ,(all-the-icons-faicon "medkit"))
          ("health" . ,(all-the-icons-faicon "medkit"))
          ("law" . ,(all-the-icons-faicon "gavel"))
          ("government" . ,(all-the-icons-faicon "building-o"))
          ("censorship" . ,(all-the-icons-faicon "assistive-listening-systems"))
          ("complaint" . ,(all-the-icons-faicon "bullhorn"))
          ("chart" . ,(all-the-icons-faicon "bar-chart"))
          ("map" . ,(all-the-icons-faicon "map-pin"))
          ("location" . ,(all-the-icons-faicon "map-marker"))
          ("bee" . ,(all-the-icons-faicon "beer"))
          ("coffee" . ,(all-the-icons-faicon "coffee"))
          ("bicycle" . ,(all-the-icons-faicon "bicycle"))
          ("car" . ,(all-the-icons-faicon "car"))
          ("bus" . ,(all-the-icons-faicon "bus"))
          ("subway" . ,(all-the-icons-faicon "subway"))
          ("plane" . ,(all-the-icons-faicon "plane"))
          ("travel" . ,(all-the-icons-faicon "suitcase"))
          ("child" . ,(all-the-icons-faicon "child"))
          ;; -----------------------------------------------------
          ("file" . ,(all-the-icons-faicon "file"))
          ("archive" . ,(all-the-icons-faicon "file-archive-o"))
          ("document" . ,(all-the-icons-faicon "file-text-o"))
          ("word" . ,(all-the-icons-faicon "file-word-o"))
          ("excel" . ,(all-the-icons-faicon "file-excel-o"))
          ("powerpoint" . ,(all-the-icons-faicon "file-powerpoint-o"))
          ("image" . ,(all-the-icons-faicon "file-image-o"))
          ("video" . ,(all-the-icons-faicon "file-video-o"))
          ("audio" . ,(all-the-icons-faicon "file-audio-o"))
          ("pdf" . ,(all-the-icons-faicon "file-pdf-o"))
          ("book" . ,(all-the-icons-faicon "book"))
          ("bookmark" . ,(all-the-icons-faicon "bookmark"))
          ("file" . ,(all-the-icons-faicon "file-text-o"))
          ("note" . ,(all-the-icons-faicon "pencil"))
          ("paperclip" . ,(all-the-icons-faicon "paperclip"))
          ("search" . ,(all-the-icons-faicon "search"))
          ("download" . ,(all-the-icons-faicon "download"))
          ("email" . ,(all-the-icons-faicon "envelope-o"))
          ("reply" . ,(all-the-icons-faicon "reply"))
          ("share" . ,(all-the-icons-faicon "share-square-o"))
          ("rss" . ,(all-the-icons-faicon "rss"))
          ("picture" . ,(all-the-icons-faicon "picture-o"))
          ("music" . ,(all-the-icons-faicon "music"))
          ("headphone" . ,(all-the-icons-faicon "headphones"))
          ("film" . ,(all-the-icons-faicon "film"))
          ("game" . ,(all-the-icons-faicon "gamepad"))
          ("camera" . ,(all-the-icons-faicon "camera-retro"))
          ("vlog" . ,(all-the-icons-faicon "video-camera"))
          ("record" . ,(all-the-icons-faicon "microphone"))
          ;; -----------------------------------------------------
          ;; Work
          ("work" . ,(all-the-icons-faicon "black-tie"))
          ("print" . ,(all-the-icons-faicon "print"))
          ("business_trip" . ,(all-the-icons-faicon "briefcase"))
          ;; -----------------------------------------------------
          ;; Programming
          ("@programming" . ,(all-the-icons-faicon "code"))
          ("code" . ,(all-the-icons-faicon "code"))
          ("source_code" . ,(all-the-icons-faicon "file-code-o"))
          ("bug" . ,(all-the-icons-faicon "bug"))
          ("coding" . ,(all-the-icons-faicon "keyboard-o"))
          ("git" . ,(all-the-icons-faicon "git"))
          ("github" . ,(all-the-icons-faicon "github"))
          ("database" . ,(all-the-icons-faicon "database"))
          ("computer" . ,(all-the-icons-faicon "laptop"))
          ("laptop" . ,(all-the-icons-faicon "laptop"))
          ("mobile" . ,(all-the-icons-faicon "mobile"))
          ("hardware" . ,(all-the-icons-faicon "desktop"))
          ("usb" . ,(all-the-icons-faicon "usb"))
          ("firefox" . ,(all-the-icons-faicon "firefox"))
          ("chrome" . ,(all-the-icons-faicon "chrome"))
          ("terminal" . ,(all-the-icons-faicon "terminal"))
          ("cloud" . ,(all-the-icons-faicon "cloud"))
          ("DevOps" . ,(all-the-icons-faicon "cogs")) ; "sitemap"
          ("security" . ,(all-the-icons-faicon "shield"))
          ("disk" . ,(all-the-icons-faicon "hdd-o"))
          ;; -----------------------------------------------------
          ;; Systems
          ("Linux" . ,(all-the-icons-faicon "linux"))
          ("Apple" . ,(all-the-icons-faicon "apple"))
          ("macOS" . ,(all-the-icons-faicon "apple"))
          ("Windows" . ,(all-the-icons-faicon "windows"))
          ("Android" . ,(all-the-icons-faicon "android"))
          ;; -----------------------------------------------------
          ;; Programming Languages
          ("Shell" . ,(all-the-icons-alltheicon "script"))
          ("LISP" . ,(all-the-icons-fileicon "lisp"))
          ("Common_Lisp" . ,(all-the-icons-fileicon "clisp"))
          ("Clojure" . ,(all-the-icons-alltheicon "clojure-line"))
          ("ClojureScript" . ,(all-the-icons-fileicon "cljs"))
          ("Python" . ,(all-the-icons-alltheicon "python"))
          ("Haskell" . ,(all-the-icons-alltheicon "haskell"))
          ("C" . ,(all-the-icons-alltheicon "c-line"))
          ("cpp" . ,(all-the-icons-alltheicon "cplusplus-line"))
          ("C#" . ,(all-the-icons-alltheicon "csharp-line"))
          ("Java" . ,(all-the-icons-alltheicon "java"))
          ("Go" . ,(all-the-icons-alltheicon "go"))
          ("Rust" . ,(all-the-icons-alltheicon "rust"))
          ("Swift" . ,(all-the-icons-alltheicon "swift"))
          ("JavaScript" . ,(all-the-icons-alltheicon "javascript"))
          ("HTML" . ,(all-the-icons-alltheicon "html5"))
          ("HTML5" . ,(all-the-icons-alltheicon "html5"))
          ("CSS" . ,(all-the-icons-alltheicon "css3"))
          ("CSS3" . ,(all-the-icons-alltheicon "css3"))
          ("SQL" . ,(all-the-icons-faicon "database"))
          ("PostgreSQL" . ,(all-the-icons-alltheicon "postgresql"))
          ("TeX" . ,(all-the-icons-fileicon "tex"))
          ("LaTeX" . ,(all-the-icons-fileicon "tex"))
          ("Spring" . ,(all-the-icons-alltheicon "spring"))
          ("AWS" . ,(all-the-icons-alltheicon "aws"))
          ;; -----------------------------------------------------
          ;; Project
          ("@project" . ,(all-the-icons-faicon "bolt"))
          ;; -----------------------------------------------------
          ;; Family
          ("@family" . ,(all-the-icons-material "home"))
          ;; -----------------------------------------------------
          ;; Company
          ("users" . ,(all-the-icons-faicon "users"))
          ("promotion" . ,(all-the-icons-faicon "bullhorn"))
          ("copyright" . ,(all-the-icons-faicon "copyright"))
          ;; -----------------------------------------------------
          ;; Internet
          ("Internet" . ,(all-the-icons-faicon "globe"))
          ("Google" . ,(all-the-icons-faicon "google"))
          ("Facebook" . ,(all-the-icons-faicon "facebook-official"))
          ("Amazon" . ,(all-the-icons-faicon "amazon"))
          ("Tencent" . ,(all-the-icons-faicon "qq")))))


(provide 'init-org-tag)

;;; init-org-tag.el ends here
