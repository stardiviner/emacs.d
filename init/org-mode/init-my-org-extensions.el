;;; init-my-org-extensions.el --- init for Org Extensions
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-attach ] -- Manage file attachments to org-mode tasks.

;; - [C-c C-a] :: `org-attach'.
;; - drag & drop

(require 'org-attach)

(setq org-attach-directory "data/"
      org-attach-archive-delete 'query
      org-attach-allow-inheritance t
      org-attach-auto-tag nil
      )


;;; [ org-plot ] -- Support for plotting from Org-mode.

(require 'org-plot)


;;; [ org-bbdb ] -- Support for links to BBDB entries from within Org-mode.

;; - [C-c C-l] + `bbdb:' link.

;; (require 'org-bbdb)
;;
;; ;; 'anniversary, 'birthday,
;; (setq org-bbdb-anniversary-field 'birthday
;;       org-bbdb-default-anniversary-format "birthday"
;;       org-bbdb-anniversary-format-alist
;;       '(("birthday" lambda
;;          (name years suffix)
;;          (concat "Birthday: [[bbdb:" name "][" name " ("
;;                  (format "%s" years)
;;                  suffix ")]]"))
;;         ("wedding" lambda
;;          (name years suffix)
;;          (concat "[[bbdb:" name "][" name "'s "
;;                  (format "%s" years)
;;                  suffix " wedding anniversary]]")))
;;       )

;; - put `%%(org-bbdb-anniversaries)' in one of my agenda files. and set
;;   headline with property (:CATEGORY: Anniv)
;;
;; - [C-c C-x p] to set property
;;
;; - select CATEGORY property, value is "`Anniv'".
;;
;; - put this line into agenda file below the
;;   headline. %%(org-bbdb-anniversaries).


;;; [ org-contacts ] -- Contacts management in Org-mode.

(require 'org-contacts)

(setq org-contacts-files '("~/Org/Contacts/Contacts.org")
      ;; org-contacts-icon-use-gravatar (fboundp 'gravatar-retrieve)
      org-contacts-icon-use-gravatar nil
      org-contacts-icon-property "AVATAR"
      org-contacts-icon-size 32
      org-contacts-enable-completion t ; enable in message-mode.
      )

(add-to-list 'org-capture-templates
             '("C" "Contacts"
               entry (file+headline "~/Org/Contacts/Contacts.org" "Meet")
               "** %(org-contacts-template-name)
:PROPERTIES:
:NAME(Chinese): %^{Name(Chinese)}
:NAME(English): %^{Name(English)}
:NICK: %^{Nick}
:ICON: %^{ICON.jpg}
:BIRTHDAY: %^{Birthday}
:GENDER: %^{Gender|Male|Female}
:Sexual: %^{Sexual|Unknown|Heterosexual|Bisexual|Homosexual}
:RELATIONSHIP: %^{Relationship|Meet|Friend|Good Friend|Boy Friend|Girl Friend|Classmate|Schoolmate}
:FIRST-MEET: %^U  %^{first-time meet}
:Thought: %^{Thought|open|conservative}
:MOBILE: %^{Mobile Phone}
:EMAIL: %(org-contacts-template-email)
:QQ: %^{QQ}
:WeChat: %^{WeChat}
:Facebook: %^{Facebook}
:ADDRESS(home): %^{address(home)}
:ADDRESS(work): %^{address(work)}
:CHARACTER: %^{Character}
:FEELING: %^{Feeling}
:World-Views: %^{World Views}
:LANGUAGES: %^{Languages|Chinese|English|Japanese|Korean}
:EDUCATION: %^{Education}
:PROGRAMMING-SKILLS: %^{Programming Skills}
:SKILLS: %^{Skills}
:END:"
               :empty-lines 1
               :jump-to-captured t
               ))

;; (add-to-list 'org-property-set-functions-alist
;;              '(".*" . org-completing-read))

(setq org-contacts-matcher
      "NAME<>\"\"|EMAIL<>\"\"|Mailing-List<>\"\"|ALIAS<>\"\"|RELATIONSHIP<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"|BIRTHDAY<>\"\"|PROGRAMMING-SKILLS<>\"\"|SKILLS<>\"\"|EDUCATION<>\"\"|JOBS<>\"\"|NOTE"
      )

;; Create agenda view for contacts matching NAME.
(define-key my-org-prefix (kbd "b") 'org-contacts)

(dolist (hook '(message-mode-hook
                mu4e-compose-mode-hook
                ))
  (add-hook hook 'org-contacts-setup-completion-at-point))



;;; [ org-screenshot ] -- Take and manage screenshots in Org-mode files.

;;; Usage:
;;
;; - [M-x org-screenshot]
;; - `org-screenshot-take'

;; (setq org-screenshot-command-line "scrot -d 5 -s %f" ; "import %f",
;;       org-screenshot-relative-links t
;;       org-attach-directory "data/"
;;       org-screenshot-image-directory "./images/"
;;       org-screenshot-file-name-format "screenshot-%2.2d.png"
;;       )
;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c o s") 'org-screenshot)))


;;; [ org-download ] -- drag and drop images to Emacs org-mode.

(use-package org-download
  :ensure t
  :config
  (org-download-enable)
  
  (setq org-download-screenshot-method "scrot -s %s"
        org-download-method 'attach ; 'attach, 'directory,
        ;; if you don't want the #+DOWNLOADED: annotation in your Org document
        org-download-annotate-function (lambda (_) "")
        org-download-backend t ; url-retrieve (t), wget, curl.
        ;; org-download-heading-lvl
        ;; org-download-timestamp "_%Y-%m-%d_%H:%M:%S"
        org-download-image-dir "data/images" ; nil: default to "."
        ;; org-download-image-width nil ; use #+attr_html: :width
        ;; org-download-img-regex-list '("<img +src=\"" "<img +\\(class=\"[^\"]+\"\\)? *src=\"")
        )

  (unless (boundp 'my-org-download-map)
    (define-prefix-command 'my-org-download-map))
  (define-key my-org-prefix (kbd "d") 'my-org-download-map)

  (define-key my-org-download-map (kbd "i") 'org-download-image)
  (define-key my-org-download-map (kbd "s") 'org-download-screenshot)
  (define-key my-org-download-map (kbd "y") 'org-download-yank)
  (define-key my-org-download-map (kbd "d") 'org-download-delete)
  (define-key my-org-download-map (kbd "e") 'org-download-edit)

  (define-key org-mode-map (kbd "<drag-n-drop>") 'org-download-dnd)
  (define-key org-mode-map (kbd "<C-drag-n-drop>") 'org-download-dnd)
  (define-key org-mode-map (kbd "<M-drag-n-drop>") 'org-download-dnd)
  )


;;; [ org-pomodoro ] -- adds support for Pomodoro technique in Org-mode.

;;; Usage:
;;
;; - Move point to a task as you would do with `org-clock-in'. Call
;;   `org-pomodoro' the task will be clocked-in.
;;
;; - When there's time for break, the task will be org-clock-out'ed.
;;
;; - If you call `org-pomodoro' during a pomodoro, you'll be asked to reset a
;;   pomodoro.
;;
;; - If you call `org-pomodoro' outside org-mode, you'll be presented with list
;;   of recent tasks, as C-u org-clock-in would.

;; (require 'alert)
;; (require 'org-pomodoro)
;;
;; (setq org-pomodoro-audio-player "/usr/bin/mplayer"
;;       org-pomodoro-play-sounds t
;;       org-pomodoro-play-start-sound t
;;       org-pomodoro-play-ticking-sounds nil
;;       ;; org-pomodoro-ticking-sound
;;       org-pomodoro-ticking-sound-args "-volume 50" ; adjust ticking sound volume
;;       ;; org-pomodoro-start-sound-args "-volume 0.3"
;;       ;; org-pomodoro-long-break-sound-args "-volume 0.3"
;;       org-pomodoro-format "Pomodoro~%s" ; mode-line string
;;       )
;;
;; (define-key my-org-prefix (kbd "p") 'org-pomodoro)
;;
;; ;; start another pomodoro automatically upon a break end.
;; (add-hook 'org-pomodoro-break-finished-hook
;;           (lambda ()
;;             (interactive)
;;             (org-pomodoro '(16)) ; double prefix [C-u C-u]
;;             ))


;;; [ org-ref ] -- citations, cross-references, indexes, glossaries and bibtex utilities for org-mode

;; (use-package org-ref)


;;; [ helm-org-rifle ] -- Rifle through your Org buffers and acquire your target.

(use-package helm-org-rifle
  :ensure t
  :config
  (setq helm-org-rifle-show-path t
        helm-org-rifle-fontify-headings t
        helm-org-rifle-show-todo-keywords t
        helm-org-rifle-show-tags t)
  (define-key my-org-prefix (kbd "g") 'helm-org-rifle-current-buffer)
  (define-key my-org-prefix (kbd "G") 'helm-org-rifle)
  )


;;; [ otama ] -- Simple org-table based database, intended to be a light version of BBDB and helm-friendly.

;; (use-package otama
;;   :ensure t
;;   :config
;;   (setq otama-database-file-name
;;         (concat (getenv "HOME") "/Org" "/otama/otama.org"))
;;
;;   (define-key my-org-prefix (kbd "D") 'otama-helm)
;;   )


;;; [ org-eww ] -- automatically use eww to preview current org-file when save.

(use-package org-eww
  :ensure t
  :config
  ;; (add-hook 'org-mode-hook 'org-eww-mode)
  )


(provide 'init-my-org-extensions)

;;; init-my-org-extensions.el ends here
