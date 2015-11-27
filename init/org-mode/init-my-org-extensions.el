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
;;       ;; TODO: improve this option
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

;; FIXME: (org-bbdb-anniversaries)


;;; [ org-contacts ] -- Contacts management in Org-mode.

(use-package org-contacts
  :config

  (setq org-contacts-files '("~/Org/Contacts/Contacts.org")
        org-contacts-icon-use-gravatar (fboundp 'gravatar-retrieve)
        org-contacts-icon-property "ICON"
        org-contacts-icon-size 32
        org-contacts-enable-completion t ; enable in message-mode.
        )

  (add-to-list 'org-capture-templates
               '("C" "Contacts"
                 entry (file "~/Org/Contacts/Contacts.org")
                 "* %(org-contacts-template-name)
:PROPERTIES:
:NAME: %(org-contacts-template-name)
:NICK:
:ALIAS:
:ICON: %(org-contacts-template-name).jpg
:BIRTHDAY: %t
:GENDER: %^{Gender|Male|Female}
:Sexual: %^{Sexual|Heterosexual|Bisexual|Homosexual}
:RELATIONSHIP: %^{Relationship|Friend|Good Friend|Boy Friend|Girl Friend}
:FIRST-MEET: %^U  %^{first-time meet}
:DATE: %^u
:TIME: %^U
:URL: %^{URL}
:EMAIL: %(org-contacts-template-email)
:MOBILE: %^{Mobile Phone}
:PHONE:
:IRC:
:QQ:
:WeChat: %^{WeChat}
:BLOG:
:ADDRESS(home): %^{address(home)}
:ADDRESS(live): %^{address(live)}
:ADDRESS(work):
:EDUCATION(college): %^{Education(college)}
:COMPANY: %^{Companies}
:Bio:
:INTERESTS:
:PROGRAMMING-SKILLS: %^{programming skills}
:SKILLS: %^{skills}
:JOBS: %^{jobs}
:EDUCATION:
:LANGUAGES: %^{Languages|Chinese|English|Japanese|Korean}
:PROJECTS:
:NOTE:
:END:"
                 :empty-lines 1
                 :jump-to-captured t
                 ))

  ;; TODO: add more custom complete functions.
  ;; (add-to-list 'org-contacts-complete-functions 'func)

  ;; (add-to-list 'org-property-set-functions-alist
  ;;              '(".*" . org-completing-read))

  (define-key my-org-prefix (kbd "b") 'org-contacts)

  (dolist (hook '(message-mode-hook
                  mu4e-compose-mode-hook
                  ))
    (add-hook hook 'org-contacts-setup-completion-at-point))
  )


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

;;; Usage:
;;
;; *This extension facilitates moving images from point A to point B.*
;;
;; * Point A (the source) can be:
;;
;;   - An image inside your browser that you can drag to Emacs.
;;
;;   - An image on your file system that you can drag to Emacs.
;;
;;   - A local or remote image address in kill-ring. Use the org-download-yank
;;     command for this. Remember that you can use "=[0 w]=" in dired to get an
;;     address.
;;
;;   - An screenshot taken using /gnome-screenshot/ or /scrot/ or /gm/. Use the
;;     org-download-screenshot command for this. Customize the backend with
;;     org-download-screenshot-method.
;;
;; * Point B (the target) is an Emacs org-mode buffer where the inline link will
;;   be inserted. Several customization options will determine where exactly on
;;   the file system the file will be stored.
;;
;;They are: org-download-method:
;;
;; 1) 'attach => use org-mode attachment machinery
;;
;; 2) 'directory => construct the directory in two stages:
;;
;;   1. first part of the folder name is:
;;      - either "." (current folder)
;;
;;      - or org-download-image-dir (if it's not nil).
;;
;;      - ~org-download-image-dir~ becomes buffer-local when set, so each file can customize this value, e.g with:
;;
;;        #+BEGIN_EXAMPLE
;;        -*- mode: Org; org-download-image-dir: "~/Pictures/foo"; -*-
;;        #+END_EXAMPLE
;;
;;        To set it for all files at once, use this:
;;        
;;        #+BEGIN_SRC emacs-lisp
;;        (setq-default org-download-image-dir "~/Pictures/foo")
;;        #+END_SRC
;;
;;   2. second part is:
;;
;;       - ~org-download-heading-lvl~ is ~nil~ => ""
;;
;;       - ~org-download-heading-lvl~ is ~n~ => the name of current heading with level n.
;;
;;         Level count starts with 0, i.e. * is 0, ** is 1, *** is 2
;;         etc. org-download-heading-lvl becomes buffer-local when set, so each
;;         file can customize this value, e.g with:
;;
;;         #+BEGIN_EXAMPLE
;;         -*- mode: Org; org-download-heading-lvl: nil; -*-
;;         #+END_EXAMPLE

(org-download-enable)

(use-package org-download
  :config
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


;;; [ org-present ]

(use-package org-present
  :config
  ;; Precise behaviour of org-present during start and quit is controlled from
  ;; hooks. The following will enlarge text, show images, hide the cursor and
  ;; make the buffer read-only:
  (add-hook 'org-present-mode-hook
            '(lambda ()
               (org-present-big)
               (org-display-inline-images)
               (org-present-hide-cursor)
               (org-present-read-only)

               ;; bind [SPACE] to navigate to next slice.
               (define-key org-present-mode-keymap (kbd "SPC") 'org-present-next)
               ))
  (add-hook 'org-present-mode-quit-hook
            '(lambda ()
               (org-present-small)
               (org-remove-inline-images)
               (org-present-show-cursor)
               (org-present-read-write)))

  (define-key my-org-prefix (kbd "p") 'org-present)
  )


(provide 'init-my-org-extensions)

;;; init-my-org-extensions.el ends here
