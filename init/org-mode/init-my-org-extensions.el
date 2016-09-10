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
               "** %^{NAME}
:PROPERTIES:
:NAME(Chinese): %^{Name(Chinese)}
:NAME(English): %^{Name(English)}
:NICK: %^{Nick}
:ICON:
:BIRTHDAY:
:GENDER: %^{Gender|Male|Female}
:Sexual: %^{Sexual|Heterosexual|Bisexual|Homosexual}
:RELATIONSHIP: %^{Relationship|Meet|Friend|Good Friend|Boy Friend|Girl Friend|Classmate|Schoolmate}
:FIRST-MEET: %^U  %^{How is the first-time meet?}
:Thought: %^{Thought|open|conservative}
:MOBILE: %^{Mobile Phone}
:EMAIL: %(org-contacts-template-email)
:QQ:
:WeChat: %^{WeChat}
:Facebook: %^{Facebook}
:ADDRESS(home): %^{address(home)}
:ADDRESS(work): %^{address(work)}
:CHARACTER: 
:FEELING: 
:World-Views: 
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


(defun org-contacts-properties-drawer-link-workaround ()
  "Fix can't open link in properties drawer issue."
  (if (cl-some #'(lambda (x)
                   (string= (expand-file-name x) (buffer-file-name)))
               org-contacts-files)
      (setq-local org-startup-with-latex-preview nil)))

(add-hook 'org-mode-hook 'org-contacts-properties-drawer-link-workaround)


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


;;; [ org-ref ] -- citations, cross-references, indexes, glossaries and bibtex utilities for Org-mode.

(use-package org-ref
  :ensure t
  :config
  (setq bibtex-completion-pdf-open-function 'org-open-file)

  ;; (setq org-ref-bibtex-hydra-key-binding "\C-cj")

  (unless (boundp 'org-ref-prefix)
    (define-prefix-command 'org-ref-prefix))
  (define-key my-org-prefix (kbd "C-]") 'org-ref-prefix)

  (define-key org-ref-prefix (kbd "C-]") 'org-ref-insert-link)
  (define-key org-ref-prefix (kbd "c") 'org-ref-helm-insert-cite-link)
  (define-key org-ref-prefix (kbd "l") 'org-ref-helm-insert-label-link)
  (define-key org-ref-prefix (kbd "r") 'org-ref-helm-insert-ref-link)

  ;; Let org-mode auto process the LaTeX export to PDF process.
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  )


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
