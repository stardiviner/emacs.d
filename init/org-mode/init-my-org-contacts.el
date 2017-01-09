;;; init-my-org-contacts.el --- init for Org Contacts
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-contacts ] -- Contacts management in Org-mode.

(require 'org-contacts)

(setq org-contacts-files '("~/Org/Contacts/Contacts.org")
      ;; org-contacts-icon-use-gravatar (fboundp 'gravatar-retrieve)
      org-contacts-icon-use-gravatar nil
      org-contacts-icon-property "AVATAR"
      org-contacts-icon-size 32
      org-contacts-enable-completion t ; enable in message-mode.
      )

(setq org-capture-templates
      (append '(("C" "org-contacts")
                ("Cc" "contact"
                 entry (file+headline (concat org-directory "/Contacts/Contacts.org") "Meet")
                 "** %^{NAME}
:PROPERTIES:
:NAME(Chinese): %^{Name(Chinese)}
:NAME(English): %^{Name(English)}
:NICK: %^{Nick}
:AVATAR: %^{Avatar}
:BIRTHDAY:
:GENDER: %^{Gender|Male|Female}
:Sexual: %^{Sexual|Heterosexual|Bisexual|Homosexual}
:RELATIONSHIP: %^{Relationship|Internet|Meet|Friend|Good Friend|Boy Friend|Girl Friend|Classmate|Schoolmate}
:FIRST-MEET: %^U  %^{How is the first-time meet?}
:Thought: %^{Thought|open|conservative}
:MOBILE: %^{Mobile Phone}
:EMAIL: %(org-contacts-template-email)
:QQ:
:WeChat: %^{WeChat}
:Facebook: %^{Facebook}
:GitHub: %^{GitHub}
:ADDRESS(home): %^{address(home)}
:ADDRESS(work): %^{address(work)}
:ADDRESS(live): %^{address(live)}
:CHARACTER:
:FEELING:
:World-Views:
:LANGUAGES: %^{Languages|Chinese|Chinese, English|English|Japanese|Korean}
:EDUCATION: %^{Education}
:SKILLS: %^{Skills}
:Programming-Skills: %^{Programming Skills}
:Programming-Languages: %^{Programming Languages}
:Occupation: %^{Occupation|Freelancer|Businessman|Servant}
:Hobbies: %^{Hobbies}
:END:"
                 :empty-lines 1
                 :jump-to-captured t
                 )

                ("Cm" "meet people"
                 entry (file+headline (concat org-directory "/Contacts/Contacts.org") "Meet")
                 "** %^{NAME}
:PROPERTIES:
:NAME(Chinese): %^{Name(Chinese)}
:NICK: %^{Nick}
:AVATAR: %^{Avatar}
:GENDER: %^{Gender|Male|Female}
:RELATIONSHIP: Meet
:FIRST-MEET: %^U  %^{How is the first-time meet?}
:Thought: %^{Thought|open|conservative}
:MOBILE: %^{Mobile Phone}
:WeChat: %^{WeChat}
:ADDRESS(home): %^{address(home)}
:ADDRESS(work): %^{address(work)}
:ADDRESS(live): %^{address(live)}
:LANGUAGES: %^{Languages|Chinese|Chinese, English|English|Japanese|Korean}
:EDUCATION: %^{Education}
:SKILLS: %^{Skills}
:Occupation: %^{Occupation|Freelancer|Businessman|Servant}
:Hobbies: %^{Hobbies}
:END:"
                 :empty-lines 1
                 :jump-to-captured t
                 )

                ("Cp" "programmer"
                 entry (file+headline (concat org-directory "/Contacts/Contacts.org") "Programmers")
                 "** %^{NAME}
:PROPERTIES:
:NAME(Chinese): %^{Name(Chinese)}
:NAME(English): %^{Name(English)}
:NICK: %^{Nick}
:AVATAR: %^{Avatar}
:GENDER: %^{Gender|Male|Female}
:RELATIONSHIP: Internet
:FIRST-MEET: %^U  %^{How is the first-time meet?}
:Thought: %^{Thought|open|conservative}
:EMAIL: %(org-contacts-template-email)
:WeChat: %^{WeChat}
:Facebook: %^{Facebook}
:GitHub: %^{GitHub}
:ADDRESS(home): %^{address(home)}
:ADDRESS(work): %^{address(work)}
:ADDRESS(live): %^{address(live)}
:LANGUAGES: %^{Languages|Chinese|Chinese, English|English|Japanese|Korean}
:EDUCATION: %^{Education}
:SKILLS: %^{Skills}
:Programming-Skills: %^{Programming Skills}
:Programming-Languages: %^{Programming Languages}
:Occupation: %^{Occupation|Freelancer|Businessman|Servant}
:Hobbies: %^{Hobbies}
:END:"
                 :empty-lines 1
                 :jump-to-captured t
                 )
                
                ("Cf" "friend"
                 entry (file+headline (concat org-directory "/Contacts/Contacts.org") "Friends")
                 "** %^{NAME}
:PROPERTIES:
:AVATAR: %^{Avatar}
:NAME(Chinese): %^{Name(Chinese)}
:NAME(English): %^{Name(English)}
:NICK: %^{Nick}
:BIRTHDAY:
:GENDER: %^{Gender|Male|Female}
:Sexual: %^{Sexual|Heterosexual|Bisexual|Homosexual}
:RELATIONSHIP: Friend
:FIRST-MEET: %^U  %^{How is the first-time meet?}
:Thought: %^{Thought|open|conservative}
:MOBILE: %^{Mobile Phone}
:EMAIL: %(org-contacts-template-email)
:WeChat: %^{WeChat}
:ADDRESS(home): %^{address(home)}
:ADDRESS(work): %^{address(work)}
:ADDRESS(live): %^{address(live)}
:CHARACTER:
:FEELING:
:World-Views:
:LANGUAGES: %^{Languages|Chinese|Chinese, English|English|Japanese|Korean}
:EDUCATION: %^{Education}
:SKILLS: %^{Skills}
:Occupation: %^{Occupation|Freelancer|Businessman|Servant}
:Hobbies: %^{Hobbies}
:END:"
                 :empty-lines 1
                 :jump-to-captured t
                 )
                )
              org-capture-templates))

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

(use-package helm-org-rifle
  :ensure t
  :config
  ;; Contacts
  (defun my-org-rifle-Contacts-reference ()
    (interactive)
    (let ((my-contacts-reference-dir
           (concat org-directory "/Contacts/")))
      (helm-org-rifle-directories
       (list my-contacts-reference-dir))))
  
  (define-key references-rifle-prefix (kbd "C-c") 'my-org-rifle-Contacts-reference)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-org-contacts)

;;; init-my-org-contacts.el ends here
