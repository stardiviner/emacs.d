;;; init-my-startup.el --- init startup things

;;; Commentary:

;;; Code:


;;; initial Emacs frame size

;; 1:
;; (add-hook 'before-make-frame-hook
;;           #'(lambda ()
;;               (add-to-list 'default-frame-alist '(left . 0))
;;               (add-to-list 'default-frame-alist '(top . 0))
;;               (add-to-list 'default-frame-alist '(height . 150))
;;               (add-to-list 'default-frame-alist '(width . 100))))

;; 2:
;; (if window-system
;;    (set-frame-size (selected-frame) 120 45))

;; 3:
;; ~/.Xresources (or .Xdefaults):
;; Emacs*geometry:  80x24

;; 4:
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 columns windows for smaller displays
        ;; pick whatever number make sense for you
        (if (> (x-display-pixel-width) 1080)
            (add-to-list 'default-frame-alist (cons 'width 250))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; or:
        ;; (add-to-list 'default-frame-alist (cons 'width 120))
        ;; for the height, subtract a couple hundred pixels from the screen
        ;; height (for panels, menubars and whatnot), then divide by the height
        ;; of a char to get the height we want.
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 150)
                                      (frame-char-height))))
        ;; or:
        ;; (add-to-list 'default-frame-alist (cons 'height 600))
        ;; FIXME add parameters for this function. only show one window when
        ;; startup default frame.
        ;; (delete-other-windows)
        )))

(set-frame-size-according-to-resolution)

;; 5.
;; (setq initial-frame-alist
;;       '((top . 1) (left . -220)
;;         (width . 120)
;;         (height . 55)
;;         ))



;; [ Auto opened files ]
(find-file "~/Git/Arch/install.sh")
;; (find-file "~/Git/Ubuntu/install.sh")
;; (find-file "~/Git/Ubuntu/init.pp")
;; (find-file "~/Git/Puppet/init.pp")
(find-file "~/.emacs.d/init.el")
(find-file "~/Org/INDEX.org")           ; index
(find-file "~/Org/Wiki/Wiki.org")       ; Wiki
(find-file "~/Org/Wiki/Reading/Reading.org")
(find-file "~/Org/Wiki/Reading/Aphorisms/Aphorisms.org")
;; (find-file "~/Org/Wiki/Reading/Novels/Data/九州.org")
(find-file "~/Org/Wiki/Computer/Computer.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming.org")
(find-file "~/Org/Wiki/Computer/Programming/Implements/Implements.org")
(find-file "~/Org/Wiki/Computer/Programming/Algorithm/Algorithm.org")
(find-file "~/Org/Wiki/Computer/Programming/Data Structure/Data Structure.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Programming Languages.org")
(find-file "~/Org/Wiki/Computer/Programming/Code/Code.org")
(find-file "~/Org/Wiki/Computer/Hack/Hack.org")
(find-file "~/Org/Wiki/Computer/Systems/Linux/Linux.org")
(find-file "~/Org/Wiki/Computer/Systems/Linux/Arch/Arch.org")
(find-file "~/Org/Wiki/Computer/Programming/Emacs/Emacs.org")
(find-file "~/Org/Wiki/Computer/Programming/Emacs/modes/modes.org")
(find-file "~/Org/Wiki/Computer/Programming/Tools/Version Control System/Git/Git.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Lisp/Lisp.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Lisp/Common Lisp/Common Lisp.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Ruby.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Data/Frameworks/Ruby on Rails/Ruby on Rails.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Data/Wiki/The Ruby Programming Language.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Tools/RubyGems/RubyGems.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Python/Python.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Python/Data/Wiki/The Python Programming Language.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Go/Go.org")
(find-file "~/Org/Wiki/Computer/Softwares/Softwares.org")
(find-file "~/Org/Wiki/Computer/Systems/Linux/Command Line/Command Line.org")
(find-file "~/Org/Wiki/Computer/Systems/Linux/Command Line/Commands/Commands.org")
(find-file "~/Org/Wiki/Computer/Systems/Linux/Configurations/Configurations.org")
(find-file "~/Org/Wiki/Computer/Hardware/Hardware.org")
(find-file "~/Org/Wiki/Computer/Systems/Android/Android.org")
(find-file "~/Org/Wiki/Computer/Systems/Android/Apps/Apps.org")
(find-file "~/Org/Wiki/Myself/Myself.org")
(find-file "~/Org/Wiki/Computer/censorship/censorship.org")
(find-file "~/Org/Wiki/Computer/Programming/Emacs/modes/Org/Org-mode.org")
(find-file "~/Org/Wiki/Learning/Learning.org")
(find-file "~/Org/Wiki/Learning/MyLearningPlan/Learn Programming.org")
(find-file "~/Org/Wiki/Writing/Writing.org")
(find-file "~/Org/Wiki/Kung Fu/Kung Fu.org")
(find-file "~/Org/Wiki/Science/Science.org")
(find-file "~/Org/Wiki/Life/Life.org")
(find-file "~/Org/Revenge/Revenge.org")
(find-file "~/Org/Wiki/Myself/LifeExperience/LifeExperience.org")
(find-file "~/Org/Projects/Projects.org")
(find-file "~/Org/Work/Work.org")
(find-file "~/Org/Tasks.org")
(find-file "~/Org/Daily.org")

(find-file "~/Git/Accounts/accounts.org")
(find-file "~/Org/Porn/Porn.org")

(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Ruby.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Data/Wiki/The Ruby Programming Language.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Frameworks/Ruby on Rails/Ruby on Rails.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Frameworks/Ruby on Rails/Data/Manuals/My Ruby on Rails Reference.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Frameworks/Ruby on Rails/Data/Manuals/My Ruby on Rails Guides.org")
(find-file "~/Code/learning/Ruby/test.rb")
(find-file "~/Code/learning/Ruby/Ruby on Rails/blog/README.rdoc")
(find-file "~/Code/learning/Ruby/Ruby on Rails/campo/README.md")

(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Python/Python.org")
(find-file "~/Org/Wiki/Computer/Programming/Programming Languages/Python/Data/Wiki/The Python Programming Language.org")
;; (find-file "~/Code/learning/Python/test.py")



;; replace initial/scratch buffer with our primary `.org' file
(setq my-org-special-directory (expand-file-name "~/Org"))
;; (setq initial-buffer-choice (concat my-org-special-directory "/Tasks.org"))


;;; sidebar
;; (neotree-show)


;;; start Sauron at Emacs startup
;; (sauron-start)


;;; start ERC at Emacs startup
;; (my-erc-start-or-switch)


;;; open Org-Agenda at startup.
(org-agenda-list)
;; (org-todo-list)

;; (setq initial-buffer-choice "*Org Agenda*")


;;; Email - mu4e
;; (add-hook 'emacs-startup-hook 'mu4e)



(provide 'init-my-startup)

;;; init-my-startup.el ends here
