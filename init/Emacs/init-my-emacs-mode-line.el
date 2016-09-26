;;; init-my-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; - [C-h v major-mode] for current buffer major mode.
;; - [C-h v minor-mode-alist] for current buffer minor modes list.


;;; [ window-divider-mode ]

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  ;; (window-divider-mode 1)
  )


;;; separate settings for only active mode-line.

(defvar my/mode-line-selected-window nil)

(defun my/mode-line-record-selected-window ()
  (setq my/mode-line-selected-window (selected-window)))

(defun my/mode-line-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'my/mode-line-record-selected-window)

(add-hook 'buffer-list-update-hook 'my/mode-line-update-all)

(defun active ()
  (eq my/mode-line-selected-window (selected-window)))

;; load necessary package which will be used later.
(use-package projectile
  :ensure t
  :init
  (require 'projectile))
(use-package projectile-rails
  :ensure t
  :init
  (require 'projectile-rails)
  ;; avoid invalid variable `projectile-rails-mode'
  (defvar projectile-rails-mode nil)
  )
(require 'vc)
(require 'vc-git)
(use-package flycheck
  :ensure t)

(use-package all-the-icons
  :ensure t)

;; nyan-mode
;; Nyan Mode is an analog indicator of your position in the buffer. The Cat
;; should go from left to right in your mode-line, as you move your point from
;; 0% to 100%.
;; (use-package nyan-mode
;;   :ensure t)
;; (setq nyan-animate-nyancat t
;;       nyan-wavy-trail nil
;;       nyan-animation-frame-interval 0.2
;;       nyan-bar-length 15
;;       nyan-cat-face-number 1
;;       )

;; spinner
(use-package spinner
  :ensure t)
;; (spinner-start 'vertical-breathing 10)
;; (spinner-start 'minibox)
;; (spinner-start 'moon)
;; (spinner-start 'triangle)

(require 'org-timer)

(use-package wc-mode
  :ensure t)

(setq-default
 mode-line-format
 (quote
  (
   ;; active window indicator
   (:eval
    (if (eq my/mode-line-selected-window (selected-window))
        (propertize "▌"
                    'face '(:foreground "yellow")
                    )))
   
   ;; emacsclient indicator
   (:eval (if (frame-parameter nil 'client)
              (propertize " あ "
                          'face '(:foreground "#333333" :background "yellow" :weight bold :height 120)
                          'help-echo "emacsclient frame")))

   ;; anzu
   (:propertize (:eval (anzu--update-mode-line))
                face (:foreground "cyan" :weight bold
                                  :box '(:color "cyan")))
   
   ;; multiple-cursors (mc/)
   (:eval (if (and 'mc/fake-cursor-p (> (mc/num-cursors) 1)) ; (if 'mc/fake-cursor-p ...)
              (propertize (format "[%d]" (mc/num-cursors)) ; `mc/mode-line'
                          'face '(:foreground "firebrick" :background "black")
                          'help-echo "multiple-cursors")))

   ;; input method
   (:eval (if current-input-method-title ; `set-input-method'
              (propertize (format " {%s}" current-input-method-title)
                          'face '(:foreground "cyan" :weight bold))))

   ;; mule info
   (:propertize (" " mode-line-mule-info)
                face (:foreground "dark gray"))   ; U:[*--]
   
   ;; Buffer status
   ;; mode-line-client
   ;; mode-line-remote
   ;; mode-line-frame-identification
   ;; mode-line-buffer-identification
   (:eval
    (cond
     (buffer-read-only
      (propertize " ⚑ "
                  'face '(:foreground "red" :weight bold)
                  'help-echo "buffer is read-only!!!"))
     ;; (overwrite-mode
     ;;  (propertize "??"))
     ((buffer-modified-p)
      (propertize " ☡ "
                  'face '(:foreground "orange")
                  'help-echo "buffer modified."))

     ;; (mode-line-remote
     ;;  (propertize " R "
     ;;              'face '(:foreground "dark magenta")
     ;;              'help-echo "remote buffer")
     ;;  )
     (t "   ")
     ))

   ;; buffer encoding
   (:eval
    (if (not (memq buffer-file-coding-system
                   '(utf-8
                     utf-8-unix
                     prefer-utf-8-unix
                     undecided-unix ; TODO: remove in future
                     )))
        (progn
          (list
           (propertize (format "[%s]" (symbol-name buffer-file-coding-system))
                       'face '(:foreground "red"))))
      ))
   
   ;; rbenv & rvm
   (:eval
    (if (memq (buffer-local-value 'major-mode (current-buffer))
              '(ruby-mode enh-ruby-mode rhtml-mode))
        (progn
          (list
           (propertize " ("
                       'face '(:foreground "#444444"))
           (propertize "Ruby: "
                       'face '(:family "Segoe Print"
                                       :height 80
                                       :foreground "red2"))
           (propertize (rbenv--active-ruby-version) ; `rbenv--modestring'
                       'face '(:foreground "cyan" :height 70)
                       'help-echo '(concat "\nCurrent Ruby version: " (rbenv--active-ruby-version)
                                           "\nmouse-1: switch Ruby version menu")
                       )
           (propertize ")"
                       'face '(:foreground "#444444"))
           )
          )
      ))
   

   ;; pyvenv `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
   (:eval
    (if (memq (buffer-local-value 'major-mode (current-buffer))
              '(python-mode))
        (progn
          (list
           (propertize (format "[%s]" pyvenv-virtual-env-name)
                       'face '(:foreground "orange" :height 80))))
      ))

   ;; conda: `conda-env-current-name'
   (:eval
    (if (bound-and-true-p conda-env-current-name)
        (propertize (format "[%s]" conda-env-current-name)
                    'face '(:foreground "dark orange" :height 80))))

   ;; Clojure - CIDER
   (:eval
    (when (and
           (equal major-mode 'clojure-mode)
           (not (equal (cider--modeline-info) "not connected")))
      ;; (propertize (format "CIDER √")
      ;;             'face '(:foreground "forest green"))
      ;; (propertize (format "CIDER: %s" (cider--project-name nrepl-project-dir))
      ;;             'face '(:foreground "forest green"))

      (propertize (format " CIDER: √ ")
                  'face '(:foreground "forest green")
                  'display
                  (let ((clojure-icon (concat user-emacs-directory "resources/icon/" "Clojure.xpm")))
                    (if (and (file-exists-p clojure-icon)
                             (image-type-available-p 'xpm))
                        (create-image clojure-icon 'xpm nil :ascent 'center))))
      )
    )

   ;; CIDER project info
   (:eval
    (when (and
           (equal major-mode 'clojure-mode)
           (not (equal (cider--modeline-info) "not connected")))
      (propertize (format " %s" (or (cider--project-name nrepl-project-dir)
                                    ""))
                  'face '(:foreground "cyan"))
      )
    )

   ;; `ejc-sql' connection name
   (:eval
    (when (and (bound-and-true-p ejc-sql-mode)
               (bound-and-true-p ejc-connection-name))
      (propertize (format "%s" ejc-connection-name)
                  'face '(:foreground "green"))))
   
   ;; VCS - Git, SVN, CVS,
   (:eval
    (when vc-mode
      (let* ((backend
              (substring vc-mode
                         (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))
             (state (vc-state buffer-file-name))
             (ascii (cond ((memq state '(update-to-date))
                           "✔")
                          ((memq state '(edited))
                           "∓")
                          ((memq state '(added))
                           "✚")
                          ((memq state '(removed))
                           "✖")
                          ((memq state '(conflict))
                           "≠")
                          ((memq state '(needs-merge))
                           "⌥")
                          ((memq state '(needs-updated))
                           "⇧")
                          ((memq state '(unregistered))
                           "-")
                          ((memq state '(missing))
                           "-")
                          (nil
                           "")
                          ))
             (face (cond ((memq state '(up-to-date))
                          'face '(:foreground "green"))
                         ((memq state '(edited added))
                          'face '(:foreground "yellow"))
                         ((memq state '(removed needs-merge needs-update))
                          'face '(:foreground "dark violet"))
                         ((memq state '(unregistered))
                          'face '(:foreground "dim gray"))
                         ((memq state '(conflict))
                          'face '(:foreground "firebrick"))
                         (t
                          'face '(:foreground "deep pink"))
                         )))
        
        (list
         ;; TODO:
         ;; (propertize (all-the-icons-octicon "git-branch")
         ;;             'face `(:inherit ,(if active face 'mode-line-inactive)
         ;;                              :family ,(all-the-icons-octicon-family)
         ;;                              :height 1.25)
         ;;             'display '(raise -0.1))
         
         (propertize (format "%s" backend)
                     'face (if (active) face 'mode-line-inactive)
                     'face '(:foreground "red")
                     )
         (propertize (format "(%s)" ascii)
                     'face (if (active) face 'mode-line-inactive)
                     )
         ))
      ))


   ;; projectile-rails-mode
   (:propertize " ")
   (:eval
    (when (and (featurep 'projectile-rails-mode)
               (eq projectile-rails-mode t))
      (propertize (format " Rails ")
                  'face '(:foreground "red")
                  'display (let ((rails-icon (concat user-emacs-directory "resources/icon/" "Rails.xpm")))
                             (if (and (file-exists-p rails-icon)
                                      (image-type-available-p 'xpm))
                                 (create-image rails-icon 'xpm nil :ascent 'center))))))
   
   ;; org-tree-slide slide number
   ;; (:eval
   ;;  (when (org-tree-slide--active-p)
   ;;    (propertize (format "[%s]" org-tree-slide--slide-number)
   ;;                'face '(:foreground "cyan"))))
   
   ;; the buffer name; the filename as a tool tip
   (:propertize " ["
                face (:foreground "cyan"))
   (:propertize "%b"
                face (:foreground "white"
                                  :height 75)
                help-echo (buffer-file-name))
   (:propertize "]"
                face (:foreground "cyan"))

   ;; flycheck
   (:eval
    (if flycheck-current-errors
        (propertize (flycheck-mode-line-status-text)
                    'face '(:foreground "orange" :height 70))))


   ;; mmm-mode


   ;; process: inferior,
   (:eval
    (when mode-line-process
      (list
       (propertize " ◌ "
                   'face '(:foreground "cyan" :weight bold :height 120)
                   'help-echo "buffer-process")
       (propertize mode-line-process
                   'face '(:foreground "DeepSkyBlue" :slant 'italic)
                   'help-echo "buffer-process")
       )))
   
   ;; notifications
   ;; IRC


   ;; org-timer
   (:eval
    (unless (not org-timer-countdown-timer)
      (if (eq my/mode-line-selected-window (selected-window))
          (propertize (let* ((rtime (decode-time
                                     (time-subtract
                                      (timer--time org-timer-countdown-timer)
                                      (current-time))))
                             (rmins (nth 1 rtime))
                             (rsecs (nth 0 rtime)))
                        (format "🕔 %d:%d" rmins rsecs))
                      'face '(:foreground "cyan3")
                      'help-echo "org-timer"))))
   
   ;; org-clock
   ;; (:propertize (t org-mode-line-string)
   ;;              face (:foreground "cyan"))
   ;; (:eval
   ;;  (if (string-empty-p org-mode-line-string)
   ;;      (propertize (t org-mode-line-string)
   ;;                  'face '(:foreground "cyan" :weight bold)
   ;;                  'help-echo "Org-mode clock"))
   ;;  )
   
   ;; --------------------------- right align ----------------------------------
   
   ;; Email

   ;; fill with ' '.
   ;; (:propertize "% ")
   
   (:propertize mode-line-end-spaces)
   )))

;; update org-clock timer in mode-line after `org-clock-out-hook'.
;; fix org-clock timer does not disappear after clock out.
(add-hook 'org-clock-out-hook
          '(lambda ()
             ;; (org-clock-update-mode-line)
             (setq org-mode-line-string nil)
             (force-mode-line-update)))

;; --------------------------- right align ----------------------------------

;;; mode-line right align (which replace `mode-line-end-spaces')
;;; you can custom here (add right aligned things here)
(display-time-mode t)
(setq global-mode-string (remove 'display-time-string global-mode-string))
(setq
 mode-line-end-spaces
 (quote
  (
   (:propertize " "
                display (space :align-to (- right 55)))

   ;; nyan-mode
   ;; (:eval
   ;;  (when nyan-mode (list (nyan-create))))

   ;; spinner
   ;; Let spinner support to be used in custom mode-line as a function.
   ;; '(:eval (spinner-start 'minibox))
   ;;
   ;; '(:propertize  (:eval (spinner-start 'minibox))   ; 'spinner--mode-line-construct
   ;;                :face (:foreground "dark gray"))

   ;; line and column number, relative position
   ;; `mode-line-position'
   ;; '(:propertize " [%p,%I] ")
   (:eval
    (list
     (propertize " ("
                 'face '(:foreground "dark gray"))
     (propertize "%02l"
                 'face '(:foreground "dark gray" :height 75))
     (propertize ","
                 'face '(:foreground "dark gray"))
     (propertize "%02c"
                 'face
                 (if (>= (current-column) 75)
                     '(:foreground "red" :height 75)
                   '(:foreground "dark gray" :height 75)))
     (propertize ")"
                 'face '(:foreground "dark gray"))
     (propertize "_%03p"
                 'face '(:foreground "dark gray" :height 75))
     )
    )
   
   (:propertize " ["
                face (:foreground "gray" :weight bold))
   ;; projectile
   (:eval
    (if (bound-and-true-p projectile-mode)
        (list
         (propertize "P: "
                     'face '(:foreground "dim gray" :height 75))
         (propertize (projectile-project-name) ; `projectile-mode-line'
                     'face '(:foreground "orange" :height 75))
         )
      ))
   (:propertize "] "
                face (:foreground "gray" :weight bold))

   ;; the major mode of the current buffer.
   ;; `mode-name', `mode-line-modes', `minor-mode-alist'
   (:propertize
    "%m"
    face (:foreground "cyan"
                      :family "Comic Sans MS" :weight bold :height 80))
   )))


;; (setq mode-line-in-non-selected-windows t)

;;; 1. color style
;; (set-face-attribute 'mode-line nil
;;                     :inverse-video nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil)
;;                     :family "DejaVu Sans Mono"
;;                     :height 90
;;                     )
;; (set-face-attribute 'mode-line-inactive nil
;;                     :inverse-video nil
;;                     :foreground "#444444" :background "black" ; :background "#242424"
;;                     :family "DejaVu Sans Mono"
;;                     :box '(:color "slate blue" :line-width -1 :style nil)
;;                     :height 90
;;                     )

;;; 2. darker style
;; (set-face-attribute 'mode-line nil
;;                     :inverse-video nil
;;                     :foreground "white"
;;                     :background "#484848"
;;                     :box '(:color "dark gray" :line-width 1 :style nil)
;;                     ;; :box '(:color "slate blue" :line-width 1 :style nil)
;;                     :family "DejaVu Sans Mono"
;;                     :height 100
;;                     )
;; (set-face-attribute 'mode-line-inactive nil
;;                     :inverse-video nil
;;                     :foreground "dim gray"
;;                     :background (color-darken-name (face-background 'default) 3)
;;                     :family "DejaVu Sans Mono"
;;                     :box '(:color "dark slate gray" :line-width 1 :style nil)
;;                     :height 100
;;                     )


;;; [ display-time ]

;; (setq display-time-interval 60)
;; (setq display-time-24hr-format nil)
;; (setq display-time-format nil)
;; (setq display-time-day-and-date nil)

;; ;;; event
;; (display-time-event-handler)

;;; Mail
(setq display-time-mail-directory "~/Mails/INBOX/new/")
(setq display-time-use-mail-icon t)
;; (setq display-time-mail-file nil)
;; (setq display-time-mail-function nil)
(display-time-mail-check-directory)

;; ;;; load-average
;; (setq display-time-default-load-average 0)
;; (setq display-time-load-average-threshold 0.5)

(display-time-mode t)


(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
