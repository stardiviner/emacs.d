;;; init-company-mode.el --- init company-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Company Mode ]

;; TODO: https://github.com/company-mode/company-mode/wiki/Third-Party-Packages

;;; Annotations:
;; - <f> :: function
;; - ->  :: snippet (yasnippet)

;;; Usage:
;;;

(require 'company)

(setq company-minimum-prefix-length 2   ; minimum prefix character number for auto complete.
      company-idle-delay 0.2
      company-tooltip-align-annotations t ; align annotations to the right tooltip border.
      company-tooltip-limit 10          ; tooltip candidates max limit.
      company-tooltip-minimum 6         ; minimum candidates limit.
      company-tooltip-minimum-width 0   ; The minimum width of the tooltip's inner area.
                                        ; This doesn't include the margins and the scroll bar.
      company-tooltip-margin 1          ; width of margin columns to show around the tooltip
      company-tooltip-offset-display 'scrollbar ; 'lines - how to show tooltip unshown candidates number.
      ;; company-show-numbers nil ; t: show quick-access numbers for the first ten candidates.
      ;; company-selection-wrap-around nil
      ;; company-async-wait 0.03
      ;; company-async-timeout 2
      )

(setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                          company-echo-metadata-frontend
                          company-preview-if-just-one-frontend))

(setq company-backends '(company-elisp
                         company-eclim
                         company-semantic company-clang  company-cmake
                         company-capf
                         company-ropemacs
                         company-nxml company-css
                         company-cmake
                         company-xcode
                         company-bbdb
                         (company-dabbrev-code company-yasnippet company-gtags company-etags company-keywords)
                         company-files company-dabbrev company-abbrev
                         company-oddmuse
                         ;; company-ispell
                         ))

;; To use company-mode in all buffers, add the following line to your init file:
;; (unless (featurep 'auto-complete)
;;   (message "auto-complete isn't enabled, active company-mode instead.")
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (after 'global-company-mode
;;     (diminish company-mode))
;;   )

(add-hook 'after-init-hook 'global-company-mode)
(after 'global-company-mode
  (diminish company-mode))


;; keybindings
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-j") 'company-complete-selection)

(define-key company-active-map (kbd "M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p") 'company-select-previous)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "M-j") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete)
(define-key company-active-map [mouse-1] 'company-complete-mouse)
(define-key company-active-map [mouse-3] 'company-select-mouse)
(define-key company-active-map (kbd "C-g") '(lambda ()
                                              (interactive)
                                              (company-abort)))
(define-key company-active-map [tab] 'company-complete-common)
(define-key company-active-map (kbd "M-i") 'company-complete-common)
(define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "C-v") 'company-show-location)
(define-key company-active-map (kbd "C-s") 'company-search-candidates)
(define-key company-active-map (kbd "C-M-s") 'company-filter-candidates)
(define-key company-search-map (kbd "C-g") 'company-search-abort)
(define-key company-search-map (kbd "C-s") 'company-search-repeat-forward)
(define-key company-search-map (kbd "C-r") 'company-search-repeat-backward)
(define-key company-search-map (kbd "C-o") 'company-search-kill-others)


;; faces
(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "white"
                    :weight 'normal :slant 'normal)
(set-face-attribute 'company-tooltip-selection nil
                    :inherit 'company-tooltip
                    :foreground "white" :background "#212121")
(set-face-attribute 'company-tooltip-mouse nil
                    :inherit 'company-tooltip
                    :foreground "cyan" :background "black"
                    :weight 'bold)
(set-face-attribute 'company-tooltip-common nil
                    :inherit 'company-tooltip
                    :foreground "dark gray")
(set-face-attribute 'company-tooltip-common-selection nil
                    :inherit 'company-tooltip-common
                    :inverse-video nil
                    :foreground "white" :background " ")
(set-face-attribute 'company-tooltip-search nil
                    :inherit 'company-tooltip
                    :foreground "red")
(set-face-attribute 'company-tooltip-annotation nil
                    :inherit 'company-tooltip
                    :foreground "dark red" :background " ")
(set-face-attribute 'company-scrollbar-fg nil
                    :foreground "black" :background "black")
(set-face-attribute 'company-scrollbar-bg nil
                    :foreground " " :background "gray")

;; TODO:
;; company-prefix is for "inline" displayed first matched candidate.
;; - company-preview
;; - company-preview-common
;; - company-preview-search
;; - company-echo
;; - company-echo-common




(provide 'init-company-mode)

;;; init-company-mode.el ends here
