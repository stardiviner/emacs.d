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
                         company-xcode
                         company-bbdb
                         (company-dabbrev-code company-yasnippet company-gtags company-etags company-keywords)
                         company-oddmuse company-files company-dabbrev company-abbrev
                         ;; company-ispell
                         ))

;; To use company-mode in all buffers, add the following line to your init file:
(add-hook 'after-init-hook 'global-company-mode)
(diminish company-mode)

;; keybindings
;; TODO:
(define-key company-mode-map (kbd "M-n") 'company-select-next)
(define-key company-mode-map (kbd "M-p") 'company-select-previous)
(define-key company-mode-map [return] nil)
(define-key company-mode-map (kbd "M-j") 'company-complete-selection)
(define-key company-mode-map [mouse-1] 'company-complete-mouse)
(define-key company-mode-map [mouse-3] 'company-select-mouse)
(define-key company-mode-map (kbd "C-g") 'company-abort)
(define-key company-mode-map [tab] 'company-complete-common)
(define-key company-mode-map (kbd "M-i") 'company-complete-common)
(define-key company-mode-map (kbd "<f1>") 'company-show-doc-buffer)
(define-key company-mode-map (kbd "M-h") 'company-show-doc-buffer)
(define-key company-mode-map (kbd "C-w") 'company-show-location)
(define-key company-mode-map (kbd "C-s") 'company-search-candidates)
(define-key company-mode-map (kbd "C-M-s") 'company-filter-candidates)
(define-key company-search-map (kbd "C-g") 'company-search-abort)
(define-key company-search-map (Kbd "C-s") 'company-search-repeat-forward)
(define-key company-search-map (kbd "C-r") 'company-search-repeat-backward)
(define-key company-search-map (kbd "C-o") 'company-search-kill-others)

;; TODO:
;; faces
;; - company-tooltip
;; - company-tooltip-selection
;; - company-tooltip-mouse
;; - company-tooltip-common
;; - company-tooltip-common-selection
;; - company-tooltip-annotation
;; - company-scrollbar-fg
;; - company-scrollbar-bg
;; - company-preview
;; - company-preview-common
;; - company-preview-search
;; - company-echo
;; - company-echo-common




(provide 'init-company-mode)

;;; init-company-mode.el ends here
