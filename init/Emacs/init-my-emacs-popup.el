;;; init-my-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;;; [ tooltip ]

(require 'tooltip)
(tooltip-mode t)

(setq-default tooltip-delay 0.3         ; default 0.7
              tooltip-hide-delay 10     ; default 10
              tooltip-short-delay 0.1   ; default 0.1
              tooltip-x-offset 5
              tooltip-y-offset 20
              ;; tooltip-functions '(tooltip-help-tips)
              ;; tooltip-hide-time nil
              tooltip-frame-parameters '((name . "tooltip")
                                         (internal-border-width . 2)
                                         (border-width . 2))
              )

(set-face-attribute 'tooltip nil
                    :foreground "black" :background "khaki"
                    :family "DejaVu Sans Mono")

;; (set-face-attribute 'tooltip nil
;;                     :foreground "dark gray" :background "#002630")

(require 'tooltip-help)

;; (setq th-max-tooltip-lines 30
;;       th-titlebar-height 0)


;;; [ popup ]

;; Features:
;;  - Tooltip
;;      (popup-tip)
;;  - Popup Menu
;;      (popup-menu*)
;;  - Popup Cascade Menu
;;      (popup-cascade-menu)
;; e.g. (popup-cascade-menu '(("Top1" "Sub1" "Sub2") "Top2"))
;;      Navigate between menu candidate and cascade menu with [C-f], [C-b]

(require 'popup)

(set-face-attribute 'popup-tip-face nil
                    :foreground "black" :background "#EDED00"
                    :family "DejaVu Sans Mono")
(set-face-attribute 'popup-isearch-match nil
                    :inherit 'ac-candidate-face
                    :inverse-video nil
                    :foreground "#209FC9" :background "white"
                    :underline '(:color "dim gray" :style line)
                    )
(set-face-attribute 'popup-menu-face nil
                    :inherit 'ac-candidate-face
                    :foreground "green yellow" :background "black")

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-j") 'popup-select)
;; (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
;; (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
;; (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)



;;; [ pos-tip ]

(require 'pos-tip)

(setq pos-tip-use-relative-coordinates nil
      ;; pos-tip-frame-offset
      ;; pos-tip-frame-offset-array
      pos-tip-border-width 1
      pos-tip-internal-border-width 10
      ;; pos-tip-tab-width
      )

(setq pos-tip-foreground-color "white"
      pos-tip-background-color "khaki")

;; (setq pos-tip-background-color "#002630" ; '(color-darken-name (face-background 'default) 1)
;;       pos-tip-foreground-color "dark gray")


;;; [ popup-pos-tip ]

(require 'popup-pos-tip)



;;; [ showtip ]
;; (load "~/.emacs.d/my-init/extensions/showtip.el")
;; (setq showtip-timeout 30)
;; (setq showtip-top-adjust 40)

(require 'showtip)

;; (set-face-attribute 'showtip-face nil
;;                     :inherit 'tooltip
;;                     :family "DejaVu Sans Mono"
;;                     ;; :family "WenQuanYi Zen Hei Mono"
;;                     )


(provide 'init-my-emacs-popup)

;;; init-my-emacs-popup.el ends here
