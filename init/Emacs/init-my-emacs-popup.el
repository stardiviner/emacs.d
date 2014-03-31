;;; init-my-emacs-popup.el --- init popup settings for Emacs.
;;
;;; Commentary:

;;; Code:

;;; [ tooltip.el ]
(require 'tooltip)
(tooltip-mode t)

(require 'tooltip-help)


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
                    :foreground "black" :background "#EDED00")
(set-face-attribute 'popup-isearch-match nil
                    :inherit 'ac-candidate-face
                    :inverse-video nil
                    :foreground "#209FC9" :background "white"
                    :underline '(:color "dim gray" :style line)
                    )
(set-face-attribute 'popup-menu-face nil
                    :foreground "green")



;;; [ pos-tip ]
(require 'pos-tip)

;; TODO set popup items properties.
;; - face: popup-face
;; - selection-face: popup-selection-face
;; - document:
;; - summary:
;; - symbol:
;; - sublist:


;;; popup-pos-tip

(require 'popup-pos-tip)



;;; [ showtip ]
;; (load "~/.emacs.d/my-init/extensions/showtip.el")
;; (setq showtip-timeout 30)
;; (setq showtip-top-adjust 40)

;; (set-face-attribute 'showtip-face nil
;;                     :background "#444444" :foreground "white")




(provide 'init-my-emacs-popup)

;;; init-my-emacs-popup.el ends here
