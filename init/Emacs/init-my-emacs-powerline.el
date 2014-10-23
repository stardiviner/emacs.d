;;; init-my-emacs-powerline.el --- init for Powerline
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ powerline (emacs-powerline) ]

(require 'powerline)

(setq powerline-arrow-shape 'arrow) ; 'arrow, 'curve, 'half,

(setq powerline-color1 "#0088CC")
(setq powerline-color2 "white")
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(powerline-default-theme)
;; (powerline-center-theme)

(set-face-attribute 'mode-line nil
                    :family "DejaVu Sans Mono for Powerline"
                    :foreground "#0044CC" :background "grey40")
(set-face-attribute 'mode-line-inactive nil
                    :family "DejaVu Sans Mono for Powerline"
                    :foreground "grey22" :background "grey40")
(set-face-attribute 'powerline-active1 nil
                    :family "DejaVu Sans Mono for Powerline"
                    :foreground "#0088CC" :background "grey40")
(set-face-attribute 'powerline-active2 nil
                    :family "DejaVu Sans Mono for Powerline"
                    :foreground "white" :background "grey40")
(set-face-attribute 'powerline-inactive1 nil
                    :family "DejaVu Sans Mono for Powerline"
                    :foreground "grey22" :background "grey40")
(set-face-attribute 'powerline-inactive2 nil
                    :family "DejaVu Sans Mono for Powerline"
                    :foreground "white" :background "grey40")

;; (setq-default mode-line-format
;;            '("%e"
;;              (:eval
;;               (let* ((active (eq (frame-selected-window) (select-window)))
;;                      (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                      (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                      (lhs (list
;;                            (powerline-raw "%*" nil 'l)
;;                            (powerline-buffer-size nil 'l)
;;                            (powerline-buffer-id nil 'l)
;;                            (powerline-raw " ")
;;                            (powerline-arrow-right nil face1)
;;                            (powerline-major-mode face1 'l)
;;                            (powerline-minor-modes face1 'l)
;;                            (powerline-raw mode-line-process face1 'l)
;;                            (powerline-narrow face1 'l)
;;                            (powerline-arrow-right face1 face2)
;;                            (powerline-vc face2)
;;                            ))
;;                      (rhs (list
;;                            (powerline-raw global-mode-string face2 'r)
;;                            (powerline-arrow-left face2 face1)
;;                            (powerline-raw "%41" face1 'r)
;;                            (powerline-raw ":" face1)
;;                            (powerline-raw "%3c" face1 'r)
;;                            (powerline-arrow-left face1 nil)
;;                            (powerline-raw " ")
;;                            (powerline-raw "%6p" nil 'r)
;;                            (powerline-hud face2 face1))))
;;                 (concat
;;                  (powerline-render lhs)
;;                  (powerline-fill face2 (powerline-width rhs))
;;                  (powerline-render rhs))))))


(provide 'init-my-emacs-powerline)

;;; init-my-emacs-powerline.el ends here
