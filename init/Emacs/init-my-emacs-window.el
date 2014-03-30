;;; init-my-emacs-window.el --- my Emacs window init

;;; Commentary:


;;; Code:

;;; [ switch-window ] -- show a number on window instead of modeline.
;; (require 'switch-window)
;; (global-set-key (kbd "C-x o") 'switch-window)


;;; [ window-number ] --
(require 'window-number)

(window-number-mode)

(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according
  to numbers with the C-x C-j prefix. Another mode,
  `window-number-meta-mode' enables the use of the M- prefix."
  t)

;; (autoload 'window-number-meta-mode "window-number"
;; "A global minor mode that enables use of the M- prefix to select
;; windows, use `window-number-mode' to display the window numbers in
;; the mode-line."
;; t)

;; (push (cons 'my-window-number-meta-mode my-window-number-mode-map) minor-mode-map-alist)

;;; window-number face
(set-face-attribute 'window-number-face nil
                    :background "red" :foreground "black"
                    :box '(:color "dark red" :line-width 1 :style nil)
                    :bold 'normal)


;;; [ window-numbering ] --
;; (unless (package-installed-p 'window-numbering)
;;   (package-install 'window-numbering))
;; (require 'window-numbering)



(provide 'init-my-emacs-window)

;;; init-my-emacs-window.el ends here
