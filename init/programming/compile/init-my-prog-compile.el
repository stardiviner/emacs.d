;;; init-prog-compile.el --- Summary
;;
;;; Commentary:

;;; Code:

;;; [ compile ]

(setq compilation-ask-about-save nil ; save without asking.
      compilation-scroll-output 'next-error ; stop on first error.
      compilation-skip-threshold 2 ; don't stop on info or warnings.
      )

(setq compilation-window-height nil ; use Emacs default.
      )

(set-face-attribute 'compilation-line-number nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "cyan"
                    :underline nil
                    :weight 'normal
                    :box '(:color "black" :line-width -1))
(set-face-attribute 'compilation-column-number nil
                    :background (color-darken-name (face-background 'default) 5)
                    :foreground "orange"
                    :underline nil
                    :weight 'normal :slant 'normal
                    :box '(:color "black" :line-width -1))
(set-face-attribute 'compilation-info nil
                    :background (color-darken-name (face-background 'default) 5)
                    :underline nil
                    :weight 'normal
                    )
(set-face-attribute 'compilation-warning nil
                    :foreground "orange")
(set-face-attribute 'compilation-error nil
                    :background "dark red"
                    ;; :family nil
                    )
(set-face-attribute 'compilation-mode-line-exit nil
                    :inherit 'compilation-info
                    :foreground "forest green")
(set-face-attribute 'compilation-mode-line-fail nil
                    :foreground "black" :background "dark red")
(set-face-attribute 'compilation-mode-line-run nil
                    :foreground "black" :background nil)



;;; [ smart-compile ]

;;; Usage:
;; - [M-x smart-compile]

(require 'smart-compile)


;;; [ smart-compile+ ]

;;; Usage:
;; - [M-x smart-compile]

(require 'smart-compile+)


(add-to-list 'smart-run-alist '("\\.py$" . "python %n"))
(add-to-list 'smart-run-alist '("\\.rb$" . "ruby %n"))



(global-set-key [f5] 'smart-compile)


(provide 'init-my-prog-compile)

;;; init-my-prog-compile.el ends here
