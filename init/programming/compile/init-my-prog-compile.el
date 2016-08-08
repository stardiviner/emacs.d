;;; init-prog-compile.el --- Summary
;;
;;; Commentary:

;;; Code:

;;; [ compile ]

(setq compilation-ask-about-save t ; save without asking.
      compilation-skip-threshold 2 ; don't stop on info or warnings.
      compilation-window-height 7
      compilation-scroll-output 'first-error ; stop on first error.
      ;; NOTE: t: will cause to can't jump to error location.
      compilation-auto-jump-to-first-error nil
      ;; compilation-auto-jump-to-next
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
                    :foreground "black")


;;; [ smart-compile ]

;; (use-package smart-compile
;;   :ensure t)


;;; [ smart-compile+ ]

;;; Usage:
;; - [M-x smart-compile]


;;; [ quickrun ] -- Run command quickly.

(use-package quickrun
  :ensure t
  :config
  (setq quickrun-focus-p t)

  (quickrun-set-default "c" "c/clang")
  (quickrun-set-default "c++" "c++/clang++")

  ;; Examples:
  ;; check out `quickrun/template-place-holders' for description of `%?'.
  
  ;; Use this parameter as C++ default
  ;; (quickrun-add-command "c++/c11"
  ;;                       '((:command . "g++")
  ;;                         (:exec    . ("%c -std=c++11 %o -o %e %s"
  ;;                                      "%e %a"))
  ;;                         (:remove  . ("%e")))
  ;;                       :default "c++")

  (quickrun-add-command "browser/firefox"
                        '((:command . "firefox")
                          (:exec    . ("%c %s"))
                          :default "browser"))
  (quickrun-add-command "browser/chrome"
                        '((:command . "google-chrome-stable")
                          (:exec    . ("%c %s"))
                          :default "browser"))
  (quickrun-set-default "html" "browser/chrome")
  )


;;; [ multi-compile ] -- multiple targets compile

;;; Usage:
;;
;; - `multi-compile-run' ::

;; (use-package multi-compile
;;   :ensure t
;;   :config
;;   (setq multi-compile-completion-system 'ivy-read)
;;   )



(if (featurep 'quickrun)
    (global-set-key [f5] 'quickrun)
  (global-set-key [f5] 'smart-compile))


(provide 'init-my-prog-compile)

;;; init-my-prog-compile.el ends here
