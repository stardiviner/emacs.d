;;; init-prog-compile.el --- Summary
;;
;;; Commentary:

;;; Code:

;;; [ compile ]

(setq compilation-ask-about-save t ; save without asking.
      compilation-skip-threshold 2 ; don't stop on info or warnings.
      compilation-window-height 7
      compilation-scroll-output t ; 'first-error ; stop on first error.
      compilation-auto-jump-to-first-error nil ; jump to error file position.
      compilation-auto-jump-to-next nil
      )

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


(if (featurep 'quickrun)
    (global-set-key [f5] 'quickrun)
  (global-set-key [f5] 'smart-compile))


(provide 'init-my-prog-compile)

;;; init-my-prog-compile.el ends here
