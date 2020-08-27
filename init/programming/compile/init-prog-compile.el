;;; init-prog-compile.el --- Summary
;;
;;; Commentary:

;;; Code:

;;; [ compile ]

(use-package compile
  :defer t
  :commands (compile)
  :custom ((compilation-ask-about-save t) ; save without asking.
           (compilation-skip-threshold 2) ; don't stop on info or warnings.
           (compilation-window-height 7)
           (compilation-scroll-output 'first-error) ; 'first-error ; stop on first error.
           (compilation-auto-jump-to-first-error nil) ; jump to error file position.
           (compilation-auto-jump-to-next nil))
  :init
  (add-to-list 'display-buffer-alist '("^\\*compilation\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*Compile-Log\\*" . (display-buffer-below-selected)))
  ;; make `compile-goto-error' open result target in current window.
  (add-to-list 'display-buffer-alist
               '((lambda (&rest _) (eq this-command 'compile-goto-error)) .
                 ((display-buffer-same-window)
                  (inhibit-same-window . nil)))))

;;; [ quickrun ] -- Run command quickly.

(use-package quickrun
  :ensure t
  :bind ([f5] . quickrun)
  :custom (quickrun-focus-p t)
  :init (add-to-list 'display-buffer-alist '("^\\*quickrun\\*" . (display-buffer-below-selected)))
  :config
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
  (quickrun-set-default "html" "browser/firefox"))


(provide 'init-prog-compile)

;;; init-prog-compile.el ends here
