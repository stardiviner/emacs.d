;;; init-my-emacs-workspace.el --- init for Emacs workspace.

;;; Commentary:



;;; Code:

;;; [ perspeen ] -- A emacs plugin for multi workspace.

;; (use-package perspeen
;;   :ensure t
;;   :config
;;   (setq perspeen-use-tab t)
;;   (setq perspeen-keymap-prefix (kbd "C-z"))
;;   (define-key perspeen-command-map (kbd "k") 'perspeen-delete-ws)
;;   (define-key perspeen-command-map (kbd "r") 'perspeen-rename-ws)
;;   (define-key perspeen-command-map (kbd "s") 'perspeen-goto-ws)
;;   (perspeen-mode 1)
;;   )

;;; [ window-purpose ] -- Purpose-based window management for Emacs.

;; (use-package window-purpose
;;   :ensure t
;;   :config
;;   (setq purpose-preferred-prompt 'ivy
;;         purpose-layout-dirs (locate-user-emacs-file ".purpose/layouts/")
;;         )
;;
;;   ;; (setq pop-up-frames t)
;;
;;   ;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;;   ;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;;   ;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;;   ;;
;;   ;; - popup-window
;;   ;; - sidebar-window
;;   ;; - help-window
;;   ;; - search-window
;;   ;; - compilation-window
;;   ;; - repl-window
;;   ;; - utility-window
;;
;;   (add-to-list 'purpose-user-mode-purposes '(popwin-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(compilation-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(comint-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(help-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(apropos-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(xref--xref-buffer-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(Man-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(ag-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(pt-search-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(dired-mode . sidebar-window))
;;   (add-to-list 'purpose-user-mode-purposes '(project-explorer-mode . sidebar-window))
;;   (add-to-list 'purpose-user-mode-purposes '(bm-show-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(process-menu-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(quickrun/mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(pdf-occur-buffer-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(pdf-outline-buffer-mode . search-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-lisp-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(sly-mrepl-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(slime-repl-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(slime-inspector-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inf-clojure-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(cider-clojure-interaction-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(cider-docview-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(cider-inspector-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-ess-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-julia-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(yari-mode . help-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inf-ruby-mode . popup-window))
;;   (add-to-list 'purpose-user-mode-purposes '(ruby-compilation-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(projectile-rails-generate-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(projectile-rails-compilation-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(projectile-rails-server-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-js-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(inferior-haskell-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(haskell-interactive-mode . repl-window))
;;   (add-to-list 'purpose-user-mode-purposes '(sbt-mode . compilation-window))
;;   (add-to-list 'purpose-user-mode-purposes '(calc-mode . utility-window))
;;   (add-to-list 'purpose-user-mode-purposes '(godoc-mode . help-window))
;;
;;
;;   ;; (setq purpose-special-action-sequences '(purpose-display-reuse-window-buffer
;;   ;;                                          purpose-display-reuse-window-purpose
;;   ;;                                          purpose-display-pop-up-frame
;;   ;;                                          popup-frame)
;;   ;;       )
;;
;;   (setq purpose-use-default-configuration t)
;;
;;   (purpose-compile-user-configuration)
;;
;;   (purpose-mode)
;;   )

;;; [ eyebrowse ] -- A simple-minded way of managing window configs in Emacs.

(use-package eyebrowse                  ; [C-c C-w]
  :ensure t
  :config
  (setq eyebrowse-new-workspace t)
  
  ;; also save side and slot windows configuration.
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))

  (eyebrowse-mode t)
  )


(provide 'init-my-emacs-workspace)

;;; init-my-emacs-workspace.el ends here
