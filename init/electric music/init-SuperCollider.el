;;; init-SuperCollider.el --- init for SuperCollider

;;; Commentary:

;;; SuperCollider
;;
;; An audio server, programming language, and IDE for sound synthesis and
;; algorithmic composition.

;;; Code:


(use-package supercollider
  ;; from Linux System Package (which only contains *.elc files)
  :load-path "/usr/share/emacs/site-lisp/SuperCollider/"
  ;; from Git source code repository (contains *.el source code files)
  ;; :load-path (concat (getenv "HOME")
  ;;                    "/Code/SuperCollider/supercollider"
  ;;                    "/editors/scel/el/")
  :config
  (require 'sclang)
  (require 'sclang-interp)
  (require 'w3m)

  ;; (setq sclang-extension-path '("/usr/share/SuperCollider/Extensions"
  ;;                               "~/.local/share/SuperCollider/Extensions"))

  ;; Sclang Interface
  (setq sclang-auto-scroll-post-buffer t
        sclang-show-workspace-on-startup nil
        sclang-use-symbol-table t
        sclang-main-run nil
        sclang-main-stop nil
        )

  ;; Sclang mode
  (setq sclang-indent-level 2)

  ;; Sclang minor mode

  ;; auto-complete for SuperCollider
  ;; (add-hook 'sclang-mode-hook
  ;;           (lambda ()
  ;;             (company-mode -1)
  ;;             (setq-local ac-auto-start 1)
  ;;             ))

  ;; company-mode for SuperCollider
  (add-hook 'sclang-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'sclang-complete-symbol nil t)
              ))

  (define-key sclang-mode-map (kbd "C-c M-r") 'sclang-main-run)
  (define-key sclang-mode-map (kbd "C-c M-s") 'sclnag-main-stop)
  (define-key sclang-mode-map (kbd "C-c C-z") 'sclang-switch-to-past)
  (define-key sclang-mode-map (kbd "C-c C-w") 'sclang-switch-to-workspace)
  (define-key sclang-mode-map (kbd "M-.") 'sclang-find-definitions)
  (define-key sclang-mode-map (kbd "M-,") 'sclang-pop-definition-mark)
  (define-key sclang-post-buffer-mode-map (kbd "C-c C-z") 'sclang-switch-to-src)
  (define-key sclang-post-buffer-mode-map (kbd "C-c C-w") 'sclang-switch-to-workspace)

  ;; auto start SuperCollider inferior process
  (defun my-sclang-auto-start ()
    "Start SuperCollider inferior process."
    (interactive)
    (unless (or (equal (buffer-name) sclang-post-buffer)
                (sclang-get-process))
      (sclang-start)))

  ;; (add-hook 'sclang-mode-hook #'my-sclang-auto-start)
  (define-key sclang-mode-map (kbd "C-c C-s") 'my-sclang-auto-start)
  )


;;; [ sclang-extensions ] -- A collection of minor modes that improve your SuperCollider experience within Emacs.

(use-package sclang-extensions
  :ensure t
  :config
  (setq sclang-bury-post-on-start? t
        sclang-run-supercollider-if-not-active? nil ; run SuperCollider process will mute System sound.
        )
  
  (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
  )


;;; [ Overtone ] -- Combine SuperCollider + Clojure.

(defun my/overtone-auto-start ()
  "Auto start Overtone in Emacs."
  (interactive)
  ;; for external server
  (my/cider-repl-eval "(use 'overtone.core)")
  (my/cider-repl-eval "(overtone.core/boot-external-server)")
  ;; for internal server
  ;; (my/cider-repl-eval "(use 'overtone.live)")
  ;; (my/cider-repl-eval "(overtone.live/boot-server)")
  )

(add-hook 'cider-connected-hook #'my/overtone-auto-start)

;;; auto load with `clomacs'.
(clomacs-defun overtone-load-and-boot-external-server
               overtone.core/boot-external-server
               :lib-name "overtone"
               :namespace overtone.core
               :doc "Load Overtone library and boot external server.")

(clomacs-defun overtone-load-and-boot-internal-server
               overtone.live/boot-server
               :lib-name "overtone"
               :namespace overtone.live
               :doc "Load Overtone library and boot internal server.")

;; (overtone-load-and-boot-external-server)
;; (overtone-load-and-boot-internal-server)



(provide 'init-SuperCollider)

;;; init-SuperCollider.el ends here
