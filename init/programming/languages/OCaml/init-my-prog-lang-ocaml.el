;;; init-my-prog-lang-ocaml.el --- init for OCaml
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ tuareg-mode ] -- major mode for OCaml.

(use-package tuareg
  :ensure t
  :init
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Mode majeur pour éditer du code Caml" t)
  (autoload 'camldebug "camldebug" "Exécuter le débogueur Caml" t)
  :config
  (when (string= (getenv "MY_EMACS_DAEMON") "ocaml")
    (load-file "~/.emacs.d/elisp/daemon/my-tuareg-daemon.el"))
  
  ;; setup environment variables using opam
  (dolist (var (car
                (read-from-string (shell-command-to-string
                                   "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  
  ;; update the emacs path
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory)))
  
  ;; update the emacs load path
  (add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                            (getenv "OCAML_TOPLEVEL_PATH")))
  
  ;; utop top level
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  )


;;; [ merlin ] -- context-sensitive completion for OCaml in Vim and Emacs.

(use-package merlin
  :ensure t
  :config
  ;; Add opam emacs directory to the load-path
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  ;; Load merlin-mode
  (require 'merlin)
  ;; Start merlin on ocaml files
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  ;; Make company aware of merlin
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  ;; Enable company on merlin managed buffers
  (add-hook 'merlin-mode-hook 'company-mode)
  ;; Or enable it globally:
  ;;(add-hook 'after-init-hook 'global-company-mode)

  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam)

  (defun tuareg-run-metaocaml ()
    "Run an OCaml toplevel process.  I/O via buffer `*ocaml-toplevel*'."
    (interactive)
    (tuareg-run-process-if-needed
     "/usr/bin/opam config exec -- metaocaml")
    (display-buffer tuareg-interactive-buffer-name))

  (add-hook 'tuareg-mode-hook
            ' (lambda ()
                (define-key tuareg-mode-map (kbd "C-c M-s")
                  'tuareg-run-metaocaml)))

  (setq tuareg-interactive-program "/usr/local/bin/opam config -- exec metaocaml")
  ;;(setq merlin-use-auto-complete-mode t)
  ;;(setq merlin-error-after-save nil)
  )


;;; [ utop ] -- universal toplevel for OCaml.

(use-package utop
  :ensure t)


(provide 'init-my-prog-lang-ocaml)

;;; init-my-prog-lang-ocaml.el ends here
