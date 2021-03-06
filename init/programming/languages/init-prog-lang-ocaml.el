;;; init-prog-lang-ocaml.el --- init for OCaml
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ tuareg-mode ] -- major mode for OCaml.

(use-package tuareg
  :ensure t
  :defer t
  :init
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  :config
  ;; setup environment variables using opam
  ;; (dolist (var (car
  ;;               (read-from-string (shell-command-to-string
  ;;                                  "opam config env --sexp"))))
  ;;   (setenv (car var) (cadr var)))
  
  ;; utop top level
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))

;;; [ ob-ocaml ]

(use-package ob-ocaml
  :defer t
  :commands (org-babel-execute:ocaml)
  :config
  (add-to-list 'org-babel-load-languages '(ocaml . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("ocaml" . "ml")))

;;; [ merlin ] -- context-sensitive completion for OCaml in Vim and Emacs.

(use-package merlin
  :ensure t
  :defer t
  :init
  (defun my/merlin-mode-setup ()
    (merlin-mode 1)
    (my-company-add-backend-locally 'merlin-company-backend)
    (define-key tuareg-mode-map (kbd "C-c C-s") 'tuareg-run-metaocaml))
  (dolist (hook '(tuareg-mode-hook
                  caml-mode-hook))
    (add-hook hook #'my/merlin-mode-setup))
  
  :config
  ;; Use opam switch to lookup ocamlmerlin binary
  ;; (setq merlin-command 'opam)
  ;; (setq merlin-command "ocamlmerlin")
  ;; (setq tuareg-interactive-program "/usr/local/bin/opam config -- exec metaocaml")

  ;;(setq merlin-error-after-save nil)

  ;; complete
  (setq merlin-completion-dwim t
        merlin-completion-types t
        merlin-completion-arg-type t
        merlin-completion-with-doc t)

  (defun tuareg-run-metaocaml ()
    "Run an OCaml toplevel process.  I/O via buffer `*ocaml-toplevel*'."
    (interactive)
    (tuareg-run-process-if-needed
     "/usr/bin/opam config exec -- metaocaml")
    (display-buffer tuareg-interactive-buffer-name)))

;;; [ utop ] -- universal toplevel for OCaml.

(use-package utop
  :ensure t
  :defer t)


(provide 'init-prog-lang-ocaml)

;;; init-prog-lang-ocaml.el ends here
