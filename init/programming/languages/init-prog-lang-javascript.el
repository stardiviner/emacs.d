;;; init-prog-lang-javascript.el --- init JavaScript for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ JavaScript ]

(use-package js
  :mode (("\\.js\\'" . js-mode)
         ("\\.jsx\\'" . js-jsx-mode))
  :config
  ;; fix `js-find-symbol' [M-.] overriding other packages' keybinding.
  (substitute-key-definition 'js-find-symbol 'xref-find-definitions js-mode-map))

;;; [ ob-js ]

(use-package ob-js
  :defer t
  :commands (org-babel-execute:js)
  :config
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
  (add-to-list 'org-babel-default-header-args:js '(:results . "output"))

  (defun ob-js-insert-session-header-arg (session)
    "Insert ob-js `SESSION' header argument.
- `js-comint'
- `skewer-mode'
- `Indium'
"
    (interactive (list (completing-read "ob-js session: "
                                        `(,(when (featurep 'js-comint) "js-comint")
                                          ,(when (featurep 'skewer-mode) "skewer-mode")
                                          ,(when (featurep 'indium) "indium")))))
    (org-babel-insert-header-arg
     "session"
     (pcase session
       ("js-comint" "\"*Javascript REPL*\"")
       ("skewer-mode" "\"*skewer-repl*\"")
       ("indium" "\"*JS REPL*\""))))
  (define-key org-babel-map (kbd "J") 'ob-js-insert-session-header-arg))

;;; [ ob-deno ] -- Babel Functions for Javascript/TypeScript with Deno.

(use-package ob-deno
  :ensure t
  :commands (org-babel-execute:deno)
  :config
  (add-to-list 'org-babel-load-languages '(deno . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("deno" . "js"))
  (add-to-list 'org-babel-default-header-args:deno '(:results . "output"))
  ;; optional (require the typescript.el)
  ;; (add-to-list 'org-src-lang-modes '("deno" . typescript))
  )

;;; [ nvm ] -- Manage Node versions within Emacs.

;; (use-package nvm
;;   :ensure t
;;   :init (nvm-use "v13.5.0"))

;;; [ lsp-mode ] -- Javascript and Typescript support for lsp-mode using javascript-typescript-langserver.

(use-package lsp-mode
  :ensure t
  :hook ((js-mode rjsx-mode) . lsp))

;;; [ npm-mode ] -- minor mode for working with npm projects.

(use-package npm-mode
  :ensure t
  :hook (js-mode . npm-mode))

;;; [ npm ] -- NPM client for Emacs.

(use-package npm
  :ensure t
  :defer t
  :commands (npm-menu))

;;; [ typescript-mode ] -- Major mode for editing typescript.

(use-package typescript-mode
  :ensure t
  :defer t)

;;; [ ob-typescript ] -- org-babel functions for typescript evaluation.

(use-package ob-typescript
  :defer t
  :commands (org-babel-execute:typescript)
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))
  (add-to-list 'org-babel-default-header-args:typescript '(:results . "output")))

;;; [ tide ] -- Typescript Interactive Development Environment.

(use-package tide
  :ensure t
  :after typescript-mode
  :commands (tide-mode)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

;;; [ lsp-mode ] -- Javascript and Typescript support for lsp-mode using javascript-typescript-langserver.

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode) . lsp))

;;; [ osa ] -- OSA (JavaScript / AppleScript) bridge.

(use-package osa
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vue.js                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vue-mode
  :ensure t
  :ensure vue-html-mode)


(provide 'init-prog-lang-javascript)

;;; init-prog-lang-javascript.el ends here
