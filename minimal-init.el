;;; minimal-init.el --- minimal init file for testing.

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)
(require 'package)

(setq package-enable-at-startup nil)
(setq package-menu-async t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

;;; Load `use-package' ahead before `package-initialize' for (use-package org :pin manual ...).

;;; [ use-package ]
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                     ; if you use any `:bind' variant
(use-package delight                    ; if you use `:delight'
  :ensure t)
(setq use-package-verbose t ; 'debug: any evaluation errors report to `*use-package*` buffer.
      use-package-always-ensure nil)

;;; [ Org Mode (source code) ]
(if (not (file-exists-p "~/Code/Emacs/org-mode/lisp/"))
    (progn
      (use-package org
        :pin org
        :ensure t
        :mode (("\\.org\\'" . org-mode)
               ("\\.org_archive\\'" . org-mode))
        :custom ((org-modules nil) ; disable all extra org-mode modules to speed-up Org-mode file opening.
                 (org-startup-folded t)
                 (org-agenda-inhibit-startup t)))
      (use-package org-plus-contrib
        :pin org
        :ensure t))

  ;; disable Emacs built-in Org Mode
  (delete (format "/usr/local/share/emacs/%s/lisp/org" emacs-version) load-path)
  (delete "/usr/share/emacs/site-lisp/org/" load-path)
  
  (use-package org
    :pin manual
    :load-path "~/Code/Emacs/org-mode/lisp/"
    :defer t
    :mode (("\\.org\\'" . org-mode))
    :custom ((org-modules nil) ; disable all extra org-mode modules to speed-up Org-mode file opening.
             (org-startup-folded t)
             (org-agenda-inhibit-startup t))
    ;; load org before org-mode init files settings.
    :init (require 'org))
  (use-package org-plus-contrib
    :pin manual
    :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
    :defer t
    :no-require t)
  ;; add source code version Org-mode Info into Emacs.
  (if (file-exists-p "~/Code/Emacs/org-mode/doc/org")
      (with-eval-after-load 'info
        (add-to-list 'Info-directory-list "~/Code/Emacs/org-mode/doc/")
        (info-initialize))))

;;=============================== helpful packages ==============================
;;; add your customizations from here

(use-package ace-window
  :ensure t
  :bind ("C-x C-j" . ace-window))

;;=========================== minimal config required for debugging===============

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))



(provide 'minimal-init)

;;; minimal-init.el ends here
