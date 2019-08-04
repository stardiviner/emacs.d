;;; init-macOS.el --- init for Mac OS X
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; For MacOS
;; (setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
;; (setq mac-option-modifier 'super) ; sets the Option key as Super
;; (setq mac-command-modifier 'meta) ; sets the Command key as Meta
;; (setq mac-control-modifier 'meta) ; sets the Control key as Meta

;; Use Command key as Meta
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
(global-set-key [kp-delete] 'delete-char) ; sets fn-delete to be right-delete

;;; fix macOS /usr/local/bin/ path not in Emacs default path.
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;; [ exec-path-from-shell ] -- Make Emacs use the $PATH set up by the user's shell.

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

;;; variable-pitch faces settings for Org Mode.
(set-face-attribute 'variable-pitch nil
                    :family "DejaVu Sans Mono"
                    :height 120
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "Hack"
                    :height 120
                    :weight 'normal :slant 'normal)
(set-face-attribute 'fixed-pitch-serif nil
                    :family "Hack"
                    :height 120
                    :weight 'normal :slant 'italic)

;;; [ company-xcode ]

(use-package company
  :ensure t
  :config
  (defun my-xcode-setup ()
    (require 'company-xcode)
    (my-company-add-backend-locally 'company-xcode)))

;;; [ counsel-osx-app ]

(use-package counsel-osx-app
  :ensure t
  :defer t
  :commands (counsel-osx-app))

;;; [ helm-xcdoc ] -- Search Xcode Document by docsetutil and eww with Helm interface.

(use-package helm-xcdoc
  :ensure t
  :defer t
  :commands (helm-xcdoc-search))

;;; [ dash-alfred ] -- Search Dash on-the-fly in Emacs.

(use-package dash-alfred
  :ensure t
  :defer t
  :commands (dash-alfred-helm dash-alfred-ivy))

;;; [ sysctl ] -- Manage sysctl through Emacs.

(use-package sysctl
  :ensure t
  :defer t
  :commands (sysctl))


(provide 'init-macOS)

;;; init-macOS.el ends here
