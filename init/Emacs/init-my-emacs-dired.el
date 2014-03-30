
;;; [ Dired ] (Directory editing mode)
;; Usage:
;; - [C-x d] -- dired (cond 'helm-mode (helm-mode-dired))
;; - [C-x C-d] -- simple entry list.
;; - [C-u C-x C-d] -- detail entry list.
(require 'dired)


;;; [ direx ] --- direx.el is a simple directory explorer. It also works as a generic tree explore library.

(require 'direx)

;; (global-set-key (kbd "C-c C-j") 'direx:jump-to-directory)

(push '(direx:direx-mode :position left :width 35 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-c C-j") 'direx:jump-to-directory-other-window)

;;; [ direx-project ] --- (bundled with direx.el) -- project tree explorer.
(require 'direx-project)

;; (global-set-key (kbd "C-c C-p") 'direx-project:jump-to-project-root)


(provide 'init-my-emacs-dired)
