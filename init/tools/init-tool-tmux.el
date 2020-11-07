;;; init-tool-tmux.el --- init for Tmux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emamux ] -- Tmux manipulation from Emacs.

(use-package emamux
  :ensure t
  :defer t
  :custom (emamux:completing-read-type 'normal)
  :commands (emamux:run-command))


(provide 'init-tool-tmux)

;;; init-tool-tmux.el ends here
