;;; init-my-tool-tmux.el --- init for Tmux
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emamux ] -- Tmux manipulation from Emacs.

(use-package emamux
  :ensure t
  :config
  (setq emamux:completing-read-type 'normal)
  )


(provide 'init-my-tool-tmux)

;;; init-my-tool-tmux.el ends here
