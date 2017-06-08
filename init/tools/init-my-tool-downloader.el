;;; init-my-tool-downloader.el --- init for Downloader
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ aria2 ] -- control `aria2c' commandline tool from Emacs.

(use-package aria2
  :ensure t
  :config
  (setq aria2-download-directory (expand-file-name "~/Downloads"))
  )

;;; [ download-region ] -- simple in-buffer download manager for Emacs.

(use-package download-region
  :ensure t
  :defer t
  )


(provide 'init-my-tool-downloader)

;;; init-my-tool-downloader.el ends here
