;;; init-my-tool-downloader.el --- init for Downloader
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ download-region ] -- simple in-buffer download manager for Emacs.

(use-package download-region
  :ensure t
  :config
  (set-face-attribute 'download-region-downloading nil
                      :background "#004A5D" :foreground "white"
                      :box '(:color "cyan" :line-width -1)
                      )
  )


(provide 'init-my-tool-downloader)

;;; init-my-tool-downloader.el ends here
