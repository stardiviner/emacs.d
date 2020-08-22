;;; init-tool-downloader.el --- init for Downloader
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ download-region ] -- simple in-buffer download manager for Emacs.

(use-package download-region
  :ensure t
  :commands (download-region-as-url))

;;; [ aria2 ] -- a major mode for controlling aria2c RPC daemon downloader.

(use-package aria2
  :quelpa (aria2 :fetcher github :repo "stardiviner/aria2")
  :commands (aria2-downloads-list)
  :custom ((aria2-download-directory (expand-file-name "~/Downloads"))))

;;; [ youtube-dl ] -- A youtube-dl download manager for Emacs.

(use-package youtube-dl
  :quelpa (youtube-dl :fetcher github :repo "stardiviner/youtube-dl.el")
  :defer t
  :commands (youtube-dl youtube-dl-list)
  :custom ((youtube-dl-directory "~/Downloads/")
           (youtube-dl-proxy "socks5://127.0.0.1:1086")
           (youtube-dl-proxy-url-list '("youtube.com" "pornhub.com")))
  :config (add-to-list 'display-buffer-alist
                       '("^ \\*youtube-dl list\\*" . (display-buffer-below-selected))))


(provide 'init-tool-downloader)

;;; init-tool-downloader.el ends here
