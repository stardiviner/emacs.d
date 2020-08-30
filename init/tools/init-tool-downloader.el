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
  ;; :quelpa (aria2 :fetcher github :repo "stardiviner/aria2")
  :load-path "~/Code/Emacs/aria2"
  :commands (aria2-downloads-list)
  :custom ((aria2-download-directory (expand-file-name "~/Downloads")))
  :init (add-to-list 'display-buffer-alist '("\\*aria2: downloads list\\*" . (display-buffer-below-selected))))

;;; [ youtube-dl ] -- A youtube-dl download manager for Emacs.

(use-package youtube-dl
  ;; :quelpa (youtube-dl :fetcher github :repo "stardiviner/youtube-dl.el")
  :load-path "~/Code/Emacs/youtube-dl"
  :defer t
  :commands (youtube-dl youtube-dl-list)
  :custom ((youtube-dl-directory "~/Downloads/")
           (youtube-dl-proxy "socks5://127.0.0.1:1086")
           (youtube-dl-proxy-url-list '("youtube.com" "pornhub.com")))
  :init
  (add-to-list 'display-buffer-alist '("^ \\*youtube-dl list\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^ \\*youtube-dl log\\*" . (display-buffer-below-selected)))
  :config
  ;; auto download and embed subtitles of video
  (add-to-list 'youtube-dl-arguments "--write-sub")
  (add-to-list 'youtube-dl-arguments "--write-auto-sub")
  (add-to-list 'youtube-dl-arguments "--embed-subs")
  (add-to-list 'youtube-dl-arguments "--sub-lang" t)
  (add-to-list 'youtube-dl-arguments "en,zh-Hans" t))


(provide 'init-tool-downloader)

;;; init-tool-downloader.el ends here
