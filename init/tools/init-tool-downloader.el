;;; init-tool-downloader.el --- init for Downloader
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ aria2 ] -- control `aria2c' commandline tool from Emacs.

(use-package aria2
  :ensure t
  :defer t
  :commands (aria2-downloads-list)
  :init (setq aria2-download-directory (expand-file-name "~/Downloads")))

;;; [ download-region ] -- simple in-buffer download manager for Emacs.

(use-package download-region
  :ensure t
  :commands (download-region-as-url))

;;; [ transmission ] -- An interface to a Transmission session for GNU Emacs.

(use-package transmission
  :ensure t
  :defer t
  :commands (transmission transmission-add)
  :init (add-to-list 'display-buffer-alist
                     '("\\*transmission\\*" .
                       (display-buffer-reuse-window display-buffer-below-selected))))

;;; [ youtube-dl ] -- A youtube-dl download manager for Emacs.

(use-package youtube-dl
  :load-path "~/Code/Emacs/youtube-dl-emacs/"
  :defer t
  :commands (youtube-dl youtube-dl-list)
  :init (setq youtube-dl-directory "~/Downloads/")
  (add-to-list 'display-buffer-alist
               '("^\\*youtube-dl list\\*" .
                 (display-buffer-reuse-window display-buffer-below-selected))))


(provide 'init-tool-downloader)

;;; init-tool-downloader.el ends here
