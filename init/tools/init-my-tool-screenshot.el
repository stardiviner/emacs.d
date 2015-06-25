;;; init-my-tool-screenshot.el --- init for Screenshot
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ screenshot.el ]

;; Take a screenshot by ImageMagick in Emacs easily. Then send the image to
;; remote host by scp (optional). Finally the URL or filename is in the
;; kill-ring. The screenshot file format is PostScript. Use PDF viewer to open it.
;;
;; - [M-x screenshot]
;; - [M-x screenshot-take]

(require 'screenshot)

(setq screenshot-schemes
      '(
        ;; To local image directory
        ("local"
         :dir "~/Screenshots/")            ; Image repository directory
        ;; To current directory
        ("current-directory"          ; No need to modify
         :dir default-directory)
        ;; To remote ssh host
        ("remote-ssh"
         :dir "/tmp/"                 ; Temporary saved directory
         :ssh-dir "www.example.org:public_html/archive/" ; SSH path
         :url "http://www.example.org/archive/")  ; Host URL prefix
        ;; To EmacsWiki (need yaoddmuse.el)
        ("EmacsWiki"                 ; Emacs users' most familiar Oddmuse wiki
         :dir "~/.yaoddmuse/EmacsWiki/"  ; same as yaoddmuse-directory
         :yaoddmuse "EmacsWiki")         ; You can specify another Oddmuse Wiki
        ;; To local web server
        ("local-server"
         :dir "~/public_html/"           ; local server directory
         :url "http://127.0.0.1/")))     ; local server URL prefix

(setq screenshot-default-scheme "local")

(setq screenshot-take-delay 0.5)

(define-key my-tools-prefix-map (kbd "s") 'screenshot-take)
(define-key my-tools-prefix-map (kbd "S") 'screenshot)


(provide 'init-my-tool-screenshot)

;;; init-my-tool-screenshot.el ends here
