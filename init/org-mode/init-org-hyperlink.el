;;; init-org-hyperlink.el --- init for Org Hyperlinks
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Check out variable `org-link-parameters'.

(setq org-indirect-buffer-display 'current-window
      org-link-use-indirect-buffer-for-internals t
      org-link-keep-stored-after-insertion t)

(define-key org-mode-map (kbd "M-,") 'org-mark-ring-goto)

;;; use :ID: property for org linking.
;; (use-package org-id
;;   :init
;;   (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
;;         org-id-track-globally t
;;         org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)))


;; (add-hook 'org-follow-link-hook #'sound-tick)

;;; Open and play GIF image in Emacs buffer.
(defun my/open-and-play-gif-image (file &optional link)
  "Open and play GIF image `FILE' in Emacs buffer.
Optional for Org-mode file: `LINK'."
  (let ((gif-image (create-image file))
        (tmp-buf (get-buffer-create "*Org-mode GIF image animation*")))
    (switch-to-buffer tmp-buf)
    (erase-buffer)
    (insert-image gif-image)
    (image-animate gif-image nil t)
    (local-set-key (kbd "q") 'kill-current-buffer)))

(defun emms-play-file-for-org (file &optional link)
  "An wrapper function on `emms-play-file'."
  (emms-play-file file))

(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        (remote . emacs)
        (t . default)))

;;; Links are now customizable
;;
;; Links can now have custom colors, tooltips, keymaps, display behavior, etc.
;; Links are now centralized in `org-link-parameters'.
;; (add-to-list 'org-link-parameters '())
;; `org-link-types'
;; `org-link-set-parameters'

(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file) ; `find-file-other-window'
        (wl . wl-other-frame)))

(cl-case system-type
  ('gnu/linux
   (add-to-list 'org-file-apps
                `(,(if (executable-find "firefox")
                       `("\.x?html?\\'" . "firefox %s")
                     `("\.x?html?\\'" . "google-chrome-unstable %s"))))
   
   (dolist (pair '(;; PDF
                   ;; use Okular
                   ;; ("\\.pdf\\'" . "okular %s")
                   ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . "okular -p %1 %s")
                   ;; CHM
                   ("\\.chm\\'" . "kchmviewer %s")
                   ;; Djvu
                   ;; FIXME:
                   ;; ("\\.djvu\\'" . doc-view-mode)
                   ;; EBooks
                   ;; ("\\.epub\\'" . "okular %s") ; it is opened by `ereader', and `nov'.
                   ("\\.mobi\\'" . "ebook-viewer %s")
                   ("\\.azw3\\'" . "ebook-viewer %s")
                   ;; Image
                   ;; ("\\.png\\'" . "sxiv %s")
                   ;; ("\\.jp\(e\)?g" . "sxiv %s")
                   ("\\.gif\\'" . "sxiv -a -f %s")
                   ;; ("\\.gif\\'" . "gwenview %s")
                   ;; ("\\.gif\\'" . my/open-and-play-gif-image)
                   ;; ("\\.svg\\'" . "feh --magick-timeout 5 %s")
                   ;; ("\\.svg\\'" . "display %s") ; Emacs built-in support display svg
                   ;; Mind Maps
                   ("\\.mm\\'" . "freeplane %s")
                   ;; Office
                   ;; Open Text Document
                   ("\\.odt\\'"  . "libreoffice %s")  ; Text Documentation
                   ("\\.ods\\'"  . "libreoffice %s")  ; Spreadsheet
                   ("\\.odp\\'"  . "libreoffice %s")  ; Presentation
                   ("\\.odf\\'"  . "libreoffice %s")  ; Database / Formula
                   ;; Windows Office
                   ("\\.doc\\'"  . "libreoffice %s")
                   ("\\.ppt\\'"  . "libreoffice %s")
                   ("\\.xls\\'"  . "libreoffice %s")
                   ("\\.docx\\'" . "libreoffice %s")
                   ("\\.pptx\\'" . "libreoffice %s")
                   ("\\.xlsx\\'" . "libreoffice %s")
                   ;; Video
                   ("\\.mp4\\'"  . "mpv --slang=zho %s")
                   ("\\.mkv\\'"  . "mpv --slang=zho %s")
                   ("\\.mov\\'"  . "mpv --slang=zho %s")
                   ("\\.ogv\\'"  . "mpv --slang=zho %s")
                   ("\\.webm\\'" . "mpv --slang=zho %s")
                   ("\\.flv\\'"  . "mpv --slang=zho %s")
                   ("\\.f4v\\'"  . "mpv --slang=zho %s")
                   ("\\.rmvb\\'" . "mpv --slang=zho %s")
                   ;; Audio
                   ("\\.mp3\\'"  . "mpv %s")
                   ("\\.ogg\\'"  . "mpv %s")
                   ("\\.wav\\'"  . "mpv %s")
                   ("\\.m4a\\'"  . "mpv %s")
                   ("\\.opus\\'"  . "mpv %s")
                   ("\\.midi\\'" . "timidity %s")))
     (add-to-list 'org-file-apps pair))
   
   ;; (add-to-list 'org-file-apps '("\\.swf\\'" . "gnash %s"))
   )

  ('darwin
   )
  ('windows-nt
   ))

(add-to-list 'org-file-apps '("\\.jar\\'" . "java -jar %s"))

;; System wise: xdg-open, kde-open, gnome-open.
;; (when (boundp 'org-file-apps-gnu)
;;   (setcdr (assq 'system org-file-apps-gnu) "xdg-open %s"))

;;; Open .pdf, .epub file link with EAF.
(defun eaf-open-for-org (file &optional link)
  "An wrapper function on `eaf-open'."
  (eaf-open file))
(with-eval-after-load 'org
  ;; documents
  (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.epub\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.djvu\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.xps\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.oxps\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.cbz\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.fb2\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.fbz\\'" . eaf-open-for-org))
  ;; images
  (add-to-list 'org-file-apps '("\\.gif\\'" . eaf-open-for-org))
  ;; videos
  (add-to-list 'org-file-apps '("\\.avi\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.mp4\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.mkv\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.mov\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.ogv\\'" . eaf-open-for-org))
  (add-to-list 'org-file-apps '("\\.webm\\'" . eaf-open-for-org)))

;;; Open video file links by selecting video player interactively.
(defun my/org-open-video-file (file &optional link)
  "A wrapper function to open video file link with an interactive selection of video players."
  (let ((player (completing-read "Video Player: "
                                 (list "mpv"
                                       "EAF"
                                       "SMPlayer"
                                       "MPlayer")))
        (name (file-name-base file)))
    (pcase player
      ("mpv" (start-process
              (format "mpv %s" name)
              (format "*mpv %s*" name)
              "mpv"
              "--slang=zho" file))
      ("EAF" (eaf-open file))
      ("SMPlayer" (start-process
                   (format "smplayer %s" name)
                   (format "*smplayer %s*" name)
                   "smplayer"
                   file))
      ("MPlayer" (start-process
                  (format "mplayer %s" name)
                  (format "*mplayer %s*" name)
                  "mplayer"
                  "-slang=zh" file)))))
(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.avi\\'" . my/org-open-video-file))
  (add-to-list 'org-file-apps '("\\.mp4\\'" . my/org-open-video-file))
  (add-to-list 'org-file-apps '("\\.mkv\\'" . my/org-open-video-file))
  (add-to-list 'org-file-apps '("\\.mov\\'" . my/org-open-video-file))
  (add-to-list 'org-file-apps '("\\.ogv\\'" . my/org-open-video-file))
  (add-to-list 'org-file-apps '("\\.webm\\'" . my/org-open-video-file)))

;;; `eshell:' org-link `eshell:' support for EShell
(use-package ol-eshell)

;; `elisp:'
(setq org-confirm-elisp-link-function 'yes-or-no-p)
;; `shell:'
(setq org-confirm-shell-link-function 'yes-or-no-p)

;; `irc:'
(use-package ol-irc
  :custom (org-irc-client 'erc)
  :config (if (and (featurep 'erc)) (require 'init-erc)))

;;; `info:' link.
(use-package org-info)

;; `man:'
;; [[man:(section: 7 or 3r)gv][gv (man page)]]
(use-package ol-man
  :after org
  :custom (org-man-command 'man))

;; `occur:'
;; [[occur:my-file.txt#regex]] :: to open a file and run occur with the regex on it.
(defun org-occur-link-open (uri)
  "Visit the file specified by `URI', and run `occur' on the fragment.
  \(anything after the first '#') in the `URI'."
  (let ((list (split-string uri "#")))
    (org-open-file (car list) t)
    (occur (mapconcat 'identity (cdr list) "#"))))
(org-link-set-parameters "occur" :follow #'org-occur-link-open)

;;; `grep:'
;;  [[grep:regexp][regexp (grep)]]
(defun org-grep-link-open (regexp)
  "Run `rgrep' with `REGEXP' as argument."
  (grep-compute-defaults)
  (rgrep regexp "*" (expand-file-name "./")))
(org-link-set-parameters "grep" :follow #'org-grep-link-open)

;; `git:'
(use-package ol-git-link
  :init
  ;; add file path completion support for `git:' and `gitbare:'
  (org-link-set-parameters "git" :complete 'org-git-complete-link)
  ;; TODO: add a function to complete git: link. parse git repo metadata, show in available candidates.
  (defun org-git-complete-link ()
    "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it."
    (concat (replace-regexp-in-string "^file:" "git:" (org-file-complete-link))
            "::"
            (read-from-minibuffer "branch:" "1")
            "@"
            (read-from-minibuffer "date:" "{2017-06-24}")
            "::"
            (read-from-minibuffer "line:" "1"))))

;;; [ orgit ] -- support for Org links to Magit buffers.
;;; `orgit:'
;;; `orgit-log:'
;;; `orgit-rev:'
(use-package orgit
  :ensure t
  :custom (orgit-log-save-arguments t))

;;; [ orgit-forge ] -- Org links to Magit Forge issue buffers.
;;; `orgit-topic:'
(use-package orgit-forge
  :ensure t)


;;; [ Link abbreviations ]

;; NOTE: you can NOT contain chinese string in "link name". Org-mode does not
;; support it.

;; [C-c C-l] insert link completion.
(setq org-link-abbrev-alist
      '(("RFC" . "https://datatracker.ietf.org/doc/rfc%s")
        ;; ("RFC" . "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s")
        ;; search engines
        ("Google" . "http://www.google.com/search?q=%s")
        ("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
        ("Blekko" . "https://blekko.com/#?q=%s")
        ("Bing" . "http://cn.bing.com/search?q=")
        ("Baidu" . "http://www.baidu.com/s?wd=%s")
        ;; Wiki
        ("Wikipedia" . "http://en.wikipedia.org/w/index.php?search=%s")
        ("Wikia" . "http://www.wikia.com/index.php?search=%s")
        ("Baidu_BaiKe" . "http://baike.baidu.com/search/none?word=%s")
        ;; Q & A
        ("Quora" . "https://www.quora.com/search?q=%s")
        ("ZhiHu" . "http://www.zhihu.com/search?q=%s&type=question")
        ("Baidu_ZhiDao" . "http://zhidao.baidu.com/search?word=%s")
        ("Baidu_JingYan" . "http://jingyan.baidu.com/search?word=%s")
        ;; ISBN
        ;; ("ISBN" . "http://isbndb.com/search/all?query=%s")
        ("ISBN" . "http://www.openisbn.com/search.php?q=%s&isbn=1")
        ;; Maps
        ("Baidu_Maps" . "http://map.baidu.com/?q=%s")
        ("Google_Maps" . "http://maps.google.com/maps?q=%s")
        ("OpenStreetMap" . "https://www.openstreetmap.org/search?query=%s")
        ;; Social Networks
        ("Twitter" . "https://twitter.com/%s")
        ("Facebook" . "https://www.facebook.com/%s")
        ;; Programming
        ("Stack_Overflow" . "http://stackoverflow.com/search?q=%s")
        ("S.E_Programmers" . "http://programmers.stackexchange.com/search?q=%s")
        ;; Emacs
        ("Emacs_Wiki" . "www.emacswiki.org/emacs?search=%s")
        ("S.E_Emacs" . "http://emacs.stackexchange.com/search?q=%s")
        ;; API Search
        ("Mozilla_Developer" . "https://developer.mozilla.org/en-US/search?q=%s")
        ("{API}Search_apis.io" . "http://apis.io/?search=%s")
        ;; Code Search
        ("search_code" . "http://searchcode.com/?q=%s")
        ("GitHub" . "https://github.com/search?q=%s")
        ("Bitbucket" . "https://bitbucket.org/repo/all?name=%s")
        ("Launchpad" . "https://launchpad.net/+search?field.text=%s")
        ("Code_Project" . "http://www.codeproject.com/search.aspx?q=%s")
        ("CodePlex" . "https://www.codeplex.com/site/search?query=%s")
        ("Gitorious" . "https://gitorious.org/search?q=%s")
        ("SourceForge" . "https://sourceforge.net/directory/?q=%s")
        ("Freecode" . "http://freecode.com/search?q=%s")
        ("Active_State" . "http://code.activestate.com/search/#q=%s")
        ("Ohloh_Code" . "http://code.ohloh.net/search?s=%s")
        ("Snipplr" . "http://snipplr.com/search.php?q=%s")
        ;; chinese code search
        ("Coding" . "https://coding.net/search?q=%s")
        ("Geakit" . "https://geakit.com/search?q=%s")
        ("Git_OSC_Open_Source_China" . "https://git.oschina.net/search?search=%s")
        ;; Bug Track System
        ("CVE" . "https://cve.mitre.org/cgi-bin/cvename.cgi?name=%s")
        ;; Lisp
        ("lispdoc" . "http://lispdoc.com/?q=%s")
        ;; Clojure
        ;; Java
        ("Maven" . "https://search.maven.org/search?q=%s")
        ("Java JSR" . "https://jcp.org/en/jsr/detail?id=%s") ; Java Specification Requests
        ("Java JEP" . "https://openjdk.java.net/jeps/%s") ; JEP
        ;; Python
        ("Python_3_Documentation" . "http://docs.python.org/3/search.html?q=%s")
        ;; Ruby
        ("Ruby-Doc" . "http://ruby-doc.com/search.html?q=%s")
        ;; Perl
        ("Perl_CPAN" . "http://search.cpan.org/search?mode=all&query=%s")
        ;; PHP
        ("PHP_online_documentation" . "http://cn2.php.net/results.php?q=%s&p=manual")
        ;; JavaScript
        ("JavaScript_Mozilla" . "https://developer.mozilla.org/en-US/search?q=%s")
        ;; HTML
        ;; CSS
        ;; Book
        ("DouBan_Books" . "http://book.douban.com/subject_search?search_text=%s")
        ;; Movie
        ("IMDb" . "http://www.imdb.com/title/%s")
        ("DouBan_Movies" . "http://movie.douban.com/subject_search?search_text=%s")
        ))


;;; open image link to edit

(setq org-image-link-edit-cmd "gimp")

(defun org-image-link-edit ()
  "Open the image at point for editing."
  (interactive)
  (let* ((context (org-element-context))
         (type (plist-get (cadr context) :type))
         (path (plist-get (cadr context) :path)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (async-start-process
       "org-download-edit"
       org-image-link-edit-cmd
       (lambda (p)
         (message (format "%s" p)))
       (pcase type
         ("file" (org-link-unescape path))
         ("attachment" (org-attach-expand path)))))))

(define-key Org-prefix (kbd "E") 'org-image-link-edit)


;;; [ org-quick-peek ] -- Quick inline peeks at agenda items and linked nodes in Org-mode.

;; (use-package org-quick-peek
;;   :quelpa (org-quick-peek :fetcher github :repo "alphapapa/org-quick-peek"))

;;; [ org-kindle ] -- Make Emacs bridge between Org Mode and Kindle.

(use-package org-kindle
  :ensure t
  :defer t
  :commands (org-kindle-send-to-device org-kindle-sync-notes)
  :init (add-to-list 'display-buffer-alist '("^\\*org-kindle:.*\\*" . (display-buffer-below-selected))))

;;; [ org-screen ] -- Integrate Org Mode with screen.

(use-package org-screen
  :commands (org-screen))

;;; [ org-link-beautify ] -- beautify org links with intuitive icons.

(use-package org-link-beautify
  :ensure t
  :commands (org-link-beautify-mode)
  :init (org-link-beautify-mode 1))



(provide 'init-org-hyperlink)

;;; init-org-hyperlink.el ends here
