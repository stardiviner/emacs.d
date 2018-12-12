;;; init-org-hyperlink.el --- init for Org Hyperlinks
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Check out variable `org-link-parameters'.

(setq org-indirect-buffer-display 'current-window
      ;; org-display-internal-link-with-indirect-buffer t ; [C-u] to open in indirect buffer window.
      org-keep-stored-link-after-insertion t ; keep stored link in entire session.
      ;; You can fully-qualify links on a link-by-link basis by passing one
      ;; universal argument [C-u].
      org-link-file-path-type 'adaptive ; default 'adaptive, 'relative
      )

(define-key org-mode-map (kbd "M-,") 'org-mark-ring-goto)

;;; use :ID: property for org linking.
;; (require 'org-id)
;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
;;       org-id-track-globally t
;;       org-id-locations-file (convert-standard-filename
;; 				                     (concat user-emacs-directory ".org-id-locations")))

;;; use :CUSTOM_ID: property for org headlines linking.
(defun org-store-link-set-headline-custom-id (arg &optional interactive?)
  "Set property :CUSTOM_ID: for `org-store-link' on headline."
  (when (and (equal major-mode 'org-mode) ; handle case `org-store-link' not in org-mode file.
             (not (org-before-first-heading-p)) ; handle case point is in org-mode buffer ahead of first headline.
             ;; (org-on-heading-p t) ; detect whether on a headline
             ;; (re-search-backward (concat "^\\(?:" outline-regexp "\\)") nil t) ; detect whether under a headline?
             (not (region-active-p)) ; handle `org-drill' capture word case.
             (not (org-entry-get nil "CUSTOM_ID")))
    (if (yes-or-no-p "Set property :CUSTOM_ID: ? ")
        (org-set-property
         "CUSTOM_ID"
         (read-from-minibuffer "Property :CUSTOM_ID: value: "
                               (substring-no-properties (org-get-heading t t)))))))

;; (advice-add 'org-store-link :before #'org-store-link-set-headline-custom-id)

(add-hook 'org-follow-link-hook #'sound-tick)

;;; org-file-apps no longer accepts S-expressions as commands
;;
;; The variable now accepts functions of two arguments instead of plain
;; S-expressions. Replacing a S-expresion with an appropriate function is
;; straightforward. For example:
;;
;; ("pdf" . (foo))
;; becomes:
;; ("pdf" . (lambda (file link) (foo)))

;;; Open and play GIF image in Emacs buffer.
(defun my-func/open-and-play-gif-image (file &optional link)
  "Open and play GIF image `FILE' in Emacs buffer.
Optional for Org-mode file: `LINK'."
  (let ((gif-image (create-image file))
        (tmp-buf (get-buffer-create "*Org-mode GIF image animation*")))
    (switch-to-buffer tmp-buf)
    (erase-buffer)
    (insert-image gif-image)
    (image-animate gif-image nil t)
    (local-set-key (kbd "q") 'kill-current-buffer)
    ))

(setq org-file-apps
      `(;; Web Pages
        ,(if (executable-find "firefox")
             `("\.x?html?\\'" . "firefox %s")
           `("\.x?html?\\'" . "google-chrome-unstable %s"))
        ;; PDF (disable this because I use package `org-pdfview'.
        ;; ("\\.pdf\\'" . auto-mode)
        ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . auto-mode)
        ;; disable this, to use `doc-view' from `pdf-tools' for PDF.
        ;; ("\\.pdf\\'" . "okular %s")
        ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . "okular -p %1 %s")
        ;; CHM
        ("\\.chm\\'" . "kchmviewer %s")
        ;; EBooks
        ;; ("\\.epub\\'" . "okular %s") ; it is opened by `ereader', and `nov'.
        ("\\.mobi\\'" . "ebook-viewer %s")
        ("\\.azw3\\'" . "ebook-viewer %s")
        ;; Image
        ;; ("\\.png\\'" . "sxiv %s")
        ;; ("\\.jp\(e\)?g" . "sxiv %s")
        ;; ("\\.jpg\\'" . "sxiv %s")
        ;; ("\\.jpeg\\'" . "sxiv %s")
        ("\\.gif\\'" . "sxiv -a -f %s") ; "sxiv -a -f -- %s
        ;; ("\\.gif\\'" . "gwenview %s")
        ;; ("\\.gif\\'" . my-func/open-and-play-gif-image)
        ;; ("\\.svg\\'" . "feh --magick-timeout 5 %s")
        ;; ("\\.svg\\'" . "display %s") ; Emacs built-in support display svg
        ;; Mind Maps
        ("\\.mm\\'" . "freeplane %s")
        ;; Office
        ;; Open Text Document
        ("\\.odt\\'" . "libreoffice %s") ; Text Documentation
        ("\\.ods\\'" . "libreoffice %s") ; Spreadsheet
        ("\\.odp\\'" . "libreoffice %s") ; Presentation
        ("\\.odf\\'" . "libreoffice %s") ; Database / Formula
        ;; Windows Office
        ("\\.docx\\'" . "libreoffice %s")
        ("\\.pptx\\'" . "libreoffice %s")
        ("\\.xlsx\\'" . "libreoffice %s")
        ;; Video
        ("\\.mp4\\'" . "mplayer %s")
        ("\\.mov\\'" . "mplayer %s")
        ("\\.ogv\\'" . "mplayer %s")
        ("\\.webm\\'" . "mplayer %s")
        ("\\.flv\\'" . "mplayer %s")
        ("\\.f4v\\'" . "mplayer %s")
        ("\\.mkv\\'" . "mplayer %s")
        ;; Audio
        ("\\.mp3\\'" . "play %s")
        ("\\.ogg\\'" . "mplayer %s")
        ("\\.wav\\'" . "mplayer %s")
        ("\\.midi\\'" . "timidity %s")
        (auto-mode . emacs)
        (t . emacs)))

(add-to-list 'org-file-apps '("\\.swf\\'" . "gnash %s"))
(add-to-list 'org-file-apps '("\\.jar\\'" . "java -jar %s"))

;; System wise: xdg-open, kde-open, gnome-open.
(setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s")


;;; Links are now customizable
;;
;; Links can now have custom colors, tooltips, keymaps, display behavior, etc.
;; Links are now centralized in `org-link-parameters'.
;; (add-to-list 'org-link-parameters '())
;; `org-link-types'
;; `org-link-set-parameters'

;;; fontify broken link.
(org-link-set-parameters
 "file"
 :face (lambda (path)
         (if (file-exists-p (expand-file-name (org-link-unescape path)))
             'org-link 'org-warning)))

(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))

;;; `eshell:'
;; (require 'org-eshell)

;; `elisp:'
(setq org-confirm-elisp-link-function 'yes-or-no-p)
;; `shell:'
(setq org-confirm-shell-link-function 'yes-or-no-p)

;; Email: `mailto:' link open with Emacs internal extension like message-mode, mu4e.
;; `mail-user-agent'
;; (setq browse-url-mailto-function 'browse-url-mail)


;; IRC: `irc:'
(require 'org-irc)
;;; IRC clients: 'erc, 'rcirc, 'circe,
(setq org-irc-client 'erc)
(if (and (featurep 'erc) (featurep 'init-erc))
    (require 'init-erc))


;;; Telnet:
;;  telnet://ptt.cc
(org-link-set-parameters "telnet"
                         :follow #'telnet)


;; RSS
(defun org-rss-link-open (uri)
  "Open rss:// URI link."
  (eww uri))

(org-link-set-parameters "rss"
                         :follow #'org-rss-link-open)

;;; `info:' link.
(require 'org-info)

;; append "`man:'" protocol.
;; `[[man:(section: 7 or 3r)gv][gv (man page)]]'
(require 'org-man)
(setq org-man-command 'man) ; 'man, 'woman.

;;; `occur:'
;;   occur:my-file.txt#regex
;; to open a file and run occur with the regex on it.
(defun org-occur-link-open (uri)
  "Visit the file specified by `URI', and run `occur' on the fragment.
  \(anything after the first '#') in the `URI'."
  (let ((list (split-string uri "#")))
    (org-open-file (car list) t)
    (occur (mapconcat 'identity (cdr list) "#"))))

(org-link-set-parameters "occur"
                         :follow #'org-occur-link-open)

;;; `grep:'
;;  [[grep:regexp][regexp (grep)]]
(defun org-grep-link-open (regexp)
  "Run `rgrep' with `REGEXP' as argument."
  (grep-compute-defaults)
  (rgrep regexp "*" (expand-file-name "./")))

(org-link-set-parameters "grep" #'org-grep-link-open)


;;; `tag:'
;; e.g. [[tag:work+phonenumber-boss][Optional Description]]
(defun org-tag-link-open (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(org-link-set-parameters "tag"
                         :follow #'org-tag-link-open)

;;; [ Git ]
(require 'org-git-link)

;;; add file path completion support for `git:' and `gitbare:'
(org-link-set-parameters "git"
                         :complete 'org-git-complete-link)

;;; TODO: add a function to complete git: link. parse git repo metadata, show in available candidates.
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
          (read-from-minibuffer "line:" "1")))

;;; [ orgit ] -- support for Org links to Magit buffers.

(use-package orgit
  :ensure t
  :init
  (setq orgit-log-save-arguments t)
  ;; (add-to-list 'orgit-export-alist '())
  )

;;; `track:' for OSM Maps
;; [[track:((9.707032442092896%2052.37033874553582)(9.711474180221558%2052.375238282987))data/images/org-osm-link.svg][Open this link will generate svg, png image for track link on map]]
(use-package org-osm-link
  :quelpa (org-osm-link :fetcher github :repo "emacsattic/org-osm-link")
  :init (setq osm-do-cache t))

;;; `geo:'
;; [geo:37.786971,-122.399677;u=35]
(defcustom org-geo-link-application-command "gnome-maps"
  "Specify the program name for openning geo: link."
  :type 'string)

(defun org-geo-link-open (uri)
  "Open Geography location `URI' like \"geo:25.5889136,100.2208514\" in Map application."
  (if (executable-find "gnome-maps")
      (start-process
       "org-geo-link-open"
       "*org-geo-link-open*"
       org-geo-link-application-command
       (shell-quote-wildcard-pattern uri))
    (browse-url uri))
  )

(org-link-set-parameters "geo" :follow #'org-geo-link-open)


;;; [[video:/path/to/file.mp4::00:13:20]]
(defcustom org-video-link-open-command "mplayer"
  "Specify the program for openning video: link."
  :type 'string)

(defun org-video-link-open (uri)
  "Open video file `URI' with video player."
  (let* ((list (split-string uri "::"))
         (path (car list))
         (start-timstamp (cadr list)))
    (make-process
     :command (list org-video-link-open-command
                    "-ss" start-timstamp
                    (expand-file-name (org-link-unescape path)))
     :name "org-video-link")))

(org-link-set-parameters "video"
                         :follow #'org-video-link-open
                         :complete #'org-file-complete-link)

;;; [ Link abbreviations ]

;; NOTE: you can not contain chinese string in "link name". Org-mode does not
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
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (async-start-process
       "org-download-edit"
       org-image-link-edit-cmd
       (lambda (p)
         (message (format "%s" p)))
       (org-link-unescape (plist-get (cadr context) :path))))))

(define-key Org-prefix (kbd "E") 'org-image-link-edit)


;;; [ org-quick-peek ] -- Quick inline peeks at agenda items and linked nodes in Org-mode.

;; (use-package org-quick-peek
;;   :quelpa ((org-quick-peek :fetcher github :repo "alphapapa/org-quick-peek") :upgrade nil)
;;   )

;;; [ org-send-ebook ] -- Send org link file to ebook reader.

(use-package org-send-ebook
  :ensure t
  :defer t
  :commands (org-send-ebook)
  :bind (:map Org-prefix ("M-e" . org-send-ebook)))



(provide 'init-org-hyperlink)

;;; init-org-hyperlink.el ends here
