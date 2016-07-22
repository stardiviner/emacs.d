;;; init-my-org-hyperlink.el --- init for Org Hyperlinks
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-display-internal-link-with-indirect-buffer t
      org-indirect-buffer-display 'current-window
      ;; org-open-directory-means-index-dot-org t
      
      ;; You can fully-qualify links on a link-by-link basis by passing one
      ;; universal argument [C-u].
      org-link-file-path-type 'adaptive ; default 'adaptive, 'relative
      )

(setq org-file-apps
      '(;; default
        ;; (auto-mode . emacs)
        ;; Web Pages
        ("\.x?html?\\'" . default)
        ("\(?:xhtml\|html\)" . "firefox %s")
        ;; PDF
        ("\\.pdf\\'" . auto-mode)
        ("\\.pdf::\\([[:digit:]]+\\)\\'" . auto-mode)
        ;; disable this, to use `doc-view' from `pdf-tools' for PDF.
        ;; ("\\.pdf\\'" . "okular %s")
        ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . "okular -p %1 %s")
        ;; CHM
        ("\\.chm\\'" . "kchmviewer %s")
        ;; EPUB
        ("\\.epub\\'" . "ebook-viewer %s")
        ;; AZW3
        ("\\.azw3\\'" . "ebook-viewer %s")
        ;; mobi
        ("\\.mobi\\'" . "ebook-viewer %s")
        ;; Image
        ("\\.png\\'" . "sxiv %s")
        ("\\.jpg\\'" . "sxiv %s")
        ;; ("\\.gif\\'" . "sxiv -a -f %s")
        ("\\.gif\\'" . "gwenview %s")
        ("\\.svg\\'" . "feh --magick-timeout 5 %s")
        ;; Mind Maps
        ("\\.mm\\'" . "freemind %s") ; freeplane
        ;; Office
        ;; Open Text Document
        ("\\.odt\\'" . "libreoffice %s") ; Text Documentation
        ("\\.ods\\'" . "libreoffice %s") ; Spreadsheet
        ("\\.odp\\'" . "libreoffice %s") ; Presentation
        ("\\.odf\\'" . "libreoffice %s") ; Database / Formula
        ;; Video
        ;; ("\\mp4\\'" . "mpv %s")
        ;; ("\\mov\\'" . "mpv %s")
        ))

(add-to-list 'org-file-apps '("\\.swf\\'" . "swfdec-player %s"))
(add-to-list 'org-file-apps '("\\.jar\\'" . "java -jar %s"))

;; System wise: xdg-open, kde-open, gnome-open.
(setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s")


;; `org-link-types'
;; `org-add-link-type' + `org-add-link-props'

;; `shell:'
(setq org-confirm-shell-link-function 'yes-or-no-p)

;; Email: `mailto:' link open with Emacs internal extension like message-mode, mu4e.
;; `mail-user-agent'
(setq browse-url-mailto-function 'browse-url-mail)

;; IRC
(if (featurep 'erc)
    (setq org-irc-client 'erc)
  (if (featurep 'circe)
      (setq org-irc-client 'circe)))

;;; telnet: link type
;;  telnet://ptt.cc
(org-add-link-type "telnet" 'telnet)

;; RSS
(defun org-rss-link-open (uri)
  "Open rss:// URI link."
  (eww uri))

(org-add-link-type "rss" 'org-rss-link-open)

;; append "`man:'" protocol.
;; `[[man:printf][The printf manpage]]'
(require 'org-man)
(setq org-man-command 'man) ; 'man, 'woman.

;;; occur: link type
;;
;; and you can then use links like:
;;   occur:my-file.txt#regex
;; to open a file and run occur with the regex on it.
(defun org-occur-link-open (uri)
  "Visit the file specified by URI, and run `occur' on the fragment
  \(anything after the first '#') in the uri."
  (let ((list (split-string uri "#")))
    (org-open-file (car list) t)
    (occur (mapconcat 'identity (cdr list) "#"))))

(org-add-link-type "occur" 'org-occur-link-open)

;;; [[grep:regexp][regexp (grep)]]
(defun org-grep-link-open (regexp)
  "Run `rgrep' with REGEXP as argument."
  (grep-compute-defaults)
  (rgrep regexp "*" (expand-file-name "./")))

(org-add-link-type "grep" 'org-grep-link-open)

;;; [[tag:]]
;; e.g. [[tag:work+phonenumber-boss][Optional Description]]
(defun org-tag-link-open (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(org-add-link-type "tag" 'org-tag-link-open)

;; [[wiki:]]
;; for open wiki search query with local application database.
(defvar kiwix-browser "google-chrome-stable")

(defun org-wiki-link-open (query)
  "Search `QUERY' with Kiwix."
  (let* ((browser kiwix-browser)
         (kiwix-server "http://127.0.0.1:8000/")
         (kiwix-library "wikipedia_zh_all_2015-11")
         (url (concat kiwix-server kiwix-library "/A/" query ".html")))
    (shell-command (concat browser " " url))))

(org-add-link-type "wiki" 'org-wiki-link-open)


(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))


;;; [ Link abbreviations ]

;; NOTE: you can not contain chinese string in "link name". Org-mode does not
;; support it.

;; [C-c C-l] insert link completion.
(setq org-link-abbrev-alist
      '(
        ("RFC" . "https://datatracker.ietf.org/doc/rfc%s")
        ;; ("RFC" . "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s")
        ;; search engines
        ("Google" . "http://www.google.com/search?q=%s")
        ("google" . "http://www.google.com/search?q=%s")
        ("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
        ("blekko" . "https://blekko.com/#?q=%s")
        ("Bing" . "http://cn.bing.com/search?q=")
        ("Baidu" . "http://www.baidu.com/s?wd=%s")
        ;; Wiki
        ("Wikipedia" . "http://en.wikipedia.org/w/index.php?search=%s")
        ("Wikia" . "http://www.wikia.com/index.php?search=%s")
        ("Baidu_BaiKe" . "http://baike.baidu.com/search/none?word=%s")
        ("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html")
        ;; Q & A
        ("Quora" . "https://www.quora.com/search?q=%s")
        ("ZhiHu" . "http://www.zhihu.com/search?q=%s&type=question")
        ("Baidu_ZhiDao" . "http://zhidao.baidu.com/search?word=%s")
        ("Baidu_JingYan" . "http://jingyan.baidu.com/search?word=%s")
        ;; Maps
        ("Baidu_Maps" . "http://map.baidu.com/?q=%s")
        ("Google_Maps" . "http://maps.google.com/maps?q=%s")
        ;; Social Networkings
        ("Twitter" . "https://twitter.com/%s")
        ("Facebook" . "https://www.facebook.com/%s")
        ;; Programming
        ("Stack_Overflow" . "http://stackoverflow.com/search?q=%s")
        ("S.E_Programmers" . "http://programmers.stackexchange.com/search?q=%s")
        ;; Emacs
        ("Emacs_Wiki" . "www.emacswiki.org/emacs?search=%s")
        ("S.E_Emacs" . "http://emacs.stackexchange.com/search?q=%s")
        ;; Document Search
        ("Mozilla_Developer" . "https://developer.mozilla.org/en-US/search?q=%s")
        ;; API Search
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
        ("GitCafe" . "https://gitcafe.com/search?keyword=%s")
        ("Coding" . "https://coding.net/search?q=%s")
        ("Geakit" . "https://geakit.com/search?q=%s")
        ("Git_OSC_Open_Source_China" . "https://git.oschina.net/search?search=%s")
        ;; Lisp
        ("lispdoc" . "http://lispdoc.com/?q=%s")
        ;; Ruby
        ("Ruby-Doc" . "http://ruby-doc.com/search.html?q=%s")
        ;; Python
        ("Python_3_Documentation" . "http://docs.python.org/3/search.html?q=%s")
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


;;;_* orgit -- Support for Org links to Magit buffers.

;; (use-package orgit
;;   :ensure t
;;   :config
;;   (setq orgit-remote "origin")
;;   ;; Default: github, gitlab, bitbucket, orgmode.org, git.kernel.org
;;   ;;
;;   ;; (add-to-list orgit-export-alist ')
;;   )


;;; open image link to edit

(defvar org-image-link-edit-cmd "gimp %s")

(defun org-image-link-edit ()
  "Open the image at point for editing."
  (interactive)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "not on a link")
      (start-process-shell-command
       "org-download-edit"
       "org-download-edit"
       (format org-image-link-edit-cmd (plist-get (cadr context) :path))))))

(define-key my-org-prefix (kbd "E") 'org-image-link-edit)


(provide 'init-my-org-hyperlink)

;;; init-my-org-hyperlink.el ends here
