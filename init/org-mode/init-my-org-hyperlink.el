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

(define-key org-mode-map (kbd "M-,") 'org-mark-ring-goto)
;; (define-key org-mode-map (kbd "C-c %") 'org-mark-ring-push)

(setq org-file-apps
      '(;; default
        ;; (auto-mode . emacs)
        ;; Web Pages
        ("\.x?html?\\'" . default)
        ("\(?:xhtml\|html\)" . "firefox %s")
        ;; PDF (disable this because I use package `org-pdfview'.
        ;; ("\\.pdf\\'" . auto-mode)
        ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . auto-mode)
        ;; disable this, to use `doc-view' from `pdf-tools' for PDF.
        ;; ("\\.pdf\\'" . "okular %s")
        ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . "okular -p %1 %s")
        ;; CHM
        ("\\.chm\\'" . "kchmviewer %s")
        ;; EPUB
        ;; ("\\.epub\\'" . "ebook-viewer %s")
        ("\\.epub\\'" . "okular %s") ; use `okular' can store reading progress position.
        ("\\.mobi\\'" . "okular %s")
        ("\\.azw3\\'" . "okular %s")
        ;; Image
        ("\\.png\\'" . "sxiv %s")
        ("\\.jpg\\'" . "sxiv %s")
        ("\\.gif\\'" . "sxiv -a -f %s") ; "sxiv -a -f -- %s
        ;; ("\\.gif\\'" . "gwenview %s")
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
        ;; ("\\.mp4\\'" . "mpv %s")
        ;; ("\\.mov\\'" . "mpv %s")
        ;; ("\\.ogv\\'" . "mpv %s")
        ;; Audio
        ("\\.mp3\\'" . "mpg123 %s")
        ))

(add-to-list 'org-file-apps '("\\.swf\\'" . "swfdec-player %s"))
(add-to-list 'org-file-apps '("\\.jar\\'" . "java -jar %s"))

;; System wise: xdg-open, kde-open, gnome-open.
(setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s")


;; `org-link-types'
;; `org-link-set-parameters' `org-add-link-type' (deprecated) + `org-add-link-props'
;; (org-link-set-parameters "ebib"
;;                          :follow #'org-ebib-open
;;                          :store #'org-ebib-store-link)


;; `shell:'
(setq org-confirm-shell-link-function 'yes-or-no-p)


;; Email: `mailto:' link open with Emacs internal extension like message-mode, mu4e.
;; `mail-user-agent'
(setq browse-url-mailto-function 'browse-url-mail)


;; IRC: `irc:'
(if (featurep 'erc)
    (setq org-irc-client 'erc)
  (if (featurep 'circe)
      (setq org-irc-client 'circe)))


;;; telnet: link type
;;  telnet://ptt.cc
(org-link-set-parameters "telnet"
                         :follow #'telnet)


;; RSS
(defun org-rss-link-open (uri)
  "Open rss:// URI link."
  (eww uri))

(org-link-set-parameters "rss"
                         :follow #'org-rss-link-open)


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

(org-link-set-parameters "occur"
                         :follow #'org-occur-link-open)

;;; [[grep:regexp][regexp (grep)]]
(defun org-grep-link-open (regexp)
  "Run `rgrep' with REGEXP as argument."
  (grep-compute-defaults)
  (rgrep regexp "*" (expand-file-name "./")))

(org-link-set-parameters "grep"
                         #'org-grep-link-open)


;;; [[tag:]]
;; e.g. [[tag:work+phonenumber-boss][Optional Description]]
(defun org-tag-link-open (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(org-link-set-parameters "tag"
                         :follow #'org-tag-link-open)


;;; [[map:"address name/geography"]]
;; - address name: "Dali, Yunnan, China"
(defcustom org-map-application-command "gnome-maps"
  "Specify the program name for openning map: link.")

(defcustom org-map-application-options ""
  "Specify the program options for openning map: link.")

(defcustom org-map-services-list
  '(("Google Maps" . "http://maps.google.com/maps?q=")
    ("OpenStreetMap" . "https://www.openstreetmap.org/search?query=")
    ("Baidu Maps" . "http://map.baidu.com/?q="))
  "Specify a list of Maps services for using when querying address.")

(defun org-map-get-service-url (service-name)
  "Get Maps `SERVICE_NAME' URL through associate list."
  (cdr (assoc service-name org-map-services-list)))

(defun org-map-select-service ()
  "Interactively select Maps service."
  (completing-read "Maps service: "
                   (map-keys org-map-services-list)))

(defcustom org-map-prefer-service t
  "Prefer use Maps service because they are instantly.")

(defvar org-map-application-p (if (executable-find "gnome-maps") t nil)
  "Return boolean value after check Maps application available.")

(defun org-map-link-open (address)
  "Search `ADDRESS' in Map application."
  (if org-map-prefer-service
      ;; open with Maps service
      (let* ((map-service (org-map-get-service-url (org-map-select-service))
                          ;; detect address string is chinese string.
                          ;; TODO: don't why `multibyte-string-p' detect link `address' like "Dali, Yunnan, China" will become `t'.
                          ;; (if (multibyte-string-p address)
                          ;;     (org-map-get-service-url "Baidu Maps")
                          ;;   (org-map-get-service-url (org-map-select-service)))
                          )
             (url (concat map-service (url-encode-url address))))
        (prin1 url)
        (browse-url url)

        ;; debug upper if condition
        ;; (prin1 map-service)
        ;; (prin1 (multibyte-string-p address))
        )
    ;; open with Maps application
    (when org-map-application-p
      (start-process-shell-command
       "org-map-link-open"
       "org-map-link-open"
       (format "%s %s %s"
               org-map-application-command
               org-map-application-options
               (shell-quote-wildcard-pattern address))))))

(org-link-set-parameters "map"
                         :follow #'org-map-link-open)



;;; [ geography link ]
;; [geo:37.786971,-122.399677;u=35]

(defun org-geo-link-open ()
  "Open geography location link with program."
  
  )

(org-link-set-parameters "geo"
                         :follow #'org-geo-link-open)



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


;;; code ref

;;; auto prefix with comment char when create code ref in src block with
;;; `org-store-link'.
(advice-add 'org-store-link :before #'comment-dwim)


;;; [ org-ref ] -- citations, cross-references, indexes, glossaries and bibtex utilities for Org-mode.

(use-package org-ref
  :ensure t
  :init
  (setq org-ref-bibtex-hydra-key-binding (kbd "C-c ]"))

  (unless (boundp 'org-ref-prefix)
    (define-prefix-command 'org-ref-prefix))
  (define-key my-org-prefix (kbd "C-]") 'org-ref-prefix)

  (define-key org-ref-prefix (kbd "C-]") 'org-ref-insert-link)
  (define-key org-ref-prefix (kbd "c") 'org-ref-helm-insert-cite-link)
  (define-key org-ref-prefix (kbd "l") 'org-ref-helm-insert-label-link)
  (define-key org-ref-prefix (kbd "r") 'org-ref-helm-insert-ref-link)

  :config
  (setq bibtex-completion-pdf-open-function 'org-open-file)
  (setq org-latex-prefer-user-labels t)

  ;; Let org-mode auto process the LaTeX export to PDF process.
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  )



(provide 'init-my-org-hyperlink)

;;; init-my-org-hyperlink.el ends here
