;;; init-my-tool-feeds.el --- init for feeds in Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ elfeed ] -- An Emacs web feeds client

;;; Usage:
;;
;; - `elfeed' :: run elfeed.
;; - import feeds:
;;   - `elfeed-load-opml'
;; - export feeds:
;;   - `elfeed-export-opml'

;;; keybindings
;;
;; - g :: refresh feed list
;; - G :: update, fetch feed updates
;; - s :: update the search filter (see tags)
;;
;; - RET :: view select entry in a buffer
;; - b :: open selected entries in your browser (browse-url)
;; - y :: copy selected entries URL to clipboard
;; - r :: mark selected entries as read
;; - u :: mark selected entries as unread
;; - + :: add a specific tag to selected entries
;; - - :: remove a specific tag to selected entries
;;
;;; Auto Tagging
;;
;; (setq elfeed-feeds
;;       '(("http://nullprogram.com/feed/" blog emacs)
;;         "http://www.50ply.com/atom.xml"  ; no autotagging
;;         ("http://nedroid.com/feed/" webcomic)))
;;
;;; Filter Syntax:
;;
;; - # or elfeed-search-set-filter
;; - s
;;   - +
;;   - -
;; Any component of the search string beginning with a + or a - is treated like
;; a tag. + means the tag is required, - means the tag must not be present.
;;
;; A component beginning with a @ indicates an age. Entries older than this age
;; are filtered out. The age description accepts plain English, but cannot have
;; spaces, so use dashes. For example, "@2-years-old" or "@3-days-ago". The
;; database is date-oriented, so filters that include an age restriction are
;; significantly more efficient.
;;
;; A component beginning with a ! is treated as an "inverse" regular
;; expression. This means that any entry matching this regular expression will
;; be filtered out. The regular expression begins after the ! character. You can
;; read this as "entry not matching foo".
;;
;;; Tag hooks
;;
;; The last example assumes you've tagged posts with youtube. You probably want
;; to do this sort of thing automatically, either through the "autotags" feature
;; mentioned above, or with the elfeed-new-entry-hook. Functions in this hook
;; are called with new entries, allowing them to be manipulated, such as adding
;; tags.
;;
;; Mark all YouTube entries
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :feed-url "youtube\\.com"
;;                               :add '(video youtube)))
;;
;; Entries older than 2 weeks are marked as read
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :before "2 weeks ago"
;;                               :remove 'unread))
;; Or building your own subset feeds:
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :feed-url "example\\.com"
;;                               :entry-title '(not "something interesting")
;;                               :add 'junk
;;                               :remove 'unread))
;;
;;; Web Interface
;;
;; Elfeed includes a demonstration/toy web interface for remote network
;; access. It's a single-page web application that follows the database live as
;; new entries arrive. It's packaged separately as elfeed-web. To fire it up,
;; run M-x elfeed-web-start and visit http://localhost:8080/elfeed/ (check your
;; httpd-port) with a browser. See the elfeed-web.el header for endpoint
;; documentation if you'd like to access the Elfeed database through the web
;; API.
;;
;; It's rough and unfinished -- no keyboard shortcuts, read-only, no
;; authentication, and a narrow entry viewer. This is basically Elfeed's
;; "mobile" interface. Patches welcome.
;;
;;; Database Management
;;
;; The database should keep itself under control without any manual
;; intervention, but steps can be taken to minimize the database size if
;; desired. The simplest option is to run the elfeed-db-compact command, which
;; will pack the loose-file content database into a single compressed file. This
;; function works well in kill-emacs-hook.
;;
;; Going further, a function could be added to elfeed-new-entry-hook to strip
;; unwanted/unneeded content from select entries before being stored in the
;; database. For example, for YouTube videos only the entry link is of interest
;; and the regularly-changing entry content could be tossed to save time and
;; storage.

;; (setq elfeed-feeds)
(setq elfeed-db-directory "~/.emacs.d/.elfeed")

(setq elfeed-max-connections 4)
(setq-default elfeed-search-filter "@1-week-ago +unread ")

(define-key my-tools-prefix-map (kbd "f") 'elfeed)

(define-key elfeed-show-mode-map (kbd "#") 'elfeed-search-set-filter)


;; (require 'init-my-tool-feeds-newsticker)



(provide 'init-my-tool-feeds)

;;; init-my-tool-feeds.el ends here
