;;; init-my-tool-feeds-elfeed.el --- init for elfeed
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

(require 'elfeed)

(setq elfeed-db-directory "~/.emacs.d/.elfeed")

(setq elfeed-feeds '(("http://blog.stackoverflow.com/feed/" Stack-Overflow)
                     ("http://programmers.blogoverflow.com/feed/" Stack-Overflow)

                     ;; Emacs
                     ("http://planet.emacsen.org/atom.xml" Emacs)
                     ("http://www.masteringemacs.org/feed/" Emacs)
                     ("http://emacsrocks.com/atom.xml" Emacs)
                     ("http://emacsmovies.org/atom.xml" Emacs)
                     ("http://emacs-fu.blogspot.com/feeds/posts/default" Emacs)
                     ("http://emacser.com/feed" Emacs)

                     ;; Ruby
                     ("http://www.ruby-lang.org/en/feeds/news.rss" Ruby)
                     ("http://rubyweekly.com/rss" Ruby)
                     ("http://feeds.feedburner.com/RubyInside" Ruby)
                     ("http://feeds2.feedburner.com/Rubyflow" Ruby)
                     ("http://feeds.feedburner.com/Ruby5" Ruby)
                     ("http://ruby-china.org/topics/feed" Ruby)
                     ;; ("http://feeds.feedburner.com/gemcutter-latest" Ruby Gem)

                     ;; Python
                     ;; ("http://pythonnotes.blogspot.com/feeds/posts/default?alt=rss" Python)
                     ;; ("http://neopythonic.blogspot.com/feeds/posts/default?alt=rss" Python)
                     ;; ("http://www.pyside.org/feed/" Python)
                     ;; ("http://www.pythonware.com/daily/rss.xml" Python)

                     ;; Go
                     ;; ("http://blog.golang.org/feeds/posts/default" Go)
                     ;; ("http://feeds.feedburner.com/GoLangTutorials" Go)

                     ;; Web
                     ("https://blog.mozilla.org/feed/" Web)
                     ("http://hacks.mozilla.org/feed/" Web)
                     ("http://feeds.feedburner.com/html5rocks" Web)
                     ("http://feeds2.feedburner.com/css3" Web)
                     ;; ("http://feeds2.feedburner.com/WebDesignerWall" Web)

                     ;; Dart
                     ;; ("http://news.dartlang.org/feeds/posts/default" Dart)
                     ;; ("http://feeds.feedburner.com/dartosphere" Dart)

                     ;; Blogs
                     ;; ("http://feeds.feedburner.com/LoudThinking" Blog)
                     ;; ("http://steve-yegge.blogspot.com/atom.xml" Blog)
                     ;; ("http://www.norvig.com/rss-feed.xml" Blog)
                     ;; ("http://www.tbray.org/ongoing/ongoing.atom" Blog)
                     ;; ("http://feeds.feedburner.com/pedrokroger" Blog)
                     ;; ("http://of-vim-and-vigor.blogspot.com/feeds/posts/default" Blog)
                     ;; ("http://feeds.feedburner.com/semicomplete/main" Blog)
                     ;; ("http://www.altdevblogaday.com/feed/" Blog)
                     ;; ("http://feeds2.feedburner.com/stevelosh" Blog)
                     ;; ("http://feeds.feedburner.com/FistOfSenn" Blog)
                     ;; ("http://ola-bini.blogspot.com/feeds/posts/default" Blog)
                     ;; ("http://www.bunniestudios.com/blog/?feed=rss2" Blog)
                     ;; ("http://feeds.feedburner.com/SanityInc" Blog)
                     ;; ("http://wingolog.org/feed/atom" Blog)
                     ;; ("http://feeds.feedburner.com/codinghorror" Blog)
                     ;; ("http://tomayko.com/feed/" Blog)
                     ;; ("http://newartisans.com/feed/" Blog)
                     ;; ("http://feeds.feedburner.com/NoufalIbrahim" Blog)
                     ;; ("http://pragdave.blogs.pragprog.com/pragdave/atom.xml" Blog)
                     ;; ("http://blog.binux.me/feed/" Blog)
                     ;; ("http://feeds.feedburner.com/xxddite" Blog)
                     ;; ("http://venmos.com/atom.xml" Blog)
                     ;; ("http://pbrisbin.com/feed" Blog)
                     ;; ("http://apt-blog.net/feed" Blog)
                     ;; ("http://program-think.blogspot.com/feeds/posts/default" Blog)
                     ;; ("http://feeds2.feedburner.com/programthink" Blog)
                     ;; ("http://feeds.feedburner.com/ruanyifeng" Blog)
                     ;; ("http://feed.tmdsb.com" Blog)
                     ;; ("http://www.matrix67.com/blog/feed/atom" Blog)
                     ;; ("http://coolshell.cn/?feed=rss2" Blog)
                     ;; ("http://feed.williamlong.info/" Blog)
                     ;; ("http://www.eaglefantasy.com/feed" Blog)
                     ;; ("http://cdwillis.wordpress.com/feed/" Blog)
                     ;; ("http://feeds.feedburner.com/softwaretechandmore" Blog)
                     ;; ("https://www.hackbloc.org/rss.xml" Blog)
                     ;; ("http://blog.ibeini.com/feed" Blog)
                     ;; ("https://www.csslayer.info/wordpress/feed/" Blog)
                     ;; ("http://whileimautomaton.net/rss.rdf" Blog)
                     ;; ("http://www.gregsexton.org/feed/" Blog)
                     ;; ("http://www.artima.com/spotlight/feeds/spotlight.rss" Blog)
                     ;; ("http://www.crazyshell.org/blog/?feed=rss2" Blog)
                     ;; ("http://blog.csdn.net/sw2wolf/rss/list" Blog)
                     ;; ("http://www.kirsle.net/rss.cgi" Blog)
                     ;; ("http://usesthis.com/feed" Blog)

                     ;; Tech Blogs
                     ;; ("http://blog.ycombinator.com/posts.atom" Blog)
                     ;; ("http://blog.csdn.net/sonbai/rss/list" Blog)
                     ;; ("http://www.vpsee.com/feed/" Blog)
                     ;; ("http://0pointer.net/blog/index.atom" Blog)
                     ;; ("http://blog.woodelf.org/feed/" Blog)
                     ;; ("http://www.hyegar.com/atom.xml" Blog)
                     ;; ("http://dev.open.taobao.com/bbs/rss.php" Blog)
                     ;; ("http://ued.taobao.com/blog/feed/" Blog)
                     ;; ("http://instagram-engineering.tumblr.com/rss" Blog)
                     ;; ("http://feeds.feedburner.com/37signals_podcast" Blog)
                     ;; ("http://feeds.feedburner.com/changelogshow" Blog)

                     ;; Linux
                     ("http://openszone.com/feed" Linux)
                     ("https://www.linux.com/rss/feeds.php" Linux)

                     ;; Arch Linux
                     ("http://www.archlinux.org/feeds/news/" Arch)
                     ("http://archlinux.me/feed/" Arch)
                     ("http://planet.archlinux.org/atom.xml" Arch)

                     ;; KDE
                     ;; ("http://www.kde.org/dotkdeorg.rdf" KDE)
                     ;; ("http://planetkde.org/rss20.xml" KDE)
                     ;; ("http://pim.planetkde.org/rss20.xml" KDE)
                     ;; ("http://www.kde.org/dot/kde-apps-content.rdf" KDE)
                     ;; ("http://www.kde.org/kde-look-content.rdf" KDE)
                     ;; ("http://planet.ubuntu.com/rss20.xml" KDE)
                     ;; ("http://www.kubuntu.org/news/feed" KDE)
                     ;; ("http://feeds.feedburner.com/ubuntu-fridge" KDE)
                     ;; ("http://kerneltrap.org/node/feed" KDE)

                     ;; Gimp
                     ;; ("http://www.gimp.org/news.rdf" Gimp)
                     ;; ("http://registry.gimp.org/rss.xml" Gimp)

                     ;; censorship
                     ;; ("http://gamux.org/category/news/tec-blog/feed" censorship)
                     ;; ("http://igfw.net/feed" censorship)

                     ;; Geek News
                     ;; ("http://news.ycombinator.com/rss" geek news)
                     ;; ("http://slashdot.org/index.rss" geek news)
                     ;; ("http://reddit.com/.rss" geek news)
                     ;; ("http://feeds.howtogeek.com/HowTogeek" geek news)
                     ;; ("http://www.thegeekstuff.com/feed" geek news)
                     ("http://www.solidot.org/index.rss" geek news)
                     ("https://projecteuler.net/rss2_euler.xml" geek news)
                     ;; ("http://lwn.net/headlines/newrss" geek news)
                     ("http://linuxtoy.org/feed/" geek news)
                     ("http://linux.cn/rss.xml" geek news)
                     ("http://blog.jobbole.com/feed/" geek news)
                     ;; ("http://feeds.geekpark.net/rss" geek news)
                     ;; ("http://www.guokr.com/rss/" geek news)
                     ;; ("http://songshuhui.net/feed" geek news)

                     ;; Subscribe
                     ("http://feeds.feedburner.com/emule-fans" subscribe)
                     ("https://github.com/blog.atom" subscribe)
                     ;; ("https://github.com/blog/all.atom" subscribe)
                     ;; ("https://github.com/blog/broadcasts.atom" subscribe)
                     ("http://www.kali.org/feed/" subscribe)
                     ("http://www.backbox.org/blog/feed" subscribe)
                     ("http://blog.getfirebug.com/feed/" subscribe)
                     ("http://www.weechat.org/feeds/news/" subscribe)
                     ("http://dev.weechat.org/feed/atom" subscribe)
                     ("http://xkcd.com/rss.xml" subscribe)
                     ))

;; (setq elfeed-initial-tags '(unread))

(setq elfeed-max-connections 4
      elfeed-sort-order 'descending ; by time
      elfeed-search-title-min-width 60
      elfeed-search-title-max-width 60
      elfeed-search-trailing-width 20
      )
(setq-default elfeed-search-filter "@1-week-ago +unread") ; "@1-week-ago +unread", "@6-months-ago +unread"

(define-key my-tools-prefix (kbd "f") 'elfeed)

(define-key elfeed-search-mode-map (kbd "#") 'elfeed-search-set-filter)

(set-face-attribute 'elfeed-search-date-face nil
                    :foreground "#444444")
(set-face-attribute 'elfeed-search-title-face nil
                    :foreground "white")
(set-face-attribute 'elfeed-search-feed-face nil
                    :foreground "orange")
(set-face-attribute 'elfeed-search-tag-face nil
                    :foreground "yellow")


(define-key my-tools-prefix (kbd "f") 'elfeed)


;;; [ elfeed-web ] -- web interface to elfeed.

;;; Usage:
;;
;; - `elfeed-web-start'
;; - `elfeed-web-stop'

(require 'elfeed-web)


(provide 'init-my-tool-feeds-elfeed)

;;; init-my-tool-feeds-elfeed.el ends here
