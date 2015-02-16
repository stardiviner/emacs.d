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

(setq elfeed-db-directory "~/.emacs.d/.elfeed")

(setq elfeed-feeds '("https://github.com/blog/all.atom"
                     "https://github.com/blog/broadcasts.atom"
                     "http://planet.emacsen.org/atom.xml"
                     "http://www.masteringemacs.org/feed/"
                     "http://emacsrocks.com/atom.xml"
                     "http://emacsmovies.org/atom.xml"
                     "http://emacs-fu.blogspot.com/feeds/posts/default"
                     "http://emacser.com/feed"
                     "http://endlessparentheses.com/atom.xml"
                     "http://www.ruby-lang.org/en/feeds/news.rss"
                     "http://rubyweekly.com/rss"
                     "http://feeds.feedburner.com/RubyInside"
                     "http://feeds2.feedburner.com/Rubyflow"
                     "http://feeds.feedburner.com/Ruby5"
                     "http://ruby-china.org/topics/feed"
                     ;; "http://feeds.feedburner.com/gemcutter-latest"

                     ;; Python
                     ;; "http://pythonnotes.blogspot.com/feeds/posts/default?alt=rss"
                     ;; "http://neopythonic.blogspot.com/feeds/posts/default?alt=rss"
                     ;; "http://www.pyside.org/feed/"
                     ;; "http://www.pythonware.com/daily/rss.xml"

                     ;; Go
                     ;; "http://blog.golang.org/feeds/posts/default"
                     ;; "http://feeds.feedburner.com/GoLangTutorials"

                     ;; Web
                     "https://blog.mozilla.org/feed/"
                     "http://hacks.mozilla.org/feed/"
                     "http://feeds.feedburner.com/html5rocks"
                     "http://feeds2.feedburner.com/css3"
                     ;; "http://feeds2.feedburner.com/WebDesignerWall"
                     ;; "http://LaTex.yo2.cn/feed"

                     ;; Dart
                     ;; "http://news.dartlang.org/feeds/posts/default"
                     ;; "http://feeds.feedburner.com/dartosphere"

                     ;; Blogs
                     ;; "http://feeds.feedburner.com/LoudThinking"
                     ;; "http://steve-yegge.blogspot.com/atom.xml"
                     ;; "http://www.norvig.com/rss-feed.xml"
                     ;; "http://www.tbray.org/ongoing/ongoing.atom"
                     ;; "http://feeds.feedburner.com/pedrokroger"
                     ;; "http://of-vim-and-vigor.blogspot.com/feeds/posts/default"
                     ;; "http://feeds.feedburner.com/semicomplete/main"
                     ;; "http://www.altdevblogaday.com/feed/"
                     ;; "http://feeds2.feedburner.com/stevelosh"
                     ;; "http://feeds.feedburner.com/FistOfSenn"
                     ;; "http://ola-bini.blogspot.com/feeds/posts/default"
                     ;; "http://www.bunniestudios.com/blog/?feed=rss2"
                     ;; "http://feeds.feedburner.com/SanityInc"
                     ;; "http://wingolog.org/feed/atom"
                     ;; "http://feeds.feedburner.com/codinghorror"
                     ;; "http://tomayko.com/feed/"
                     ;; "http://newartisans.com/feed/"
                     ;; "http://feeds.feedburner.com/NoufalIbrahim"
                     ;; "http://pragdave.blogs.pragprog.com/pragdave/atom.xml"
                     ;; "http://blog.binux.me/feed/"
                     ;; "http://feeds.feedburner.com/xxddite"
                     ;; "http://venmos.com/atom.xml"
                     ;; "http://pbrisbin.com/feed"
                     ;; "http://apt-blog.net/feed"
                     ;; "http://program-think.blogspot.com/feeds/posts/default"
                     ;; "http://feeds2.feedburner.com/programthink"
                     ;; "http://feeds.feedburner.com/ruanyifeng"
                     ;; "http://feed.tmdsb.com"
                     ;; "http://www.matrix67.com/blog/feed/atom"
                     ;; "http://coolshell.cn/?feed=rss2"
                     ;; "http://feed.williamlong.info/"
                     ;; "http://www.eaglefantasy.com/feed"
                     ;; "http://cdwillis.wordpress.com/feed/"
                     ;; "http://feeds.feedburner.com/softwaretechandmore"
                     ;; "https://www.hackbloc.org/rss.xml"
                     ;; "http://blog.ibeini.com/feed"
                     ;; "https://www.csslayer.info/wordpress/feed/"
                     ;; "http://whileimautomaton.net/rss.rdf"
                     ;; "http://www.gregsexton.org/feed/"
                     ;; "http://www.artima.com/spotlight/feeds/spotlight.rss"
                     ;; "http://www.crazyshell.org/blog/?feed=rss2"
                     ;; "http://blog.csdn.net/sw2wolf/rss/list"
                     ;; "http://www.kirsle.net/rss.cgi"
                     ;; "http://usesthis.com/feed"

                     ;; Tech Blogs
                     "http://blog.ycombinator.com/posts.atom"
                     "http://blog.csdn.net/sonbai/rss/list"
                     "http://www.vpsee.com/feed/"
                     "http://0pointer.net/blog/index.atom"
                     "http://blog.woodelf.org/feed/"
                     "http://www.hyegar.com/atom.xml"
                     "http://dev.open.taobao.com/bbs/rss.php"
                     "http://ued.taobao.com/blog/feed/"
                     "http://instagram-engineering.tumblr.com/rss"
                     "http://feeds.feedburner.com/37signals_podcast"
                     "http://feeds.feedburner.com/changelogshow"

                     ;; Linux
                     "http://openszone.com/feed"
                     "https://www.linux.com/rss/feeds.php"

                     ;; Arch Linux
                     "http://www.archlinux.org/feeds/news/"
                     "http://archlinux.me/feed/"
                     "http://planet.archlinux.org/atom.xml"

                     ;; KDE
                     "http://www.kde.org/dotkdeorg.rdf"
                     "http://planetkde.org/rss20.xml"
                     "http://pim.planetkde.org/rss20.xml"
                     "http://www.kde.org/dot/kde-apps-content.rdf"
                     "http://www.kde.org/kde-look-content.rdf"
                     "http://planet.ubuntu.com/rss20.xml"
                     "http://www.kubuntu.org/news/feed"
                     "http://feeds.feedburner.com/ubuntu-fridge"
                     "http://kerneltrap.org/node/feed"

                     ;; Maemo
                     ;; "http://maemo.org/news/latest.xml"
                     ;; "http://my-maemo.com/maemo.xml"
                     ;; "http://maemoworld.org/feed/"
                     ;; "http://n900-pentesting.blogspot.com/feeds/posts/default?alt=rss"
                     ;; "http://rss.allaboutmeego.com/aam-feed-summary.xml"

                     ;; Gimp
                     ;; "http://www.gimp.org/news.rdf"
                     ;; "http://registry.gimp.org/rss.xml"

                     ;; censorship
                     ;; "http://gamux.org/category/news/tec-blog/feed"
                     ;; "http://igfw.net/feed"

                     ;; Geek News
                     "http://news.ycombinator.com/rss"
                     "http://slashdot.org/index.rss"
                     "http://reddit.com/.rss"
                     "http://www.solidot.org/index.rss"
                     "https://projecteuler.net/rss2_euler.xml"
                     "http://lwn.net/headlines/newrss"
                     "http://linuxtoy.org/feed/"
                     "http://linux.cn/rss.xml"
                     "http://blog.jobbole.com/feed/"
                     "http://feeds.geekpark.net/"
                     "http://www.guokr.com/rss/"
                     "http://songshuhui.net/feed"
                     "http://zhidemofang.com/feed"
                     "http://feeds.howtogeek.com/HowToGeek"
                     "http://www.thegeekstuff.com/feed"

                     ;; Subscribe
                     "http://feeds.feedburner.com/emule-fans"
                     "https://www.omnigroup.com/blog/rss/"
                     "http://blog.stackoverflow.com/feed/"
                     "http://programmers.blogoverflow.com/feed/"
                     "https://github.com/blog.atom"
                     "http://blog.gitcafe.com/feed"
                     "https://owncloud.org/blogfeed/"
                     "http://www.kali.org/feed/"
                     "http://www.backbox.org/blog/feed"
                     "http://project-byzantium.org/feed/"
                     "http://blog.getfirebug.com/feed/"
                     "http://subforge.org/projects/subtle/news.atom"
                     "http://definitely-awesome.posterous.com/rss.xml"
                     "http://www.weechat.org/feeds/news/"
                     "http://dev.weechat.org/feed/atom"
                     "http://xkcd.com/rss.xml"
                     ))

(setq elfeed-max-connections 4)
(setq-default elfeed-search-filter "@1-week-ago +unread ")

(define-key my-tools-prefix-map (kbd "f") 'elfeed)

(define-key elfeed-show-mode-map (kbd "#") 'elfeed-search-set-filter)


;; (require 'init-my-tool-feeds-newsticker)



(provide 'init-my-tool-feeds)

;;; init-my-tool-feeds.el ends here
