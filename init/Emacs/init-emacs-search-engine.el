;;; init-emacs-search-engine.el --- init for Search Engines
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ WebJump ] -- Emacs built-in programmable Web hotlist.

(use-package webjump
  :defer t
  :commands (webjump)
  :bind ("C-x /" . webjump)
  :config
  (setq webjump-sites
        (append
         '(;; Search Engines
           ("Google" . [simple-query "google.com" "https://www.google.com.tw/search?safe=off&q=" ""])
           ("Blekko" . [simple-query "blekko.com" "https://blekko.com/#?q=" ""])
           ("Bing" . [simple-query "bing.com" "http://cn.bing.com/search?q=" ""])
           ("Baidu 百度" . [simple-query "baidu.com" "http://www.baidu.com/s?wd=" ""])
           ;; wiki
           ("Wikipedia (zh-CN) 维基百科" . [simple-query "wikipedia.org" "http://www.wikipedia.org/search-redirect.php?language=zh&go=Go&search=" ""])
           ("Baidu·Baike 百度百科" . [simple-query "baike.baidu.com" "http://baike.baidu.com/search/none?word=" ""])
           ;; Q&A
           ("Stack Exchange" . [simple-query "stackexchange.com" "http://stackexchange.com/search?q=" ""])
           ("Stack Overflow" . [simple-query "stackoverflow.com" "https://stackoverflow.com/search?q=" ""])
           ("Quora" . [simple-query "quora.com" "https://www.quora.com/search?q=" ""])
           ("Zhihu 知乎" . [simple-query "zhihu.com" "http://www.zhihu.com/search?q=" ""])
           ;; Tools
           ("Wolfram Alpha" . [simple-query "wolframalpha.com" "http://www.wolframalpha.com/input/?i=" ""])
           ("Google Translate" . [simple-query "translate.google.com" "https://translate.google.com/?q=" ""])
           ;; Languages
           ("TeX/LaTeX CTAN" . [simple-query "ctan.org" "https://www.ctan.org/search/?phrase=" ""])
           ;; Programming Docs: API
           ("APIs" . [simple-query "apis.io" "http://apis.io/?search=" ""])
           ("Rosetta Code" . [simple-query "rosettacode.org" "https://www.rosettacode.org/mw/index.php?&search=" ""])
           ("Mozilla Developer" . [simple-query "developer.mozilla.org" "https://developer.mozilla.org/en-US/search?q=" ""])
           ("RFC" . [simple-query "ietf.org" "https://datatracker.ietf.org/doc/search/?activedrafts=on&rfcs=on&name=" ""])
           ("Emacs Wiki" . [simple-query "emacswiki.org" "www.emacswiki.org/emacs?search=" ""])
           ("Stack Exchange · Emacs" . [simple-query "emacs.stackexchange.com" "http://emacs.stackexchange.com/search?q=" ""])
           ("Lisp doc" . [simple-query "lispdoc.com" "http://lispdoc.com/?q=" ""])
           ("Clojure Docs" . [simple-query "clojuredocs.org" "http://clojuredocs.org/search?q=" ""])
           ("CrossClj" . "https://crossclj.info")
           ("Java Docs" . [simple-query "docs.oracle.com" "https://docs.oracle.com/apps/search/search.jsp?category=java&q=" ""])
           ("clojars" . [simple-query "clojars.org" "https://clojars.org/search?q=" ""])
           ("Maven" . [simple-query "search.maven.org" "https://search.maven.org/search?q=" ""])
           ;; Library Documentation Hosting for Common Lisp
           ("Quickdocs" . [simple-query "quickdocs.org" "http://quickdocs.org/search?q=" ""])
           (".NET API Docs" . [simple-query "docs.microsoft.com" "https://docs.microsoft.com/en-us/dotnet/api/?term=" ""])
           ("Ruby doc" . [simple-query "ruby-doc.com" "http://ruby-doc.com/search.html?q=" ""])
           ("Python 3 Docs" . [simple-query "docs.python.org" "http://docs.python.org/3/search.html?q=" ""])
           ("Perl CPAN" . [simple-query "search.cpan.org" "http://search.cpan.org/search?mode=all&query=" ""])
           ("PHP Doc" . [simple-query "cn2.php.net" "http://cn2.php.net/results.php?q=" ""])
           ("Mozilla Developer - JavaScript Docs" . [simple-query "developer.mozilla.org" "https://developer.mozilla.org/en-US/search?q=" ""])
           ;; Search Code
           ("GitHub" . [simple-query "github.com" "https://github.com/search?ref=simplesearch&q=" ""])
           ("Code Search" . [simple-query "searchcode.com" "http://searchcode.com/?q=" ""])
           ;; Linux
           ("Archlinux Wiki" . [simple-query "wiki.archlinux.org" "https://wiki.archlinux.org/index.php/Special:Search&fulltext=Search?search=" ""])
           ;; Mailing Lists
           ("emacs-help mailing list" . [simple-query "lists.gnu.org" "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?submit=Search&idxname=help-gnu-emacs&max=20&result=normal&sort=score&query=" ""])
           ("emacs-devel mailing list" . [simple-query "lists.gnu.org" "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?submit=Search&idxname=emacs-devel&query=" ""])
           ("org-mode mailing list" . [simple-query "lists.gnu.org" "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?submit=Search&idxname=emacs-orgmode&max=20&result=normal&sort=score&query=" ""])
           ;; Downloads
           ("The Pirate Bay 海盗湾" . [simple-query "thepiratebay.org" "https://thepiratebay.org/search/" ""])
           ;; Videos
           ("YouTube" . [simple-query "youtube.com" "http://www.youtube.com/results?aq=f&oq=&search_query=" ""])
           ("Bilibili 哔哩哔哩" . [simple-query "bilibili.com" "https://search.bilibili.com/all?keyword=" ""])
           ("Douban books 豆瓣图书" . [simple-query "book.douban.com" "http://book.douban.com/subject_search?search_text=" ""])
           ("Douban movies 豆瓣电影" . [simple-query "movie.douban.com" "http://movie.douban.com/subject_search?search_text=" ""])
           ("IMDb" . [simple-query "imdb.com" "http://www.imdb.com/title/" ""])
           ("NetEase 163 Music | 网易云音乐" . [simple-query "music.163.com" "http://music.163.com/#/search/m/?s=" ""])
           ;; Social Network
           ("Twitter" . [simple-query "twitter.com" "https://twitter.com/search?q=" ""])
           ("Facebook" . [simple-query "facebook.com" "https://www.facebook.com/" ""])
           ;; Law
           ("法信" . "http://www.faxin.cn/")
           )
         webjump-sample-sites)))


(provide 'init-emacs-search-engine)

;;; init-emacs-search-engine.el ends here
