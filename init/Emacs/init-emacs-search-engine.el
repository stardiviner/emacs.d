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
           ("DuckDuckGo" . [simple-query "duckduckgo.com" "https://duckduckgo.com/?q=" ""])
           ("DogeDoge" . [simple-query "dogedoge.com" "https://www.dogedoge.com/results?q=" ""])
           ("Blekko" . [simple-query "blekko.com" "https://blekko.com/#?q=" ""])
           ("Bing" . [simple-query "bing.com" "http://cn.bing.com/search?q=" ""])
           ("Baidu 百度" . [simple-query "baidu.com" "http://www.baidu.com/s?wd=" ""])
           ("Douban 豆瓣" . [simple-query "douban.com" "https://www.douban.com/search?source=suggest&q=" ""])
           ("Magi" . [simple-query "magi.com" "https://magi.com/search?q=" ""])
           ;; wiki
           ("Wikipedia (zh-CN) 维基百科" .
            [simple-query "wikipedia.org" "http://www.wikipedia.org/search-redirect.php?language=zh&go=Go&search=" ""])
           ("Baidu·Baike 百度百科"
            . [simple-query "baike.baidu.com" "http://baike.baidu.com/search/none?word=" ""])
           ;; Q&A
           ("Stack Exchange" . [simple-query "stackexchange.com" "http://stackexchange.com/search?q=" ""])
           ("Stack Overflow" . [simple-query "stackoverflow.com" "https://stackoverflow.com/search?q=" ""])
           ("Quora" . [simple-query "quora.com" "https://www.quora.com/search?q=" ""])
           ("Zhihu 知乎" . [simple-query "zhihu.com" "http://www.zhihu.com/search?q=" ""])
           ;; Tools
           ("Wolfram Alpha (Mathematica)" .
            [simple-query "wolframalpha.com" "http://www.wolframalpha.com/input/?i=" ""])
           ;; Translate
           ("DeepL Translate" .
            [simple-query "deepl.com" "https://www.deepl.com/en/translator#auto/zh/" ""])
           ("Google Translate" .
            [simple-query "translate.google.com" "https://translate.google.com/?q=" "&sl=auto&tl=zh-CN&op=translate"])
           ("Google Translate URL (suggested)" .
            [simple-query "translate.google.com" "https://translate.google.com/website?sl=auto&tl=zh-CN&u=" ""])
           ;; ("Google Translate URL" .
           ;;  [simple-query "translate.google.com" "https://translate.google.com.hk/translate?hl=&sl=auto&tl=zh-CN&u=" "&sandbox=1"])
           ("百度翻译 Baidu Translate" .
            [simple-query "fanyi.baidu.com" "https://fanyi.baidu.com/#en/zh/" ""])
           ;; Languages
           ("TeX/LaTeX CTAN" . [simple-query "ctan.org" "https://www.ctan.org/search/?phrase=" ""])
           ;; Programming Docs: API
           ("APIs" . [simple-query "apis.io" "http://apis.io/?search=" ""])
           ("Rosetta Code" .
            [simple-query "rosettacode.org" "https://www.rosettacode.org/mw/index.php?&search=" ""])
           ("Mozilla Developer" .
            [simple-query "developer.mozilla.org" "https://developer.mozilla.org/en-US/search?q=" ""])
           ("RFC" .
            [simple-query "ietf.org" "https://datatracker.ietf.org/doc/search/?activedrafts=on&rfcs=on&name=" ""])
           ("Emacs Wiki" . [simple-query "emacswiki.org" "www.emacswiki.org/emacs?search=" ""])
           ("Stack Exchange · Emacs" .
            [simple-query "emacs.stackexchange.com" "http://emacs.stackexchange.com/search?q=" ""])
           ("Emacs China Forum" . [simple-query "emacs-china.org" "https://emacs-china.org/search?q=" ""])
           ("Lisp doc" . [simple-query "lispdoc.com" "http://lispdoc.com/?q=" ""])
	       ("cljdoc: documentation for Clojure/ClojureScript libraries" .
	        [simple-query "cljdoc.org" "https://cljdoc.org/search?q=require" ""])
           ("Clojure Docs" . [simple-query "clojuredocs.org" "http://clojuredocs.org/search?q=" ""])
           ("CrossClj" . "https://crossclj.info")
           ("Java Docs" .
            [simple-query "docs.oracle.com" "https://docs.oracle.com/apps/search/search.jsp?category=java&q=" ""])
           ("clojars" . [simple-query "clojars.org" "https://clojars.org/search?q=" ""])
           ("Maven" . [simple-query "search.maven.org" "https://search.maven.org/search?q=" ""])
           ;; Library Documentation Hosting for Common Lisp
           ("Quickdocs" . [simple-query "quickdocs.org" "http://quickdocs.org/search?q=" ""])
           (".NET API Docs" .
            [simple-query "docs.microsoft.com" "https://docs.microsoft.com/en-us/dotnet/api/?term=" ""])
           ("Ruby doc" .
            [simple-query "ruby-doc.com" "http://ruby-doc.com/search.html?q=" ""])
           ("Python 3 Docs" .
            [simple-query "docs.python.org" "http://docs.python.org/3/search.html?q=" ""])
           ("PyPi: Python pip package search" .
            [simple-query "pypi.org" "https://pypi.org/search/?q=" ""])
           ("npm package search" .
            [simple-query "npmjs.com" "https://www.npmjs.com/search?q=" ""])
           ("Perl CPAN" .
            [simple-query "search.cpan.org" "http://search.cpan.org/search?mode=all&query=" ""])
           ("PHP Doc" . [simple-query "cn2.php.net" "http://cn2.php.net/results.php?q=" ""])
           ("Mozilla Developer - JavaScript Docs" .
            [simple-query "developer.mozilla.org" "https://developer.mozilla.org/en-US/search?q=" ""])
           ;; Search Code
           ("GitHub" .
            [simple-query "github.com" "https://github.com/search?ref=simplesearch&q=" "&type=Code"])
           ("Code Search" . [simple-query "searchcode.com" "http://searchcode.com/?q=" ""])
           ("SourceForge" . [simple-query "sourceforge.net" "https://sourceforge.net/directory/?q=" ""])
           ("Visual Studio Code Extensions Market" .
            [simple-query "marketplace.visualstudio.com"
                          "https://marketplace.visualstudio.com/search?term="
                          "&target=VSCode&category=All%20categories&sortBy=Relevance"])
           ;; Linux
           ("Linux pkgs.org package searching" . [simple-query "pkgs.org" "https://pkgs.org/search/?q=" ""])
           ("Archlinux Wiki" .
            [simple-query "wiki.archlinux.org"
                          "https://wiki.archlinux.org/index.php/Special:Search&fulltext=Search?search=" ""])
           ("Ubuntu Packages Search" . [simple-query "packages.ubuntu.com/" "https://packages.ubuntu.com/search?suite=all&searchon=names&keywords=" ""])
           ;; Docker
           ("Docker Hub" . [simple-query "hub.docker.com" "https://hub.docker.com/search?q=" "&type=image"])
           ;; Mailing Lists
           ("emacs-help mailing list" .
            [simple-query "lists.gnu.org"
                          "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?submit=Search&idxname=help-gnu-emacs&max=20&result=normal&sort=score&query=" ""])
           ("emacs-devel mailing list" .
            [simple-query "lists.gnu.org"
                          "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?submit=Search&idxname=emacs-devel&query=" ""])
           ("org-mode mailing list" .
            [simple-query "lists.gnu.org"
                          "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?submit=Search&idxname=emacs-orgmode&max=20&result=normal&sort=score&query=" ""])
           ;; Web Browser Extensions
           ("Firefox Addons Extensions" .
            [simple-query "addons.mozilla.org" "https://addons.mozilla.org/en-US/firefox/search/?platform=linux&q=" ""])
           ;; Downloads
           ("The Pirate Bay 海盗湾" .
            [simple-query "thepiratebay.org" "https://thepiratebay.org/search/" ""])
           ;; Documents
           ("Z-Library (zlibraryexau2g3p.onion Tor version)" . [simple-query "1lib.org" "https://1lib.org/s/" ""])
           ;; ("Z-Library (book4you.org Premium member only)" . [simple-query "book4you.org" "https://book4you.org/s/" ""])
           ;; ("Z-Library (b-ok.global)" . [simple-query "b-ok.global" "https://b-ok.global/s/" ""])
           ("Z-Library (1lib.org Books)" . [simple-query "1lib.org" "https://1lib.org/s/" ""])
           ("鸠摩 文档搜索引擎 jiumodiary" . "https://www.jiumodiary.com/")
           ("蓝菊花 lanjuhua" . "http://www.lanjuhua.com/")
           ("深度开源 · 文库" . [simple-query "open-open.com" "https://www.open-open.com/wenku/?kw=" ""])
           ;; Videos
           ("YouTube" .
            [simple-query "youtube.com" "http://www.youtube.com/results?aq=f&oq=&search_query=" ""])
           ("Bilibili 哔哩哔哩" .
            [simple-query "bilibili.com" "https://search.bilibili.com/all?keyword=" ""])
           ("Douban books 豆瓣图书" .
            [simple-query "book.douban.com" "http://book.douban.com/subject_search?search_text=" ""])
           ("Douban movies 豆瓣电影" .
            [simple-query "movie.douban.com" "http://movie.douban.com/subject_search?search_text=" ""])
           ("IMDb" . [simple-query "imdb.com" "http://www.imdb.com/title/" ""])
           ("电影狗 dianyinggou.com" . [simple-query "dianyinggou.com" "http://www.dianyinggou.com/so/" ""])
           ("btbtdy.com" . [simple-query "btbtdy.com" "http://btbtdy3.com/search/" ".html"])
           ;; Subtitle
           ("zimuku 字幕库 subtitle download"
            . [simple-query "zimuku.la" "http://www.zimuku.la/search?q=" ""])
           ;; Musics
           ("NetEase 163 Music | 网易云音乐" .
            [simple-query "music.163.com" "http://music.163.com/#/search/m/?s=" ""])
           ("千千音乐" . [simple-query "music.taihe.com" "https://music.taihe.com/search?word=" ""])
           ;; Social Network
           ("Twitter" . [simple-query "twitter.com" "https://twitter.com/search?q=" ""])
           ("Facebook" . [simple-query "facebook.com" "https://www.facebook.com/" ""])
           ;; Law
           ("法信 faxin.cn" . "http://www.faxin.cn/")
           ;; Papers
           ("arXiv e-Print archives" .
            [simple-query "arxiv.org" "https://arxiv.org/search/?query=" "&searchtype=all&abstracts=show&order=-announced_date_first&size=50"])
           ;; Shopping
           ("淘宝 taobao.com" . [simple-query "taobao.com" "https://s.taobao.com/search?q=" "&s_from=newHeader&ssid=s5-e&search_type=item&sourceId=tb.item"])
           ("淘宝规则 rule.taobao.com" . [simple-query "rule.taobao.com" "https://rule.taobao.com/search.htm?key=" ""])
           ;; Porn
           ("PornHub" . [simple-query "pornhub.com" "https://www.pornhub.com/video/search?search=" ""])
           )
         webjump-sample-sites)))


(provide 'init-emacs-search-engine)

;;; init-emacs-search-engine.el ends here
