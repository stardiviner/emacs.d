;;; init-emacs-search-engine.el --- init for Search Engines
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ engine-mode ] -- minor-mode for defining and querying search engines.

(use-package engine-mode
  :ensure t
  :defer t
  ;; :init (engine-mode t)
  :config
  (when (featurep 'eaf)
    (with-eval-after-load 'eaf
      (defun eaf-browse-url (url &optional arguments)
        "Open EAF browser application given a URL and ARGUMENTS."
        (eaf-open (eaf-wrap-url url) "browser" arguments))
      (setq engine/browser-function 'eaf-browse-url)))
  
  ;; general search engines
  (defengine google
    ;; "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    ;; "http://www.google.com/search?q=%s"
    "https://www.google.com.tw/search?safe=off&q=%s"
    :docstring "Google"
    :keybinding "g")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :docstring "DuckDuckGo"
    :keybinding "d")
  (defengine blekko
    "https://blekko.com/#?q=%s"
    :docstring "Blekko"
    ;; :keybinding "B"
    )
  (defengine bing
    "http://cn.bing.com/search?q="
    :docstring "Bing")
  (defengine baidu
    "http://www.baidu.com/s?wd=%s"
    :docstring "Baidu"
    :keybinding "b")

  ;; Wikipedia
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :docstring "Wikipedia"
    :keybinding "w")
  (defengine wikipedia-zh
    "http://www.wikipedia.org/search-redirect.php?language=zh&go=Go&search=%s"
    :docstring "Wikipedia"
    :keybinding "W")
  (defengine baidu-baike
    "http://baike.baidu.com/search/none?word=%s"
    :docstring "Baidu Baike"
    :keybinding "B")
  (defengine stack-exchange
    "http://stackexchange.com/search?q=%s"
    :docstring "Stack Exchange all sites: powered by Google Custom Search"
    :keybinding "s")

  ;; Computational Knowledge Engine
  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :docstring "Wolfram Alpha"
    :keybinding "A")

  ;; Translation
  (defengine google-translate
    "https://translate.google.com/?q=%s"
    :docstring "Google Translate"
    :keybinding "T")

  ;; Maps

  ;; Programming

  ;; Programming Languages
  ;; TeX/LaTeX
  (defengine TeX-LaTeX-CTAN
    "https://www.ctan.org/search/?phrase=%s"
    :docstring "TeX/LaTeX CTAN"
    :keybinding "t")

  ;; Docs: API
  (defengine APIs
    "http://apis.io/?search=%s"
    :docstring "APIs"
    :keybinding "a")
  (defengine Rosetta-Code
    "https://www.rosettacode.org/mw/index.php?&search=%s"
    :docstring "Rosetta Code"
    :keybinding "r")
  (defengine mozilla-developer
    "https://developer.mozilla.org/en-US/search?q=%s"
    :docstring "Mozilla Developer"
    :keybinding "m")
  (defengine rfcs
    "https://datatracker.ietf.org/doc/search/?name=%s&activedrafts=on&rfcs=on"
    ;; "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s"
    :docstring "RFC"
    :keybinding "R")
  (defengine emacs-wiki
    "www.emacswiki.org/emacs?search=%s"
    :docstring "Emacs Wiki"
    ;; :keybinding ""
    )
  (defengine stack-emacs
    "http://emacs.stackexchange.com/search?q=%s"
    :docstring "Emacs Stack"
    :keybinding "E")
  (defengine lisp-doc
    "http://lispdoc.com/?q=%s"
    :docstring "Lisp doc")
  (defengine clojure-docs
    "http://clojuredocs.org/search?q=%s"
    :docstring "ClojureDocs.org"
    :keybinding "C")
  (defengine crossclj
    "https://crossclj.info"
    :docstring "CrossClj: cross-referencing the Clojure ecosystem."
    :keybinding "j")
  (defengine clojars
    "https://clojars.org/search?q=%s"
    :docstring "clojars")
  (defengine java-docs
    "https://docs.oracle.com/apps/search/search.jsp?category=java&q=%s"
    :docstring "Java Docs"
    :keybinding "J")
  (defengine maven
    "https://search.maven.org/search?q=%s"
    :docstring "Maven"
    :keybinding "M")
  (defengine dotnet-api
    "https://docs.microsoft.com/en-us/dotnet/api/?term=%s"
    :docstring ".NET API"
    :keybinding "n")
  (defengine ruby-doc
    "http://ruby-doc.com/search.html?q=%s"
    :docstring "Ruby doc")
  (defengine python-3-doc
    "http://docs.python.org/3/search.html?q=%s"
    :docstring "Python 3 doc")
  (defengine perl-doc-cpan
    "http://search.cpan.org/search?mode=all&query=%s"
    :docstring "Perl CPAN")
  (defengine php-doc
    "http://cn2.php.net/results.php?q=%s&p=manual"
    :docstring "PHP doc")
  (defengine javascript-doc
    "https://developer.mozilla.org/en-US/search?q=%s"
    :docstring "Mozilla Developer - JavaScript Doc")

  ;; code
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :docstring "GitHub"
    :keybinding "G")
  (defengine code-search
    "http://searchcode.com/?q=%s"
    :docstring "Code Search"
    :keybinding "c")

  ;; Linux
  (defengine archlinux-wiki
    "https://wiki.archlinux.org/index.php/Special:Search?search=%s&fulltext=Search"
    :docstring "Archlinux Wiki"
    :keybinding "l")

  ;; Q&A
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :docstring "Stack Overflow"
    :keybinding "S")
  (defengine quora
    "https://www.quora.com/search?q=%s"
    :docstring "Quora"
    :keybinding "q")
  (defengine zhihu
    "http://www.zhihu.com/search?q=%s&type=question"
    :docstring "ZhiHu"
    :keybinding "z")

  ;; Mailing Lists
  (defengine emacs-help
    "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s&submit=Search&idxname=help-gnu-emacs&max=20&result=normal&sort=score"
    :docstring "emacs-help"
    :keybinding "e")
  (defengine emacs-devel
    "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s&submit=Search&idxname=emacs-devel"
    :docstring "emacs-devel")
  (defengine org-mode
    "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s&submit=Search&idxname=emacs-orgmode&max=20&result=normal&sort=score"
    :docstring "org-mode"
    :keybinding "o")

  ;; Downloads
  (defengine piratebay
    "https://thepiratebay.org/search/%s"
    :docstring "The Pirate Bay"
    :keybinding "p")

  ;; Media
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :docstring "YouTube"
    :keybinding "y")
  (defengine bilibili
    "https://search.bilibili.com/all?keyword=%s"
    :docstring "Bilibili")
  (defengine douban-books
    "http://book.douban.com/subject_search?search_text=%s"
    :docstring "Douban Books")
  (defengine douban-movies
    "http://movie.douban.com/subject_search?search_text=%s"
    :docstring "Douban Movies")
  (defengine imdb
    "http://www.imdb.com/title/%s"
    :docstring "IMDb")
  (defengine netease-music
    "http://music.163.com/#/search/m/?s=%s"
    :docstring "NetEase Music")

  ;; Social Network
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :docstring "Twitter")
  (defengine facebook
    "https://www.facebook.com/%s"
    :docstring "Facebook")

  ;; Law
  (defengine law
    "http://www.faxin.cn/"
    :docstring "法信"
    :keybinding "L")

  (defun engine-search-engines (&optional name query)
    "A fast interactive entry of all defined search engines."
    (interactive (list
                  (completing-read
                   "Select Search Engine: "
                   (mapcar
                    (lambda (str) (replace-regexp-in-string "engine\/search-" "" (symbol-name str)))
                    (apropos-internal "engine/search-")))
                  (read-string "Input Search Query: ")))
    (let ((engine (intern (format "engine/search-%s" name))))
      (funcall engine query)))

  (engine-mode -1)
  (global-set-key (kbd "C-x /") 'engine-search-engines))


(provide 'init-emacs-search-engine)

;;; init-emacs-search-engine.el ends here
