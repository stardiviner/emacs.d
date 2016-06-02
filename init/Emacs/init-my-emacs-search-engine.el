;;; init-my-emacs-search-engine.el --- init for Search Engines
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ engine-mode ] -- minor-mode for defining and querying search engines.

(use-package engine-mode
  :ensure t
  :config
  ;; enable `engine-mode' minor-mode for making keybindings.
  (engine-mode t)
  
  (setq engine/keybinding-prefix "C-x /"
        ;; engine/browser-function
        )
  
  ;; general search engines
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    ;; "http://www.google.com/search?q=%s"
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
  (defengine baidu_baike
    "http://baike.baidu.com/search/none?word=%s"
    :docstring "Baidu Baike"
    :keybinding "W")
  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :docstring "Wolfram Alpha"
    :keybinding "A")

  ;; programming

  ;; Docs: API
  (defengine APIs
    "http://apis.io/?search=%s"
    :docstring "APIs"
    :keybinding "a")
  (defengine mozilla-developer
    "https://developer.mozilla.org/en-US/search?q=%s"
    :docstring "Mozilla Developer"
    :keybinding "m")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    ;; "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s"
    :docstring "RFC"
    :keybinding "R")
  (defengine emacswiki
    "www.emacswiki.org/emacs?search=%s"
    :docstring "Emacs Wiki"
    :keybinding "e")
  (defengine stack-emacs
    "http://emacs.stackexchange.com/search?q=%s"
    :docstring "Emacs Stack"
    :keybinding "E")
  (defengine lisp-doc
    "http://lispdoc.com/?q=%s"
    :docstring "Lisp doc")
  (defengine ruby-doc
    "http://ruby-doc.com/search.html?q=%s"
    :docstring "Ruby doc")
  (defengine python-doc-3
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

  ;; Q&A
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :docstring "Stack Overflow"
    :keybinding "o")
  (defengine quora
    "https://www.quora.com/search?q=%s"
    :docstring "Quora"
    :keybinding "q")
  (defengine zhihu
    "http://www.zhihu.com/search?q=%s&type=question"
    :docstring "ZhiHu"
    :keybinding "z")

  ;; Media
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :docstring "YouTube"
    :keybinding "y")
  (defengine douban-books
    "http://book.douban.com/subject_search?search_text=%s"
    :docstring "Douban Books")
  (defengine douban-movies
    "http://movie.douban.com/subject_search?search_text=%s"
    :docstring "Douban Movies")
  (defengine imdb
    "http://www.imdb.com/title/%s"
    :docstring "IMDb")

  ;; Social Network
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :docstring "Twitter")
  (defengine facebook
    "https://www.facebook.com/%s"
    :docstring "Facebook")
  )


(provide 'init-my-emacs-search-engine)

;;; init-my-emacs-search-engine.el ends here
