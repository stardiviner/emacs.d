;;; init-tool-porn.el --- init for Porn -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-12-08 17:27:13 stardiviner>

;;; Commentary:



;;; Code:

;;; [ search & replace some words ]

(defun porn/reverse-anti-censorship-words ()
  "Reverse some anti-censorship words."
  (interactive)
  ;; (require 'pcre2el)
  ;; ‘pcre-query-replace-regexp’
  (query-replace-regexp "rb" "肉棒")
  (query-replace-regexp "yj" "阴茎")
  (query-replace-regexp "yx" "淫穴")
  (query-replace-regexp "jy" "精液")
  (query-replace-regexp "g头" "龟头")
  (query-replace-regexp "y道" "阴道")
  (query-replace-regexp "y核" "阴核"))



(provide 'init-tool-porn)

;;; init-tool-porn.el ends here
