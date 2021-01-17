;;; helm-baidu-fanyi-suggest.el --- 

;;; Commentary:



;;; Code:

(require 'helm)

(defun helm-baidu-fanyi-suggest-fetch (keyword)
  (let ((url-user-agent (format "%s <%s>" user-full-name user-mail-address))
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (encode-coding-string
                           (mapconcat
                            (pcase-lambda (`(,key . ,val))
                              (concat (url-hexify-string key)
                                      "="
                                      (url-hexify-string val)))
                            (list (cons "kw" keyword))
                            "&")
                           'utf-8)))
    (with-current-buffer (url-retrieve-synchronously "https://fanyi.baidu.com/sug")
      ;; 百度使用 \uxxxx，而不是 UTF-8
      ;; http://softwaremaniacs.org/blog/2015/03/22/json-encoding-problem/en/
      ;; (set-buffer-multibyte t)
      (goto-char url-http-end-of-headers)
      (let ((json (let ((json-array-type 'list))
                    (json-read))))
        (mapcar
         (lambda (x)
           (let-alist x
             (cons .k .v)))
         (alist-get 'data json))))))

(defun helm-baidu-fanyi-suggest-candidates (&optional keyword)
  (mapcar
   (pcase-lambda (`(,word . ,meaning))
     (format "%-20s %s" word
             ;; 有时开头会有空格
             (string-trim meaning)))
   (helm-baidu-fanyi-suggest-fetch (or keyword helm-pattern))))

(defvar helm-baidu-fanyi-suggest-action
  (helm-make-actions
   "Insert Query"
   (lambda (candidate)
     ;; NOTE 单词和解释之间至少间隔 2 个空格
     (insert (car (split-string candidate "  " t))))
   "Browse URL"
   (lambda (candidate)
     (let* ((query (car (split-string candidate "  " t)))
            ;; NOTE 只考虑中文 ⇔ 英文
            ;; https://fanyi.baidu.com/#en/zh/aggressive
            ;; https://fanyi.baidu.com/#zh/en/%E4%B8%AD%E5%9B%BD
            (from (if (string-match-p "\\cC" query) 'zh 'en))
            (to (pcase from
                  ('zh 'en)
                  ('en 'zh))))
       (browse-url
        (format "https://fanyi.baidu.com/#%s/%s/%s"
                from to (url-hexify-string query)))))))

(defun helm-baidu-fanyi-suggest ()
  "百度翻译（搜索补全）."
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "百度翻译"
     :header-name
     (lambda (name)
       (format "%s <%s>" name "https://fanyi.baidu.com/"))
     :candidates #'helm-baidu-fanyi-suggest-candidates
     :action helm-baidu-fanyi-suggest-action
     :volatile t
     :requires-pattern 1)
   :buffer "*helm 百度翻译*"))




(provide 'helm-baidu-fanyi-suggest)

;;; helm-baidu-fanyi-suggest.el ends here
