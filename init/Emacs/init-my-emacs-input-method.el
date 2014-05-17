;;; init-my-emacs-input-method.el --- init Input Method
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Chinese Input Method ]




;;; [ EIM ] -- Emacs Input Method (for Chinese input)

;;; Features:
;;
;; ** 五笔
;;  1. 临时拼音输入汉字。用 z 开头可以输入汉字的拼音并查看其五笔字码。
;;  2. 反查五笔。用 M-x eim-describe-char 可以查看光标处汉字的五笔字码。
;;  3. 加入自造词。M-x eim-table-add-word，默认是光标前的两个汉字。用 C-a 和 C-e 调整。
;;  4. 可以保存选择的历史。
;;
;; ** 拼音
;;  1. 自动调频
;;  2. 自动加入输入的词组。
;;  3. 不必输入词组的全部拼音，比较智能的查找词组。

;;; Usage:
;;
;; 注意，如果你的 eim-wb.el 和 eim-py.el 不在 ~/.emacs.d/site-lisp/eim 目录下，
;; 记住修改 wb.txt 和 py.txt 中相应文件 的位置。五笔输入法还要修改
;; eim-wb-history-file 和 eim-wb-user-file 的 位置。
;;
;; - [C-h I] / [M-x describe-input-method] :: check out input method help.
;;
;; - C-n :: scroll down page
;; - C-p :: scroll up page
;; - C-c :: cancel input
;; - SPC :: confirm input
;; - RET :: input.
;;
;; - ' :: separate pinyin.
;; - ; :: input English
;;
;; ("M-o" . eim-delete-last-char) ;delete last char
;; ("M-c" . eim-quit-clear)       ;quit and clear
;; ("M-m" . eim-quit-no-clear)    ;quit and no clear
;; ("M-n" . eim-next-page)        ;page down
;; ("M-p" . eim-previous-page)    ;page up

(add-to-list 'load-path "~/.emacs.d/init/extensions/eim.el")
(autoload 'eim-use-package "eim" "Another emacs input method")

(register-input-method "eim-py" "euc-cn" 'eim-use-package
                       "拼音" "汉字拼音输入法" "~/.emacs.d/site-lisp/eim/py.txt")
(register-input-method "eim-wb" "euc-cn" 'eim-use-package
                       "五笔" "汉字五笔输入法" "~/.emacs.d/site-lisp/eim/wb.txt")


(provide 'init-my-emacs-input-method)

;;; init-my-emacs-input-method.el ends here
