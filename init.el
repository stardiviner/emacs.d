;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-verbose t)

;;; extra sources which not in el-get recipes.
(setq el-get-extra-sources
      '(
	;; (:name pretty-symbols
        ;;        :description "Emacs minor mode for drawing multi-character tokens as Unicode glyphs (lambda -> λ). A configurable replacement for pretty and the like."
        ;;        :type git
        ;;        :url "https://github.com/drothlis/pretty-symbols")
	))

;;; my packages which will be installed.
(setq my-el-get-packages
      (append
        '(popup
           pos-tip
           popup-pos-tip
           showtip
           tooltip-help
           )
        (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my-el-get-packages)
