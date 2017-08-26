;;; init-my-prog-lang-swift.el --- init for Swift
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swift-mode ]

(use-package swift-mode
  :ensure t)


;;; [ swift3-mode ] -- major-mode for Apple's Swift programming language.

;; (use-package swift3-mode
;;   :ensure t)


;;; [ company-sourcekit ]

(use-package company-sourcekit
  :ensure t
  :config
  (setq company-sourcekit-use-yasnippet t
        ;; sourcekit-sourcekitdaemon-executable

        ;; for DEBUG:
        ;; company-sourcekit-verbose t
        ;; sourcekit-verbose t
        )

  (add-hook 'swift-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-sourcekit)
              ))
  
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
                 '(swift-mode
                   "true" "false" "nil" "available" "column" "elseif" "else" "endif" "file" "function" "if" "line" "selector" "associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "func" "import" "init" "inout" "internal" "let" "open" "operator" "private" "protocol" "public" "static" "struct" "subscript" "typealias" "var" "break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for" "guard" "if" "in" "repeat" "return" "switch" "where" "while" "as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws" "throw" "try" "Protocol" "Type" "and" "assignment" "associativity" "convenience" "didSet" "dynamic" "final" "get" "higherThan" "indirect" "infix" "lazy" "left" "lowerThan" "mutating" "none" "nonmutating" "optional" "override" "postfix" "precedence" "precedencegroup" "prefix" "required" "right" "set" "unowned" "weak" "willSet")))
  )


;;; [ flycheck-swift ] -- Flycheck extension for Apple's Swift.

(use-package flycheck-swift
  :ensure t)


;;; [ ob-swift ]

(use-package ob-swift
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(swift . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("swift" . "swift"))
  )


(provide 'init-my-prog-lang-swift)

;;; init-my-prog-lang-swift.el ends here
