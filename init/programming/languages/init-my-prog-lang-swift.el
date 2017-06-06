;;; init-my-prog-lang-swift.el --- init for Swift
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swift-mode ]

(use-package swift-mode
  :ensure t
  :config
  (setq swift-indent-offset 4
        swift-indent-switch-case-offset 2
        swift-indent-multiline-statement-offset 2
        swift-indent-hanging-comma-offset nil)
  (setq swift-repl-executable "swift")
  ;; flycheck + swift-mode
  (add-to-list 'flycheck-checkers 'swift)
  ;; Swift flycheck is disabled by default because not available under Linux.
  (setq flycheck-swift-sdk-path "")
  )


;;; [ swift3-mode ] -- major-mode for Apple's Swift programming language.

;; (use-package swift3-mode
;;   :ensure t)


;;; [ company-sourcekit ]

(use-package company-sourcekit
  :ensure t
  :config
  (setq company-sourcekit-use-yasnippet t
        company-sourcekit-verbose nil
        ;; sourcekit-sourcekitdaemon-executable
        )

  (add-hook 'swift-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-sourcekit)
              ))
  (eval-after-load 'company-keywords
    '(add-to-list 'company-keywords-alist
                  '(swift-mode "true" "false" "nil" "available" "column" "elseif" "else" "endif" "file" "function" "if" "line" "selector" "associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "func" "import" "init" "inout" "internal" "let" "open" "operator" "private" "protocol" "public" "static" "struct" "subscript" "typealias" "var" "break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for" "guard" "if" "in" "repeat" "return" "switch" "where" "while" "as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws" "throw" "try" "Protocol" "Type" "and" "assignment" "associativity" "convenience" "didSet" "dynamic" "final" "get" "higherThan" "indirect" "infix" "lazy" "left" "lowerThan" "mutating" "none" "nonmutating" "optional" "override" "postfix" "precedence" "precedencegroup" "prefix" "required" "right" "set" "unowned" "weak" "willSet")))
  )


;;; [ flycheck-swift ] -- Flycheck extension for Apple's Swift.

(use-package flycheck-swift
  :ensure t)


;;; [ ob-swift ]

(use-package ob-swift
  :ensure t)


(provide 'init-my-prog-lang-swift)

;;; init-my-prog-lang-swift.el ends here
