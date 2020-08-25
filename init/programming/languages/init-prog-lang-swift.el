;;; init-prog-lang-swift.el --- init for Swift
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swift-mode ]

(use-package swift-mode
  :ensure t
  :defer t
  :commands (run-swift)
  :config
  (cl-case system-type
    ('darwin
     (setq swift-mode:repl-executable
           (concat (when (executable-find "xcrun") "xcrun ") "swift")))
    ('gnu/linux
     (setq swift-mode:repl-executable "docker run --rm --privileged -it swift swift")
     (defun swift-mode-docker-attach-repl (cmd &optional dont-switch keep-default)
       (docker-container-attach "swift" nil)
       (rename-buffer "*swift*"))
     (advice-add 'run-swift :after #'swift-mode-docker-attach-repl))))

;; [ ob-swift ] -- org-babel functions for swift evaluation.

(use-package ob-swift
  ;; :ensure t
  ;; :quelpa (ob-swift :fetcher github :repo "stardiviner/ob-swift")
  :load-path "~/Code/Emacs/ob-swift"
  :defer t
  :commands (org-babel-execute:swift)
  :config
  (add-to-list 'org-babel-load-languages '(swift . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("swift" . "swift")))

;;; [ swift-playground ] -- Emacs support for Swift playgrounds.

(use-package swift-playground-mode
  :ensure t
  :defer t
  :commands (swift-playground-run)
  :hook (swift-mode-hook . swift-playground-global-mode))

;; [ company-sourcekit ]

(use-package company-sourcekit
  :ensure t
  :defer t
  :init ; (setq sourcekit-sourcekitdaemon-executable "")
  (defun my/company-sourcekit-setup ()
    (my-company-add-backend-locally 'company-sourcekit))
  (add-hook 'swift-mode-hook #'my/company-sourcekit-setup)
  :config
  (setq company-sourcekit-use-yasnippet t)
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
                 '(swift-mode
                   "true" "false" "nil" "available" "column" "elseif" "else" "endif" "file" "function" "if" "line" "selector" "associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "func" "import" "init" "inout" "internal" "let" "open" "operator" "private" "protocol" "public" "static" "struct" "subscript" "typealias" "var" "break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for" "guard" "if" "in" "repeat" "return" "switch" "where" "while" "as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws" "throw" "try" "Protocol" "Type" "and" "assignment" "associativity" "convenience" "didSet" "dynamic" "final" "get" "higherThan" "indirect" "infix" "lazy" "left" "lowerThan" "mutating" "none" "nonmutating" "optional" "override" "postfix" "precedence" "precedencegroup" "prefix" "required" "right" "set" "unowned" "weak" "willSet"))))

;; [ flycheck-swift ] -- Flycheck extension for Apple's Swift.

(use-package flycheck-swift
  :ensure t
  :defer t
  :hook (swift-mode-hook . flycheck-swift-setup))

;;; [ swift3-mode ] -- major-mode for Apple's Swift programming language.

;; (use-package swift3-mode
;;   :ensure t
;;   :defer t)


(provide 'init-prog-lang-swift)

;;; init-prog-lang-swift.el ends here
