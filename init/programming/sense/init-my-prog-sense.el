;;; init-my-prog-sense.el --- init IntelliSense & CodeSense for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ CEDET ]

;; (require 'init-my-prog-cedet)

;; (global-ede-mode t)


;;; [ Sourcegraph: srclib ] -- A hackable, multi-language code analysis library for building better software tools.

;;; Features
;;
;;   - Documentation lookups
;;   - Type information
;;   - Find usages (across all open-source projects globally)

;;; Usage:
;;
;; - [M-x sourcegraph-mode] :: enable sourcegraph-mode for buffer.
;; - [C-M-.] / `sourcegraph-describe' :: see docs, type info, and examples.
;;   - `sourcegraph-describe-show-examples' ::
;; - `sourcegraph-analyze' :: analyze code.
;; - `sourcegraph-jump-to-def' :: jump to definition.
;; - `sourcegraph-search-site'

(use-package sourcegraph
  :config
  ;; enable sourcegraph-mode minor mode in all programming modes repository.
  (add-hook 'prog-mode-hook 'sourcegraph-mode)
  )



(provide 'init-my-prog-sense)

;;; init-my-prog-sense.el ends here
