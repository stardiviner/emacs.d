;;; init-prog-sense.el --- init IntelliSense & CodeSense for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ CEDET ] -- (C)ollection of (E)macs (D)evelopment (E)nvironment (T)ools

;; (require 'init-prog-cedet)


;;; [ Sourcegraph: srclib ] -- A hackable, multi-language code analysis library for building better software tools.

;;; Usage:
;;
;; - [M-x sourcegraph-mode] :: enable sourcegraph-mode for buffer.
;; - [C-M-.] / `sourcegraph-describe' :: see docs, type info, and examples.
;;   - `sourcegraph-describe-show-examples' ::
;; - `sourcegraph-analyze' :: analyze code.
;; - `sourcegraph-jump-to-def' :: jump to definition.
;; - `sourcegraph-search-site'

;; (use-package sourcegraph
;;   :ensure t
;;   :init
;;   ;; enable sourcegraph-mode minor mode in all programming modes repository.
;;   (add-hook 'prog-mode-hook 'sourcegraph-mode)
;;   )


(provide 'init-prog-sense)

;;; init-prog-sense.el ends here
