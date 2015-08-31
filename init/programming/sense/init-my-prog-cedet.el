;;; init-my-prog-cedet.el --- a Collection of Emacs Development Environment Tools.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; which written with the end goal of creating an advanced development environment in Emacs.
;; http://cedet.sourceforge.net/
;;


;;; Code:

;;; [ CEDET ] --- a Collection of Emacs Development Environment Tools

;; load latest user CEDET instead of Emacs default built-in CEDET.
;; (load-file (concat user-emacs-directory "el-get/cedet/cedet-devel-load.el"))

;; (require 'cedet)


;;; [ Semantic ]

(require 'semantic/senator)
(require 'semantic/ia)
(require 'semantic/analyze/refs)
(require 'semantic/analyze/complete)
(require 'semantic/bovine/gcc)
(require 'semantic/mru-bookmark)

(require 'semantic)


;;; minimum setup
;; (global-semantic-idle-scheduler-mode 1)
;; (global-semanticdb-minor-mode 1)

;; or
;; (add-hook 'semantic-init-hooks 'semantic-idle-completions-mode)


(global-semantic-idle-scheduler-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semanticdb-minor-mode 1)
;; FIXME: (global-cedet-m3-minor-mode 1)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(global-semantic-show-unmatched-syntax-mode t)


;; (autoload 'semantic-mode "semantic mode" nil t)
;; (semantic-mode t) ; this should be placed *BEFORE* the statements which activate ECB.

;; I already put this in init-my-prog-ecb.el
;; alternative: This is a smarter way when you need semantic only if ECB is active.
;; (add-hook 'ecb-before-activate-hook
;;           (lambda () (semantic-mode 1)))


;;; [ Non-semantic files ]
;; Such non-semantic file-types can often be parsed by imenu and/or etags.
;; (setq speedbar-dynamic-tags-function-list)
;; (setq speedbar-tag-split-minimum-length)
;; (setq speedbar-tag-regroup-maximum-length)
;; (setq speedbar-tag-hierarchy-method)



;;; Semantic (code-parsing, smart completion) features

;;; select one of the following
;; (semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers)
;; (semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; (semantic-load-enable-semantic-debugging-helpers)

;;; enable source code function body folding
;; (global-semantic-tag-folding-mode 1)

;;; Key bindings
;; (defun my-cedet-hook ()
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "/C-c ?" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "/C-c d" 'semantic-ia-fast-jump)
;;   (local-set-key "/C-c r" 'semantic-symref-symbol)
;;   (local-set-key "/C-c R" 'semantic-symref))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)

;;; 当输入"."或">"时，在另一个窗口中列出结构体或类的成员
;; (defun my-c-mode-cedet-hook ()
;;   (local-set-key "." 'semantic-complete-self-insert)
;;   (local-set-key ">" 'semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)


;;; semantic的补全基于编程语义，emacs自带的hippie-expand则提供更为强大的编辑补全功能。这里，直接将曹乐的配置内容添加到.emacs文件中。
;;; 自动补全策略
;; (defun my-indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "//>")
;;       (hippie-expand nil)
;;     (indent-for-tab-command))
;;   )

;; (global-set-key [(control tab)] 'my-indent-or-complete)

;; (autoload 'senator-try-expand-semantic "senator")
;; (setq hippie-expand-try-functions-list
;;       '(senator-try-expand-semantic
;;         try-expand-dabbrev
;;         try-expand-dabbrev-visible
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-expand-list
;;         try-expand-list-all-buffers
;;         try-expand-line
;;         try-expand-line-all-buffers
;;         try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-whole-kill
;;         )
;;       )



;; [ Semantic ] -- a base for construction of syntactic analyzers for different programming languages.
;; Usage:
;; - [M-x semantic-mode] -- use Semantic
;; - [C-c , j] -- (semantic-complete-jump-local)
;;   -- prompt for the name of a function defined in the current file, and move point there.
;; - [C-c , J] -- (semantic-complete-jump)
;;   -- prompt for the name of a function defined in any file Emacs has parsed, and move point there.
;; - [C-c , <SPC>] -- (semantic-complete-analyze-inline)
;;   -- display a list of possible completions for the symbol at point.
;; - [C-c , l] -- (semantic-analyze-possible-completions)
;;   -- display a list of the possible completions of the symbol at point, in another window

;; check out include path and cpp macro settings
;;   - [M-x semantic-c-describe-environment RET]
;; check out GNU Global settings
;;   - [M-x semanticdb-test-gnu-global RET printf RET]

;; Semantic modes
;; - [C-h f semantic-mode RET]
;; a variety of auxiliary minor modes, listed in `semantic-default-submodes';

;; (setq semantic-default-submodes
;;       '(global-semanticdb-minor-mode ; enables global support for Semanticdb;
;;         global-semantic-idle-scheduler-mode ; activate auto parsing of source code in the idle time;
;;         ;; global-semantic-idle-completions-mode ; activate displaying of possible name completions in the idle time;
;;         ;; global-semantic-idle-summary-mode ; activate displaying of info about current tag in the idle time.
;;         ;; global-semantic-idle-local-symbol-highlight-mode ; activate highlighting of local names that are the same as name of tag under cursor;
;;         ;; global-semantic-idle-breadcrumbs-mode ;
;;         ;; global-semantic-highlight-func-mode ; activate highlighting of first line for current tag (function, class, etc.);
;;         ;; global-semantic-stickyfunc-mode ; activate mode when name of current tag will be shown in top line of buffer;
;;         global-semantic-decoration-mode ; activate use of separate styles for tags decoration (depend on tags' class).
;;         global-semantic-mru-bookmark-mode ; enable auto bookmarking of tags that you
;;         ;; global-cedet-m3-minor-mode ; activate CEDET's context menu that is bound to right mouse button.
;;
;;         ;; following sub-modes are usually useful when you develop and/or debug CEDET:
;;         ;; global-semantic-show-unmatched-syntax-mode ; show which elements weren't processed by current parser's rules.
;;         ;; global-semantic-show-parser-state-mode ; show current parser state in the modeline.
;;         ;; global-semantic-highlight-edits-mode ; show changes in the text that weren't processed by incremental parser yet.
;;         ))

;; (unless (featurep 'cedet)
;;   (when (file-directory-p "~/.emacs.d/el-get/cedet")
;;     (progn
;;       (add-to-list 'load-path  "~/.emacs.d/el-get/cedet")
;;       (load-file "~/.emacs.d/el-get/cedet/cedet-devel-load.el"))))


;;; [ EDE ]

(setq semantic-default-submodes
      '(;; cache(?)
        global-semanticdb-minor-mode

        global-semantic-highlight-edits-mode
        global-semantic-idle-local-symbol-highlight-mode
        ;; global-cedet-m3-minor-mode

        ;; code helpers
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-completions-mode

        ;; eye candy
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-highlight-edits-mode
        global-semantic-stickyfunc-mode

        ;; debugging semantic itself
        ;;global-semantic-show-parser-state-mode 1   ;; show the parsing state in the mode line
        ;;global-semantic-show-unmatched-syntax-mode 1
        ))

;; to enable more advanced functionality for name completion, etc.
;; (require 'semantic/ia)

;; System header files
;; to normal work with system-wide libraries, Semantic should has access to system include files.
;; (require 'semantic/bovine/gcc)
;; manually
;; (semantic-add-system-include "~/exp/include/boost" 'c++-mode)

;; Using EDE for Java projects
;; (require 'semantic/db-javap)

;; (semantic-load-enable-code-helpers) ; enable prototype help and smart completion

;; TODO (semantic-mode t)


;;; [ SemanticDB ] -- is included into Semantic, and implements different storage modules, that store information, needed for names completion, source code navigation, etc.

(setq semanticdb-default-save-directory (concat user-emacs-directory "semanticdb"))

;; auto load index database of system /usr/include/.
(setq semanticdb-search-system-databases t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq semanticdb-project-system-databases
                  (list (semanticdb-create-database
                         semanticdb-new-database-class
                         "/usr/include")))))


;;; [ Senator ] -- implements navigation in source code using information extracted by Semantic.


;;; [ Srecode ] -- a package for source code generation using syntactic information


;;; [ EDE ] -- implements a set of extensions to work with projects.
;; TODO (global-ede-mode 1) ; enable the project management system


;;; [ Speedbar ] -- is used to display information about current buffer using different sources of information.


;;; [ Eieio ] -- a library implements CLOS-like (Common Lisp Object System) infrastructure for Emacs Lisp.


;;; [ Cogre ] -- a library for generation of UML-like diagrams in Emacs buffer which basic integration with Semantic.



(require 'semantic-tag-folding nil 'noerror)



;;; [ eassist ]

(require 'eassist nil 'noerror)

;; Contains some useful functions features for C/C++ developers similar to
;; those from VisualAssist.  Remember that convenient M-o, M-g and M-m?

;; 1) Method navigation. [M-m]
;;    When eassist-list-methods called when c/c++ body file buffer is active
;;    a new buffer is shown, containing list of methods and functions in the
;;    format: return type, class, method name.  You can select the method
;;    moving to its line and press ENTER to jump to the method.  You also can
;;    type a string in the buffer and method list will be reduced to those
;;    which contain the string as a substring.  Nice highlight is implemented.
;;    This function is recommended to be bound to M-m in c-mode.

;; 2) Header <-> Body file switch. [M-o]
;;    You can easily switch between body (c, cpp, cc...) and its corresponding
;;    header file (h, hpp...) using eassist-switch-h-cpp.  The counterpart file
;;    is first searched in opened buffers and if there is no match the file is
;;    searched in the same directory.  You can adjust body to header correspondence
;;    customizing eassist-header-switches variable.
;;    This function is recommended to be bound to M-o in c-mode.

;; EmacsAssist uses Semantic (http://cedet.sourceforge.net/semantic.shtml)
;; EmacsAssist is a part of CEDET project (current CVS version of CEDET contains
;; EmacsAssist)
;; EmacsAssist works with current (22) and development (23) versions of Emacs and
;; does not work with version 21.
;; EmacsAssist works with CEDET 1.0pre4 and subsequent CVS versions of CEDET.

;; EmacsAssist has a page at Emacs Wiki, where you can always find the latest
;; version: http://www.emacswiki.org/cgi-bin/wiki/EAssist

;; Usage:

;; 1) Install CEDET package for Emacs (if you don't have CEDET already).
;; 2) Add convenient keymaps for fast EmacsAssist calls in c-mode and (or) python-mode
;;    and for lisp:
;;
;;    (defun my-c-mode-common-hook ()
;;      (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
;;      (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
;;    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;
;;    (defun my-python-mode-hook ()
;;      (define-key python-mode-map (kbd "M-m") 'eassist-list-methods))
;;    (add-hook 'python-mode-hook 'my-python-mode-hook)
;;
;;    (define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods)
;;
;; 3) Open any C++ file with class definition, press M-m.  Try to type
;;    any method name.
;; 4) Open any .cpp file.  Press M-o.  If there is .h or .hpp file in the
;;    same folder, it will be opened.

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
            (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "M-m") 'eassist-list-methods)))

(define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods)



(provide 'init-my-prog-cedet)

;;; init-my-prog-cedet.el ends here
