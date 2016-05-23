;;; init-my-prog-framework-qt.el --- init for Qt
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Qt mode ]

;; qt keywords and stuff ...
;; set up indenting correctly for new qt keywords:
(setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                               "\\|protected slot\\|private\\|private slot"
                               "\\)\\>")
      c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                               "\\|public slots\\|protected slots\\|private slots"
                               "\\)\\>[ \t]*:"))

(defun c++-mode-qt-keywords-highlight ()
  ;; Modify the colour of slots to match public, private, etc ...
  (font-lock-add-keywords 'c++-mode
                          '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
  ;; make new font for rest of Qt keywords
  (make-face 'qt-keywords-face)
  (set-face-foreground 'qt-keywords-face "blue violet")
  
  ;; Qt keywords
  (font-lock-add-keywords 'c++-mode
                          '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))

  (font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))
  )

(add-hook 'c++-mode-hook 'c++-mode-qt-keywords-highlight)

;; (load-library "cc-mode")
;; (require 'cc-mode)
;; (when (locate-library "cc-mode")
;;   (setq c-font-lock-keywords-3
;;         (append '("signals" "\\(public\\|protected\\|private\\) slots")
;;                 c-font-lock-keywords-3)))

(c-add-style "qt-gnu" '("gnu"
                        (c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
                        (c-basic-offset . 4)))


;; (require 'cc-mode)
;; (setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
;; (font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))


;;; Integration within Emacs

;; You can automaticaly enable CcMode for Qt source files, extend the path where
;; specific files are searched, and feed the SemanticBovinator (note that the
;; variable ‘qt-base-directory’ is defined in the previous code snippet):

;; (require 'semantic/bovine/c)

;; (setq qt-base-directory "/usr/share/qt")

;; (setq qt-source-directory (expand-file-name "QtSources/4.7.3/src"
;;                                             qt-base-directory)
;;       qt-include-directory (expand-file-name "Desktop/Qt/4.7.3/mingw/include/"
;;                                              qt-base-directory))
;; (add-to-list 'auto-mode-alist (cons qt-source-directory 'c++-mode))
;; (add-to-list 'cc-search-directories qt-source-directory)

;; (add-to-list 'auto-mode-alist (cons qt-include-directory 'c++-mode))
;; (dolist (file (directory-files qt-include-directory))
;;   (let ((path (expand-file-name file qt-include-directory)))
;;     (when (and (file-directory-p path)
;;                (not (or (equal file ".") (equal file ".."))))
;;       (progn
;;         (semantic-add-system-include path 'c++-mode)
;;         (add-to-list 'cc-search-directories path)))))

;; (dolist (file (list "QtCore/qconfig.h" "QtCore/qconfig-dist.h" "QtCore/qconfig-large.h"
;;                     "QtCore/qconfig-medium.h" "QtCore/qconfig-minimal.h" "QtCore/qconfig-small.h"
;;                     "QtCore/qglobal.h"))
;;   (add-to-list 'semantic-lex-c-preprocessor-symbol-file (expand-file-name file qt-include-directory)))


;;; [ Completion for Qt ]

(require 'cc-mode)


;;; [ QML-mode ]

(use-package qml-mode
  :ensure t)


;;; [ company-qml ]

(use-package company-qml
  :ensure t
  :config
  (add-hook 'qml-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0)
              (add-to-list (make-local-variable 'company-backends) 'company-qml)
              ))
  )


;;; [ qmake-mode ]

(load (concat user-emacs-directory "init/extensions/qmake.el"))

(require 'qmake-mode)
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qmake-mode))


(provide 'init-my-prog-framework-qt)

;;; init-my-prog-framework-qt.el ends here
