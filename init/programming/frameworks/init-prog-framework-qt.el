;;; init-prog-framework-qt.el --- init for Qt
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

(c-add-style
 "qt-gnu"
 '("gnu"
   (c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
   (c-basic-offset . 4)))

;;; [ C++ extra settings ]

(require 'cc-mode)

(use-package modern-cpp-font-lock
  :ensure t)

;;; [ Integration within Emacs ]

;;; [ Completion for Qt ]

;;; [ Qt .pro ]

(use-package qt-pro-mode
  :ensure t
  :mode ("\\.pro\\'" "\\.pri\\'"))

;;; [ QML-mode ]

(use-package qml-mode
  :ensure t
  :defer t
  :init
  (use-package company-qml
    :ensure t
    :init
    (add-hook 'qml-mode-hook
              (lambda ()
                (setq-local company-minimum-prefix-length 0)
                (add-to-list (make-local-variable 'company-backends) 'company-qml)))))

;;; [ qmake-mode ]

(load (concat user-emacs-directory "init/extensions/qmake.el"))
(require 'qmake-mode)
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qmake-mode))

(use-package qt-pro-mode
  :ensure t
  :defer t
  :mode ("\\.pro\\'" "\\.pri\\'"))


(provide 'init-prog-framework-qt)

;;; init-prog-framework-qt.el ends here
