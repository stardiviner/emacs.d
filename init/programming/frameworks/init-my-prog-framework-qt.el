;;; init-my-prog-framework-qt.el --- init for Qt
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Qt mode ]




;;; [ QML-mode ]

(use-package qml-mode
  :ensure t)


;;; [ company-qml ]

(use-package company-qml
  :ensure t
  :init
  (setq qmltypes-parser-file-list
        '("/usr/lib/qt/qml/QtQuick/Controls/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/Dialogs/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/Extras/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/Layouts/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/LocalStorage/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/Particles.2/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/PrivateWidgets/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/Window.2/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick/XmlListModel/plugins.qmltypes"
          "/usr/lib/qt/qml/QtQuick.2/plugins.qmltypes"))

  :config
  (remove-hook 'qml-mode-hook
               (lambda ()
                 ;; (add-to-list 'company-backends 'company-qml)
                 (my-company-add-backends-to-mode '(company-qml))
                 ))
  )


(provide 'init-my-prog-framework-qt)

;;; init-my-prog-framework-qt.el ends here
