;;; This file is introduced in Emacs 27.1

;;; Emacs can now be configured using an early init file.
;;;
;;; The file is called 'early-init.el', in 'user-emacs-directory'.  It is
;;; loaded very early in the startup process: before graphical elements
;;; such as the tool bar are initialized, and before the package manager
;;; is initialized.  The primary purpose is to allow customizing how the
;;; package system is initialized given that initialization now happens
;;; before loading the regular init file (see below).

;;; Emacs GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
