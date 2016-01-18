;;; init-my-prog-framework-android.el --- init for Android Development
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;;_ Code:

;;;_* [ android-mode ] -- Minor mode for Android application development

;;;_ + Usage:
;; Provides support for running Android SDK subprocesses like the emulator,
;; logcat, ddms and ant. When loaded `dired-mode' and `find-file' hooks are
;; added to automatically enable `android-mode' when opening a file or directory
;; in an android project.

(use-package android-mode
  :ensure t
  )


(provide 'init-my-prog-framework-android)

;;; init-my-prog-framework-android.el ends here
