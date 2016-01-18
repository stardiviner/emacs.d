;;; init-my-prog-document-rfc.el --- init for RFC
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ irfc ]

(use-package irfc
  :ensure t
  :config
  (setq irfc-directory (concat user-emacs-directory "documentations/RFC")) ; set RFC database storage.
  (setq irfc-assoc-mode t)        ; RFC documents are associated with `irfc-mode'.

  (setq irfc-highlight-requirement-keywords t
        irfc-requirement-keywords '("MUST" "MUST NOT" "REQUIRED"
                                    "SHALL" "SHALL NOT" "SHOULD" "SHOULD NOT"
                                    "RECOMMENDED" "NOT RECOMMENDED"
                                    "MAY" "OPTIONAL" "NOT")
        irfc-requirement-keyword-overlay nil
        irfc-highlight-references t)

  (define-key my-prog-help-document-map (kbd "r") 'irfc-visit)
  )


;;; [ rfcview ]

(use-package rfcview
  ;; :ensure t
  ;; :config
  ;; (setq rfcview-index-location (concat user-emacs-directory "documentations/RFC/rfc-index.txt")
  ;;       rfcview-rfc-location-pattern (concat user-emacs-directory "documentations/RFC/rfc%s.txt")
  ;;       rfcview-std-location-pattern (concat user-emacs-directory "documentations/RFC/std%s.txt")
  ;;       )

  ;; (define-key rfcview-mode-map (kbd "q") 'rfcview-quit)
  ;; (define-key rfcview-mode-map (kbd "j") 'rfcview-find-rfc)
  ;; (define-key rfcview-mode-map (kbd "i") 'rfcview-textmode)
  ;; (define-key rfcview-mode-map (kbd "g") 'rfcview-goto-link)
  ;; (define-key rfcview-mode-map (kbd "I") 'rfcview-find-index)
  ;; (define-key rfcview-mode-map (kbd "TAB") 'rfcview-next-button)
  ;; (define-key rfcview-mode-map (kbd "t") 'rfcview-hyperlink-contents)
  ;; (define-key rfcview-mode-map (kbd "s") 'rfcview-find-location-of-rfc)
  ;; (define-key rfcview-mode-map (kbd "d") 'rfcview-imenu-index-function)
  )


(provide 'init-my-prog-document-rfc)

;;; init-my-prog-document-rfc.el ends here
