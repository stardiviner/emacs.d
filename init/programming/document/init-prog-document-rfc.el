;;; init-prog-document-rfc.el --- init for RFC
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ irfc ] -- Interface for IETF RFC document.

;; (use-package irfc
;;   :ensure t
;;   :commands (irfc-visit)
;;   :bind (:map document-prefix ("r" . irfc-visit))
;;   :init (setq irfc-directory (expand-file-name "documentations/RFC" user-emacs-directory))
;;   (setq irfc-assoc-mode t)        ; RFC documents are associated with `irfc-mode'.
;;   (setq irfc-highlight-requirement-keywords t
;;         irfc-requirement-keywords '("MUST" "MUST NOT" "REQUIRED"
;;                                     "SHALL" "SHALL NOT" "SHOULD" "SHOULD NOT"
;;                                     "RECOMMENDED" "NOT RECOMMENDED"
;;                                     "MAY" "OPTIONAL" "NOT")
;;         irfc-requirement-keyword-overlay nil
;;         irfc-highlight-references t))

;;; [ rfc-mode ] -- RFC document browser and viewer.

(use-package rfc-mode
  :ensure t
  :defer t
  :commands (rfc-mode-browse rfc-mode-read)
  :bind (:map document-prefix ("r" . rfc-mode-browse))
  :init (setq rfc-mode-directory (expand-file-name "documentations/RFC/" user-emacs-directory)))


(provide 'init-prog-document-rfc)

;;; init-prog-document-rfc.el ends here
