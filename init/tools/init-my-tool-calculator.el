;;; init-my-tool-calculator.el --- init Tool Calculator
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Calc ]

;;; Usage:
;;
;; - [' 1+1 RET] :: Algebraic style.
;; - [1 RET 3 +] :: RPN style.
;; - [C-x * 0] :: calc-reset.
;;
;; GET HELP
;; - get help -> [C-h i g (calc)]
;; - in Calculator window.
;;   - [C-h m] :: mode help.
;;   - [h i] :: go to info.
;;   - [h ?] :: get short help
;;   - [h ? ?] :: more help.
;;   - [h s] :: summary of help.
;;   - [h t] :: tutorial of help.
;; - [C-x *] :: prefix
;; - [C-x * *] ::
;; ELISP> (calc-eval "1+2")
;;
;; - Plot
;;   - [g p] :: graph-plot.

(setq calc-complex-format 'i            ; complex number style: x + yi.
      )

(define-key my-tools-prefix-map (kbd "c") 'calc)


(provide 'init-my-tool-calculator)

;;; init-my-tool-calculator.el ends here
