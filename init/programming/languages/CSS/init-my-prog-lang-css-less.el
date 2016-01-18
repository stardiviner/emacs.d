;;; init-my-prog-lang-css-less.el --- init for LESS CSS
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ less-css-mode ] -- Emacs support for LESS CSS (lesscss.org)

;;; Usage:
;;
;; `less-css-mode'


;;; [ skewer-less ] -- Emacs minor mode allowing LESS stylesheet manipulation via skewer-mode.
;;
;;; make your browser magically reload your LESS stylesheets
;;
;;; Usage:
;;
;; - `skewer-mode'
;;
;; Note that this is intended for use in place of skewer-css-mode, which does not work with LESS.
;;
;; Operates by invoking less.refresh() via skewer on demand, or whenever the buffer is saved.
;;
;; For this to work properly, the less javascript should be included in the
;; target web page, and less should be configured in development mode, e.g.
;;
;;    #+BEGIN_SRC html
;;    <script>
;;      var less = {env: "development"};
;;    </script>
;;    <link href="/stylesheets/application.less" rel="stylesheet/less">
;;    <script src="/path/to/less.js" type="text/javascript"></script>
;;    #+END_SRC
;;
;;I may consider providing an option to instead run lessc from Emacs, then send
;;the output via skewer-css. Let me know if you want this.

(use-package skewer-less
  ;; :ensure t
  ;; :config
  ;; (skewer-less-mode)
  )


(provide 'init-my-prog-lang-css-less)

;;; init-my-prog-lang-css-less.el ends here
