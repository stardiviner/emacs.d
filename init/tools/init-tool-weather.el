;;; init-tool-weather.el --- Weather forecast tools.

;;; Commentary:



;;; Code:

;;; [ forecast ] -- weather forecast.

(use-package forecast
  :ensure t
  :defer t
  :commands (forecast)
  :config (setq forecast-api-key (my/json-read-value my/account-file 'forecast)))




(provide 'init-tool-weather)

;;; init-tool-weather.el ends here
