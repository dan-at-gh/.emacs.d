;;; text-custom --- Text (all markup) mode customization

;;; Commentary:
;; Load this package with:
;; (require 'text-custom)

;;; Code:
;;** Text mode (all markup modes)

;; (add-hook 'text-mode-hook
;;   (lambda ()
;;     (goto-address-mode)
;;     (font-lock-add-keywords nil
;;       '((hyperlink-fontify-button 0 nil append t)) 'append)
;; ))
;; (setq text-mode-hook nil)



(provide 'text-custom)

;;; text-custom.el ends here
