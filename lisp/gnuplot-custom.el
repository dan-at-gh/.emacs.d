;;; gnuplot-custom --- Gnuplot mode customization

;;; Commentary:
;; Load this package with:
;; (require 'gnuplot-custom)

;;; Code:
;;** Gnuplot mode


;;*** Set variables

;; Automatically open files ending with .gp or .gnuplot in gnuplot mode

(setq auto-mode-alist
  (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode))
          auto-mode-alist))


;;*** Hook

(add-hook 'gnuplot-mode-hook
  (lambda ()
    (remove-hook 'after-change-functions 'gnuplot-scan-after-change t)
    (define-key gnuplot-mode-map "\C-cc" 'compile)
    (define-key gnuplot-mode-map "\C-cv" 'TeX-view-manage-windows)))


;;; emacs.el ends here

(provide 'gnuplot-custom)

;;; gnuplot-custom.el ends here
