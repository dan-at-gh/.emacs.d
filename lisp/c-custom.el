;;; c-custom --- C Customization

;;; Commentary:

;;; Code:
;;** C mode


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c++-mode-hook
 (lambda ()
   (define-key c++-mode-map"\C-cc" 'compile)))
 

(provide 'c-custom)

;;; c-custom.el ends here
