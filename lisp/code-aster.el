;;; code-aster --- Code Aster input file major mode

;;; Commentary:

;;; Code:

;;** Code Aster


(add-to-list 'auto-mode-alist '("\\.comm\\'" . code-aster-mode))


(define-derived-mode code-aster-mode python-mode "Code_Aster"
    "Major mode for editing Code_Aster input files (*.comm)."
    (setq-local code-block-lang "python"))

(provide 'code-aster)

;;; code-aster.el ends here
