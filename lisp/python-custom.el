;;; python-custom --- Customization of python mode

;;; Commentary:
;; Load this package with:
;; (require 'python-custom)

;;; Code:


(setq python-outline-regexp (concat
                             "#\\*\\(\\*\\**\\)\s[^\n].*\\|"
                             "\s*\\(class\s\\|def\s\\|"
                             "#\\+BEGIN_DOXY def\\|"
                             "#\\+BEGIN_DOXY class\\)")
      python-outline-heading-alist '(("class" . 9)
                                     ("#+BEGIN_DOXY class" . 9)
                                     ("def" . 10)
                                     ("#+BEGIN_DOXY def" . 10)
                                     ("!+BEGIN_DOXY file" . 1))
      python-outline-end-regexp "\n"
      python-indent-guess-indent-offset nil
      python-indent-guess-indent-offset-verbose nil
      python-fill-docstring-style 'pep-257-nn)


(add-hook 'python-mode-hook
  (lambda ()
    (cfold-minor-mode 1)
    (flycheck-mode 1)
    (define-key python-mode-map "\C-cc" 'obuffer-compile)))
 

(provide 'python-custom)

;;; python-custom.el ends here
