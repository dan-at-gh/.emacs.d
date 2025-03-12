;;; web-custom --- Web mode customization -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'web-custom)

;;; Code:


(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . web-mode))
(add-to-list 'magic-mode-alist '("<?php" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))


(add-hook
 'web-mode-hook
 (lambda ()
   (setq web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2)
   ))

(provide 'web-custom)

;;; web-custom.el ends here
