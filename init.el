;;; init --- Emacs init  -*- lexical-binding: t; -*-

;;; Commentary:
;; Variables:
;; user-init-file: .emacs
;; custom-file
;; user-emacs-directory: .emacs.d

;;; Code:
;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Use org mode for .emacs file:
;; (require 'org)
;; (org-babel-load-file (expand-file-name "emacs.org"
;;                      user-emacs-directory))
(load-file "~/.emacs.d/emacs.el")
(put 'scroll-left 'disabled nil)

(provide 'init)

;;; init.el ends here
