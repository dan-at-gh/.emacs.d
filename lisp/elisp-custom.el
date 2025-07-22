;;; elisp-custom --- Emacs Lisp customization -*- lexical-binding: t; hallo: 3; -*-

;;; Commentary:

;;; Code:

(defun elisp-package-init ()
  "Initialize a new Emacs Lisp package file.

The file format respects flycheck conventions."
  (interactive)
  (goto-char (point-min))
  (let (( package (file-name-base (buffer-file-name))))
    (insert ";;; " package " ---  \n\n"
            ";;; Commentary:\n"
            ";; Load this package with:\n"
            ";; (require '" package ")\n\n"
            ";;; Code:\n\n"
            "(provide '" package ")\n\n"
            ";;; " (file-name-nondirectory (buffer-file-name)) " ends here"))
  (add-file-local-variable-prop-line 'lexical-binding t))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (flycheck-mode 1)))

(provide 'elisp-custom)

;;; elisp-custom.el ends here
