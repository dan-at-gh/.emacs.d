;;; elisp-custom --- Emacs Lisp customization

;;; Commentary:

;;; Code:


(defun elisp-package-init ()
  "Initialize a new Emacs Lisp package file.

The file format respects flycheck conventions."
  (interactive)
  (goto-char (point-min))
  (let (( package (file-name-base (buffer-file-name))))
    (insert ";;; " package " ---\n\n"
            ";;; Commentary:\n"
            ";; Load this package with:\n"
            ";; (require '" package ")\n\n"
            ";;; Code:\n\n"
            "(provide '" package ")\n\n"
            ";;; " (file-name-nondirectory (buffer-file-name)) " ends here")))
      

(defun elisp-insert-doc-template ()
  (interactive)
  (end-of-line)
  (when (re-search-forward "(defun" nil t -1)
    (beginning-of-line 2)
    (insert
"  \"

Args:
  ARG1:
Returns: \"
")))


;;*** Hook

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (cfold-minor-mode 1)
    (flycheck-mode 1)))


(provide 'elisp-custom)

;;; elisp-custom.el ends here
