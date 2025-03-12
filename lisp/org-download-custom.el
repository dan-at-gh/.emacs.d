;;; org-download-custom --- Org-download customization

;;; Commentary:
;; Load this package with:
;; (require 'org-download-custom)

;;; Code:

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


(setq org-download-display-inline-images nil
      org-download-link-format "[[file:%s][file:%s]]\n"
      org-download-link-format-function 'e/org-download-link-format-function)


(defun e/org-download-link-format-function (filename)
  "Custom function."
  (if (and (>= (string-to-number org-version) 9.3)
           (eq org-download-method 'attach))
      (format "[[attachment:%s]]\n"
              (org-link-escape
               (file-relative-name filename (org-attach-dir))))
    (replace-regexp-in-string "%s"
                              (org-link-escape
                               (funcall org-download-abbreviate-filename-function
                                        filename))
                              org-download-link-format)))


(provide 'org-download-custom)

;;; org-download-custom.el ends here
