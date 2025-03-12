;;; markdown-custom --- Markdown customization

;;; Commentary:
;; Load this package with:
;; (require 'markdown-custom)

;;; Code:
;;** Markdown mode

(defvar jekyll-root-url "http://localhost:4000")


(defun jekyll-url ()
  (string-match "^\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)-\\(.*?\\)\\.md$" (buffer-name))
  (concat (file-name-as-directory jekyll-root-url)
          (file-name-as-directory (match-string 1 (buffer-name)))
          (file-name-as-directory (match-string 2 (buffer-name)))
          (file-name-as-directory (match-string 3 (buffer-name)))
          (match-string 4 (buffer-name)) ".html"))


(defun jekyll-view ()
  (interactive)
  (crowded-close-others)
  (async-shell-command (concat "firefox "
                               (jekyll-url)
                               " -P emacs -no-remote")))


(add-hook 'markdown-mode-hook
  (lambda ()
    (setq-local tab-width 2)
    (when buffer-file-name
      (let* (( infile (file-name-nondirectory buffer-file-name)))
        (set (make-local-variable 'compile-command)
             (concat "mdparse " infile))))
    (define-key markdown-mode-map "\C-cc" 'compile)
    (define-key markdown-mode-map "\C-cv" 'jekyll-view)))



(provide 'markdown-custom)

;;; markdown-custom.el ends here
