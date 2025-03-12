;;; prog-custom --- Customize prog mode

;;; Commentary:

;;; Code:
;;** Prog mode (all programming modes)


(defun line-prefix ( &optional pos prefix-extra)
  (save-match-data
    (save-excursion
      (when pos (goto-char pos))
      (beginning-of-line)
      (unless prefix-extra (setq prefix-extra "[^\s\n]+"))
      (message "line-prefix")
      (when (looking-at (concat "^" (string-trim comment-start)
                                prefix-extra "\s"))
        (match-string-no-properties 0)))))


(defun prefix-comment-new-line ()
  "Create new comment line respecting extra comment prefix.

When no comment prefix is found at the beginning of the line, use
`comment-indent-new-line' instead."
  (interactive)
  (let (( prefix (line-prefix)))
    (if prefix
        (if (>= (current-column) (length prefix))
            (insert (concat "\n" prefix))
          (beginning-of-line)
          (insert (concat prefix "\n"))
          (end-of-line 0))
      (comment-indent-new-line))))


(add-hook 'prog-mode-hook
  (lambda ()
    (auto-complete-mode)
    (define-key prog-mode-map "\M-j" 'prefix-comment-new-line)))



(provide 'prog-custom)

;;; prog-custom.el ends here
