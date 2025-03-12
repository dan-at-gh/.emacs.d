;;; html-custom --- HTML mode customization

;;; Commentary:
;; Load this package with:
;; (require 'html-custom)

;;; Code:
;;** Html mode

;;*** Set variables

(setq sgml-validate-command "tidy")


;;*** Inserting commands

(defun html-insert-tag ()
  (interactive)
  (let (( tag (completing-read "Tag type: "
                    '("b" "a")
                    nil nil "")))
    (insert "<" tag ">" (paste-word-or-region) "</" tag ">"))
  (re-search-forward "</" (line-beginning-position) t -1))


;;*** Function view in browser

(defun html-view-on-this-workspace ()
  (interactive)
  (save-buffer)
  (crowded-close-others)
  (async-shell-command (concat "firefox " (buffer-file-name)
                               " -P emacs -no-remote"))
  (shell-command "/home/dan/bin/i3-focus-window 5 emacs"))


;;*** Hook


(add-hook 'html-mode-hook
  (lambda ()
    (define-key html-mode-map "\C-cc" 'html-view-on-this-workspace)
    (font-lock-add-keywords
     nil '(("$[A-Za-z_][A-Za-z0-9_]*" . font-lock-keyword-face)))))

(add-hook 'mhtml-mode-hook
  (lambda ()
    (font-lock-add-keywords
     nil '(("$[A-Za-z_][A-Za-z0-9_]*" . font-lock-keyword-face)))))



(provide 'html-custom)

;;; html-custom.el ends here
