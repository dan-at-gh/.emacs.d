;;; grep-custom --- Extended grep mode

;;; Commentary:

;;; Code:
(require 'grep)
(require 'outline)

(defvar grep-base-directory nil)


(defun grep-set-base-directory ()
  (interactive)
  (setq grep-base-directory
        (read-directory-name "Grep base directory: "
                             default-directory)))

(defun grep-region ()
  (interactive)
  (unless grep-base-directory
    (grep-set-base-directory))
  (let* (( default-cmd (concat "cd " grep-base-directory
                               "; grep --color -nH -e "
                               (when (region-active-p)
                                 (buffer-substring (region-beginning)
                                                   (region-end)))
                               " -r ."))
         ( cmd (read-from-minibuffer "Run grep (like this): "
                                     default-cmd nil nil 'grep-history)))
    (grep cmd)))


(defun e/grep-absolute-path ()
  (goto-char (point-min))
  (when (re-search-forward "^grep[^/]+/" nil t)
    (file-name-as-directory (thing-at-point 'filename))))


(defun e/grep-show-only-relative-path ()
  (interactive)
  (let* (( path (e/grep-absolute-path))
         overlay)
    (while (re-search-forward path nil t)
      (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                   'invisible t))))


(defun grep-match-open-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '(display-buffer-same-window
           (inhibit-same-window . nil))))
    (compile-goto-error))
  (sleep-for 1)
  (set-buffer (window-buffer))
  (outline-show-all))

(defun grep-mouse-goto-match ( event)
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))))
         (select-window window)
         (goto-char pos)
         (grep-match-open-same-window)))

(add-hook 'grep-mode-hook
  (lambda ()
    (message "grep-mode-hook executed")
    ;; (setq truncate-lines t)
    (visual-line-mode 1)
    (message "grep-mode-hook compilation-button-map mouse-2: %s"
      (lookup-key compilation-button-map (kbd "<mouse-2>")))
    (define-key grep-mode-map (kbd "r")
      'e/grep-show-only-relative-path)
    (define-key compilation-button-map (kbd "RET")
      'grep-match-open-same-window)
    (define-key compilation-button-map (kbd "<mouse-2>")
      'grep-mouse-goto-match)))


(provide 'grep-custom)

;;; grep-custom.el ends here
