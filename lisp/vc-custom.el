;;; vc-custom --- Version control customization

;;; Commentary:

;;; Code:

(defun vc-dir-open-marked-files ()
  (interactive)
  (dolist ( file (vc-dir-marked-files))
    (find-file-noselect file))
  (vc-dir-unmark-all-files 'all-states)
  (obuffer-update))


(defun vc-dir-delete-marked-files ()
  (interactive)
  (dolist ( file (vc-dir-marked-files))
    (when (y-or-n-p (format "Delete file %s? " file))
      (delete-file file)))
  (vc-dir-refresh)
  (obuffer-update))


(defun vc-dir-unhide ( &optional all)
  (interactive)
  (let* (( root (file-name-as-directory
                 (expand-file-name (vc-root-dir))))
         ( home (file-name-as-directory (expand-file-name "~")))
         ( database (concat home ".obuffer.db"))
         ( files (split-string
                  (shell-command-to-string
                   (concat "locate -d " database
                           " -r " root))
                  "\n" 'omit-nulls))
           regular-files)
    (dolist ( file files)
      (when (and (file-regular-p file)
                 (or all (get-file-buffer file)))
        (setq regular-files (cons file regular-files))))
    (let (( file-count (length regular-files)))
      (message "Resynching %s files..." file-count)
      (let (( message-log-max nil)
            ( i 0))
        (dolist ( file regular-files)
          (setq i (1+ i))
          (message "Resynch file %s of %s..." i file-count)
          (vc-dir-resynch-file file))))))


(defun vc-dir-unhide-all ()
  (interactive)
  (vc-dir-unhide 'all))


(add-hook 'vc-dir-mode-hook
  (lambda ()
    (define-key vc-dir-mode-map (kbd "O") 'vc-dir-open-marked-files)
    (define-key vc-dir-mode-map (kbd "d") 'vc-dir-delete-marked-files)))

(add-hook 'vc-checkin-hook 'obuffer-update)


(provide 'vc-custom)

;;; vc-custom.el ends here
