;;; dired-custom --- Custom dired settings


;;; Commentary:

;;; Code:

;;** Dired mode and auto-revert mode


;;*** Set variables

(setq dired-dwim-target t
      dired-isearch-filenames t
      truncate-lines t
      ;; dired-listing-switches "-alht --group-directories-first"
      dired-listing-switches "-alh"
      ;; dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$"
      ;; dired-omit-files "^\\.?#\\|^\\.$"
      ;; dired-omit-files (concat dired-omit-files "\\|^\\.[^.].+$")
      auto-revert-verbose nil
      dired-omit-verbose nil)


;;*** Function definitions

(defun dired-select-other-pane ()
  (interactive)
  (other-window 1))


(defun dired-open-file ()
  (interactive)
  (let (( path (dired-get-filename)))
    (open-file-choice path)))


(defun dired-mouse-open-file ( event)
  (interactive "e")
  (mouse-set-point event)
  (dired-open-file))


(defun dired-mouse-2-button ( event)
  (interactive "e")
  (mouse-set-point event)
  ;; (dired-find-file)
  (dired-find-alternate-file))


(defun dired-mouse-3-button ( event)
  (interactive "e")
  (mouse-set-point event)
  (dired-find-file-other-window))


(defun kill-all-dired-buffer ()
  (interactive)
  (let ( kills)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (string= major-mode "dired-mode")
          (setq name (buffer-name buf))
          (add-to-list 'kills name t))))
    (dolist (bufName kills)
      (message "Kill dired buffer: %s" bufName)
      (kill-buffer (get-buffer bufName)))))


(defun dired-fullscreen-one-pan-view ()
  (interactive)
  (let (( dirPath default-directory)
        ( file (file-name-nondirectory (buffer-file-name))))
    (other-window 1)
    (kill-all-dired-buffer)
    (dired dirPath)
    (re-search-forward (regexp-quote file) nil t)))


(defun dired-one-pan-view-file ()
  (interactive)
  (let (( file (buffer-name))
        ( dir default-directory))
       (dired dir)
       (revert-buffer)
       (goto-char (point-min))
       (re-search-forward file)))


(defun dired-fullscreen-two-pan-view ( &optional dir)
  (interactive)
  (unless dir
    (setq dir default-directory))
  (kill-all-dired-buffer)
  (dired dir)
  (mouse-delete-other-windows)
  (i3-fullscreen-by-killing)
  (split-window-right))


(defvar dired-ls-switches
  '("-lhat --group-directories-first"
    "-lha --group-directories-first"
    "-lh --group-directories-first"))


(defun dired-ls-cycle-switches ()
  (interactive)
  (let* (( rest (member dired-listing-switches dired-ls-switches))
         ( next (if (= (length rest) 1)
                    (car dired-ls-switches)
                  (cadr rest))))
    (dired-sort-other
     (setq dired-listing-switches (if next
                                      next
                                    "-lh --group-directories-first")))))
    


;;*** Running hook

(add-hook 'dired-mode-hook
  (lambda ()
    (hl-line-mode)
    ;; (turn-on-gnus-dired-mode)
    (define-key dired-mode-map [tab] 'dired-select-other-pane)
    (define-key dired-mode-map "\C-co" 'dired-open-file)
    (define-key dired-mode-map "\M-h" 'dired-ls-cycle-switches)
    (define-key dired-mode-map (kbd "<mouse-2>")
      'dired-mouse-2-button)
    (define-key dired-mode-map (kbd "<mouse-3>")
      'dired-mouse-3-button)
    (define-key dired-mode-map (kbd "<S-mouse-2>")
      'dired-up-directory)
    (define-key dired-mode-map (kbd "<C-M-mouse-3>")
               'dired-mouse-open-file)))
 

(provide 'dired-custom)

;;; dired-custom.el ends here
