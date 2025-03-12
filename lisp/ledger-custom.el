;;; ledger-custom --- Handling of ledger files


;;; Commentary:


;;** Ledger


(setq ledger-accounts-file "/home/dan/accounting/ledger/accounts.ledger")


(defun ledger-do-check ( buffer)
  "Run a check command ."
  (goto-char (point-min))
  (let ((data-pos (point))
        (have-warnings nil))
    (shell-command
     ;;  ledger balance command will just return empty if you give it
     ;;  an account name that doesn't exist.  I will assume that no
     ;;  one will ever have an account named "e342asd2131".  If
     ;;  someones does, this will probably still work for them.
     ;;  I should only highlight error and warning lines.
     (concat "ledger -f " buffer " bal e342asd2131 --strict --explicit")
     t nil)
    (goto-char data-pos)

    ;; format check report to make it navigate the file

    (while (re-search-forward "^.*: \"\\(.*\\)\", line \\([0-9]+\\)" nil t)
      (let ((file (match-string 1))
            (line (string-to-number (match-string 2))))
        (when file
          (set-text-properties (line-beginning-position) (line-end-position)
                               (list 'ledger-source (cons file (save-window-excursion
                                                                 (save-excursion
                                                                   (find-file file)
                                                                   (widen)
                                                                   (ledger-navigate-to-line line)
                                                                   (point-marker))))))
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'font-lock-face 'ledger-font-report-clickable-face))
          (setq have-warnings 'true)
          (end-of-line))))
    (if (not have-warnings)
        (insert "No errors or warnings reported."))))


(defun ledger-check-buffer ()
  "Run ledge with --explicit and --strict report errors and assist with fixing them.

The output buffer will be in `ledger-check-mode', which defines
commands for navigating the buffer to the errors found, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
                (y-or-n-p "Buffer modified, save it? "))
       (save-buffer))))
  (let ((buf (find-file-noselect (ledger-master-file)))
        (cbuf (get-buffer ledger-check-buffer-name))
        (wcfg (current-window-configuration)))
    (if cbuf
        (kill-buffer cbuf))
    (with-current-buffer
        (pop-to-buffer (get-buffer-create ledger-check-buffer-name))
      (ledger-check-mode)
      (set (make-local-variable 'ledger-original-window-cfg) wcfg)
      (ledger-do-check (buffer-name buf))
      (shrink-window-if-larger-than-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (message "q to quit; r to redo; k to kill"))))

(provide 'ledger-custom)

;;; ledger-custom.el ends here
