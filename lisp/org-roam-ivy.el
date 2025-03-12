;;; org-roam-ivy ---

;;; Commentary:
;; Load this package with:
;; (require 'org-roam-ivy)

;;; Code:
(require 'ivy)
(require 'org-roam)
(require 'org-roam-browser)

(defun e/org-roam-ivy-preview ( x)
  (let (( node (get-text-property 0 'node x)))
    (when node
      (display-buffer
       (find-file-noselect (org-roam-node-file node))))))


(defun e/org-roam-ivy-preview-browser ()
  (interactive)
  (let* (( current (ivy-state-current ivy-last))
         ( x (ivy--call-cand current))
         ( node (get-text-property 0 'node x)))
    (when node
      (e/org-browser-open-file (org-roam-node-file node)
                               (org-roam-node-id node)))))


(defun e/org-roam-ivy-preview-next-browser ()
  (interactive)
  (ivy-next-line)
  (ivy--exhibit)
  (e/org-roam-ivy-preview-browser))


(defun e/org-roam-ivy-preview-previous-browser ()
  (interactive)
  (ivy-previous-line)
  (ivy--exhibit)
  (e/org-roam-ivy-preview-browser))


(defun e/org-roam-ivy-exit-no-action ( x)
  (message "Exiting ivy without action."))


(defun e/ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (if (ivy--prompt-selected-p)
      (ivy-immediate-done)
    (setq ivy-current-prefix-arg current-prefix-arg)
    (let ((require-match (ivy-state-require-match ivy-last))
          (input (ivy--input)))
      (delete-minibuffer-contents)
      (cond ((and (= ivy--length 0)
                  (eq this-command 'ivy-dispatching-done))
             (message "10")
             (ivy--done ivy-text))
            ((or (> ivy--length 0)
                 ;; the action from `ivy-dispatching-done' may not need a
                 ;; candidate at all
                 (eq this-command 'ivy-dispatching-done))
             ;; in case of chosing existing node
             (message "20")
             ;; (ivy--done (ivy-state-current ivy-last))
             (ivy-exit-with-action 'e/org-roam-ivy-exit-no-action)
             )
            ((string= " (confirm)" ivy--prompt-extra)
             (message "30")
             (ivy--done ivy-text))
            ((or (and (memq (ivy-state-collection ivy-last)
                            '(read-file-name-internal internal-complete-buffer))
                      (eq confirm-nonexistent-file-or-buffer t))
                 (and (functionp require-match)
                      (setq require-match (funcall require-match))))
             (message "40")
             (setq ivy--prompt-extra " (confirm)")
             (insert input)
             (ivy--exhibit))
            ((memq require-match '(nil confirm confirm-after-completion))
             ;; in case of new node
             (message "50")
             (ivy--done ivy-text))
            (t
             (setq ivy--prompt-extra " (match required)")
             (message "60")
             (insert ivy-text)
             (ivy--exhibit))))))


(provide 'org-roam-ivy)

;;; org-roam-ivy.el ends here
