;;; ivy-custom --- Customization of Ivy  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ivy)

;; (define-key ivy-minibuffer-map (kbd "<C-return>") 'e/org-roam-ivy-preview-browser)
;; (define-key ivy-minibuffer-map (kbd "<C-down>") 'e/org-roam-ivy-preview-next-browser)
;; (define-key ivy-minibuffer-map (kbd "<C-up>") 'e/org-roam-ivy-preview-previous-browser)
;; C-g: Exit Ivy

;; (ivy-set-actions
;;  t
;;  '(("o" e/org-roam-ivy-preview "preview roam file")))
;; (ivy-set-actions t nil)

;; (define-key ivy-minibuffer-map (kbd "<C-M-return>") 'e/ivy-done)
;; (define-key ivy-minibuffer-map (kbd "<return>") 'e/ivy-done)
;; C-M-m: Show current in emacs
;; C-M-n, C-M-p: Show next,previous in emacs
;; C-M-return: Exit Ivy and show current in emacs


(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order))
      ivy-display-style nil
      ivy-use-selectable-prompt t
      ivy-case-fold-search-default t)
;; default:
;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

(set-face-attribute 'ivy-current-match nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'ivy-highlight-face)

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
             (message "Ivy done: Existing node")
             (ivy--done (ivy-state-current ivy-last))
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
             (message "Ivy done: New node")
             (ivy--done ivy-text))
            (t
             (setq ivy--prompt-extra " (match required)")
             (message "60")
             (insert ivy-text)
             (ivy--exhibit))))))

(provide 'ivy-custom)

;;; ivy-custom.el ends here
