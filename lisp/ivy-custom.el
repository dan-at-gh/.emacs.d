;;; ivy-custom --- Customization of Ivy

;;; Commentary:

;;; Code:

(require 'ivy)

(define-key ivy-minibuffer-map (kbd "<C-return>") 'e/org-roam-ivy-preview-browser)
(define-key ivy-minibuffer-map (kbd "<C-down>") 'e/org-roam-ivy-preview-next-browser)
(define-key ivy-minibuffer-map (kbd "<C-up>") 'e/org-roam-ivy-preview-previous-browser)
;; C-g: Exit Ivy

(ivy-set-actions
 t
 '(("o" e/org-roam-ivy-preview "preview roam file")))
(define-key ivy-minibuffer-map (kbd "<C-M-return>") 'e/ivy-done)
(define-key ivy-minibuffer-map (kbd "<return>") 'e/ivy-done)
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

(provide 'ivy-custom)

;;; ivy-custom.el ends here
