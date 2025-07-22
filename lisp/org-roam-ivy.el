;;; org-roam-ivy ---  -*- lexical-binding: t; -*-

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

(defun e/ivy-org-roam-node-find ()
  (interactive)
  (let (( completing-read-function 'ivy-completing-read))
    (org-roam-node-find)))

(defun e/ivy-org-roam-node-insert ()
  (interactive)
  (let (( completing-read-function 'ivy-completing-read))
    (when (and (word-at-point)
               (not (region-active-p)))
      (backward-word)
      (mark-word))
    (org-roam-node-insert)))

(defun e/ivy-org-roam-node-find-ow ()
  (interactive)
  (other-window 1)
  (e/ivy-org-roam-node-find))

(defun e/ivy-org-roam-ref-find ()
  (interactive)
  (ivy-mode 1)
  (unwind-protect
      (org-roam-ref-find)
    (ivy-mode -1)))

(provide 'org-roam-ivy)

;;; org-roam-ivy.el ends here
