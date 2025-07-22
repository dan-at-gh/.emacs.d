;;; org-custom --- My custom org configuration -*- lexical-binding: t; -*-


;;; Commentary:


;;; Code:

(require 'org)
(require 'org-element)
(require 'org-colview)
(require 'ox-ascii)
(require 'org-transclusion)
(require 'org-clock)
(require 'org-duration)
(require 'org-roam-custom)
(require 'org-num)
(require 'org-cite-custom)
(require 'org-latex-custom)
(require 'org-download)
(require 'visual-fill-column)
(require 'org-firefox-link)
(require 'org-ai)

(defun e/org-mouse-cycle-subtree (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (org-cycle))))

(defun e/org-mouse-cycle-global (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (org-cycle-internal-global))))

(defun e/org-open-at-point-other-window ( event)
  (interactive "e")
  (let (( org-link-frame-setup '(( file . find-file-other-window)))
        ( path (cadr (mouse-posn-property (event-start event) 'htmlize-link)))
        ( calling-buffer (current-buffer)))
    (org-open-at-mouse event)
    (when (string-search "dailies" path)
      (message "hallo welt")
      (set-window-buffer (next-window) (current-buffer))
      (set-window-buffer nil calling-buffer)
      )))

(setq org-time-stamp-custom-formats (quote
        ("%d.%m.%Y" . "%d.%m.%Y, %H:%M"))
      org-use-sub-superscripts nil
      org-default-notes-file "~/.emacs.d/org/notes.org"
      org-priority-default ?C
      org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "evince %s")
          ("\\.eps\\'" . "evince %s")
          ("\\.doc\\'" . "libreoffice %s")
          ("\\.odt\\'" . "libreoffice %s")
          ("\\.ods\\'" . "libreoffice %s")
          ("\\.xlsx\\'" . "libreoffice %s")
          ("\\.svg\\'" . "inkscape %s")
          ("\\.blend\\'" . "blender %s")
          ("\\.hdf\\'" . "salome %s")
          ("\\.xcf\\'" . "gimp %s"))
      org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))
      org-clock-in-resume t
      org-clock-report-include-clocking-task t
      org-clock-mode-line-total 'today
      org-clock-persist t
      org-log-done t
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers t
      org-log-note-clock-out nil
      org-ascii-text-width 70
      org-footnote-auto-label 'random
      org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+"
           (:strike-through t)))
      org-babel-load-languages
        '(( emacs-lisp . t)
          ( scheme .t)
          ( python . t)
          ( shell . t)
          ( java . t))
      org-babel-tangle-lang-exts
        '(("emacs-lisp" . "el")
          ("elisp" . "el")
          ("scheme" . "scm")
          ("python" . "py"))
      org-fold-show-context-detail
      '((agenda . canonical)
        (occur-tree . local)
        (tags-tree . minimal)
        (bookmark-jump . lineage)
        (isearch . lineage)
        (default . ancestors))
      org-blank-before-new-entry
      '((heading . t)
        (plain-list-item . auto))
      org-cycle-separator-lines 2
      org-cycle-include-plain-lists t
      org-num-skip-unnumbered t
      org-num-format-function 'e-org-num-format-last
      org-image-actual-width nil
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-refile-use-outline-path 'file
      org-preview-latex-default-process 'dvisvgm
      org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
      org-link-search-must-match-exact-headline nil
      org-fontify-quote-and-verse-blocks t
      org-footnote-forbidden-blocks
      '("comment" "example" "export" "src" "quote")
      org-use-tag-inheritance nil
      org-odd-levels-only nil
      org-hide-leading-stars nil
      org-hide-emphasis-markers t
      org-adapt-indentation nil
      org-directory "~/.emacs.d/org"
      org-pretty-entities nil
      org-tag-alist nil
      org-fontify-whole-heading-line nil
      org-footnote-auto-adjust t
      org-footnote-section "Footnotes"
      org-src-preserve-indentation t
      org-src-block-faces
      '(("emacs-lisp" (:background "#EEE2FF" :extend t)))
      org-src-fontify-natively t
      org-todo-keyword-faces
      '(("TODO" . "purple")
        ("DONE" . "lightgreen"))
      org-todo-keywords
      '((sequence "TODO" "|" "DONE"))
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-fontify-done-headline nil
      org-duration-format 'h:mm
      org-table-duration-hour-zero-padding nil
      org-startup-folded 'fold)

(org-clock-persistence-insinuate)

(defun e/org-set-face-attributes ()
  (let (( height 1.2)
        ( fg "gray"))
    (set-face-attribute 'org-document-title nil
                        :height height :weight 'bold)
    (set-face-attribute 'org-block-begin-line nil
                        :foreground fg
                        :height 0.7
                        :inherit 'org-document-info-keyword)
    (set-face-attribute 'org-quote nil
                        :foreground "dim gray" :background 'unspecified)
    (set-face-attribute 'org-drawer nil
                        :foreground fg
                        :height 0.7
                        :inherit 'org-document-info-keyword)
    (set-face-attribute 'org-block nil
                        :background "misty rose"
                        :extend t
                        :inherit 'shadow)
    ))

(e/org-set-face-attributes)

(defun e/org-columns-move-subtree-down (&optional arg)
  "Move the current subtree down ARG headlines, from column view."
  (interactive "p")
  (org-columns-quit)
  (org-move-subtree-down arg)
  (org-columns))

(defun e/org-columns-move-subtree-up (&optional arg)
  "Move the current subtree up ARG headlines, from column view."
  (interactive "p")
    (org-columns-quit)
  (org-move-subtree-up arg)
  (org-columns))

;;*** clocktable with average

(defun org-dblock-write:e/clocktable ( params)
  (let* (( internal-time-start (car (org-clock-special-range 'thisweek)))
         ( tstart (format-time-string (org-time-stamp-format t t)
                                          internal-time-start))
         ( internal-time-end (encode-time
                              (append '( 0 0 0) (last (decode-time (current-time)) 6))))
         ( tend (format-time-string (org-time-stamp-format t t)
                                          internal-time-end))
         ( internal-time-diff (time-subtract internal-time-end
                                             internal-time-start))
         ( time-diff (min 5 (truncate (/ (float-time internal-time-diff)
                                         (* 60 60 24)))))
         ( formula (format "@1$4=days %s::$4=($2+$3)/%s;U" time-diff time-diff)))
    (insert "#+CAPTION: " tstart " --> " tend "\n")
    (setq params (plist-put params :formula (when (> time-diff 0) formula))
          params (plist-put params :block nil)
          params (plist-put params :tstart tstart)
          params (plist-put params :tend tend))
    (org-dblock-write:clocktable params)))

;;*** folding stategies

(defun e/org-show-up-to-level ( level &optional show-entry)
  (interactive "p")
  (org-overview)
  (goto-char (point-min))
  (while (re-search-forward (concat "^\\(\\*\\{1,"
                                    (number-to-string level)
                                    "\\}\\)\s")
                            nil t)
    (if (< (length (match-string 1)) level)
        (org-fold-show-children)
      (when show-entry
        (org-fold-show-entry))))
  (goto-char (point-min))
  (message "%s" show-entry))


(defun e/org-show-entries-at-level ( level)
  (interactive "p")
  (e/org-show-up-to-level level 'show-entry))
    

;;*** Match Sparse Tree with Mouse


(defvar e/org-match-string "")


(defun e/org-match-add-at-point ( &optional subtract)
  (interactive)
  (unless (string= (car (org-thing-at-point)) "tag")
    (setq e/org-match-string "")
    (user-error "No Tag at Point. Match String Reset."))
  (org-match-line org-complex-heading-regexp)
  (let (( sign (if subtract "-" "+"))
        ( tags-beg (match-beginning 5))
	    ( tags-end (match-end 5))
        ( tag ""))
    (save-excursion
	  (let* (( beg-tag (or (search-backward ":" tags-beg 'at-limit) (point)))
		     ( end-tag (search-forward ":" tags-end nil 2)))
        (setq tag (buffer-substring-no-properties (1+ beg-tag) (1- end-tag)))))
    (setq e/org-match-string (concat e/org-match-string sign tag)))
  (org-match-sparse-tree nil e/org-match-string)
  ;; Loop over overlays to change visibility
  ;; overlay: org-type  org-occur
  (let (( overlays (overlays-in (point-min) (point-max)))
        ( i 0))
    (dolist ( overlay overlays)
      (when (eq (overlay-get overlay 'org-type) 'org-occur)
        (setq i (1+ i))))
    (message "Match Tags: %s, Found %s." e/org-match-string i)))


(defun e/org-match-subtract-at-point ()
  (interactive)
  (e/org-match-add-at-point 'subtract))


(defun e/org-match-add-at-mouse ( ev)
  (interactive "e")
  (mouse-set-point ev)
  (e/org-match-add-at-point))


(defun e/org-match-subtract-at-mouse ( ev)
  (interactive "e")
  (mouse-set-point ev)
  (e/org-match-subtract-at-point))

(defvar e/org-sparse-tree-last-match-id nil)
(defvar e/org-sparse-tree-tag-inheritance-toggle nil)

(defun e/org-sparse-tree-match ( &optional only-match-string)
  (interactive)
  (let (( match (or (org-entry-get (point) "match")
                    (when (car (last (org-heading-components)))
                      (substring
                       (replace-regexp-in-string
                        ":" "+" (car (last (org-heading-components))))
                       nil -1))
                    (user-error "Match String Undefined")))
        ( id (org-id-get-create)))
    (unless only-match-string
      (if (string= id e/org-sparse-tree-last-match-id)
          (setq e/org-sparse-tree-tag-inheritance-toggle
                (not e/org-sparse-tree-tag-inheritance-toggle))
        (setq e/org-sparse-tree-tag-inheritance-toggle nil
              e/org-sparse-tree-last-match-id id))
      (let (( org-use-tag-inheritance
              e/org-sparse-tree-tag-inheritance-toggle))
        (message "Tag Inheritance %s" (if org-use-tag-inheritance "ON" "OFF"))
        (org-match-sparse-tree nil match))
      (save-excursion
        (when (org-up-heading-safe)
          (org-fold-show-children))))
    match))


(defun e/org-sparse-tree-match-at-mouse ( ev)
  (interactive "e")
  (mouse-set-point ev)
  (e/org-sparse-tree-match))


;;*** Bug fixes


(defun e/org-cycle-hide-drawers ( state)
  (when (eq state 'overview)
    (save-excursion
      (goto-char (point-min))
      (org-cycle-hide-drawers t)))
  (org-cycle-hide-drawers state))


(remove-hook 'org-cycle-hook 'org-cycle-hide-drawers)
(add-hook 'org-cycle-hook 'e/org-cycle-hide-drawers)


;;*** Hooks

(defun e/org-font-lock-add-keywords ()
  (add-to-list 'org-font-lock-extra-keywords
               '("\\(#\\+filetags:\\)\s+"
                 ( 1 'org-document-info-keyword t)
                 ( "[[:word:]_]" nil (re-search-backward "tags:")
                   ( 0 'highlight t))
                 ( ":" nil nil
                   ( 0 'org-document-info-keyword t)))
               'append)
  ;; (add-to-list 'org-font-lock-extra-keywords
  ;;              '( org-comic-fontify-character ( 0 nil append t))
  ;;              'append)
  ;; (add-to-list 'org-font-lock-extra-keywords
  ;;              '( org-comic-fontify-character-inline ( 0 nil append t))
  ;;              'append)
  )

(add-hook 'org-font-lock-set-keywords-hook #'e/org-font-lock-add-keywords)

(add-hook 'org-mode-hook
  (lambda ()
    ;; (font-lock-add-keywords
    ;;  'emacs-lisp-mode '(("(\\([[:word:]-=]+\\)" 1 font-lock-keyword-face)))
    ;; (font-lock-add-keywords
    ;;  'scheme-mode '(("(\\([[:word:]-=]+\\)" 1 font-lock-keyword-face)))
    ;; (font-lock-add-keywords
    ;;  'org-mode '(("\\(filetags:\\)\s" 1 font-lock-keyword-face))
    ;;  'end-of-list)
    ;; (add-to-list 'org-font-lock-extra-keywords
    ;;              '(("\\(filetags:\\)\s" ( 1 font-lock-keyword-face t))))

    (define-key org-mode-map "\C-ct" 'org-toggle-timestamp-type)
    (define-key org-mode-map (kbd "C-M->") 'org-dan-subtree-template)
    (define-key org-mode-map "\C-c[" 'reftex-citation)
    (define-key org-mode-map (kbd "M-;") 'e/org-comment-dwim)
    (define-key org-mouse-map (kbd "<mouse-3>") 'e/org-open-at-point-other-window)
    (define-key org-mode-map (kbd "<S-mouse-2>") 'e/org-mouse-cycle-subtree)
    (define-key org-mode-map (kbd "<C-M-mouse-3>") 'e/org-mouse-cycle-global)
    (define-key org-mode-map (kbd "<C-S-mouse-1>") 'e/org-sparse-tree-match-at-mouse)
    (define-key org-mode-map (kbd "<C-S-mouse-2>") 'org-download-clipboard)
    (define-key org-mode-map (kbd "<C-S-mouse-3>") 'e/org-roam-directed-edge-show-entries-mouse)
    (define-key org-mode-map (kbd "C-c C-n C-n") 'e/org-roam-note-list-plain)
    (define-key org-mode-map (kbd "C-c C-n C-e") 'e/org-roam-note-list-plain-exclude)
    (define-key org-mode-map (kbd "M-e") 'e/org-publish-current-project)
    ;;
    (define-key org-mode-map (kbd "C-c x") 'e-org-cut-link)
    (define-key org-mode-map (kbd "C-c w") 'e-org-copy-link)
    (define-key org-mode-map (kbd "C-c c") 'e/org-roam-link-copy-id)
    (define-key org-mode-map (kbd "C-c y") 'e/org-roam-link-yank)
    (define-key org-mode-map (kbd "C-c r") 'e/org-remove-link)
    (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
    (define-key org-mode-map (kbd "C-c v") 'e/org-tags-visible-headings-add)
    (define-key org-mode-map (kbd "C-c m") 'e/org-roam-node-create-copy)
    (define-key org-mode-map (kbd "C-c e") 'e/org-export-buffer)
    (define-key org-mode-map (kbd "C-c p") 'e/org-set-property)
    (define-key org-mode-map (kbd "C-c q") 'e/org-insert-block-quote)
    (define-key org-mode-map (kbd "C-c C-a") 'org-agenda)
    ;; Global overrides
    (define-key org-mode-map (kbd "C-c C-x C-i") nil)
    ;; Column view
    (define-key org-columns-map (kbd "M-<down>") #'e/org-columns-move-subtree-down)
    (define-key org-columns-map (kbd "M-<up>") #'e/org-columns-move-subtree-up)

    (visual-fill-column-mode 1)
    (setq visual-fill-column-center-text t)
    (setq org-download-heading-lvl nil
          org-download-image-dir "./images"
          org-ai-image-directory "./images")
    (org-latex-custom-hook-functions)
    (org-cite-custom-hook-functions)
    (org-firefox-link-hook-functions)
    ))

(add-hook 'org-occur-hook
          (lambda ()
            (remove-overlays nil nil 'org-type 'org-occur)))


(add-hook 'org-clock-in-hook #'save-buffer)


(add-hook 'org-clock-out-hook #'save-buffer)


;;*** export to markdown


(org-link-set-parameters "rel")

(defun e/org-publish-after-message ( _ out-file)
  (when-let (( dir (org-entry-get (point-min) "EXPORT_DIR")))
    (rename-file out-file
                 (concat (file-name-as-directory
                          (expand-file-name dir))
                         (file-name-nondirectory out-file))
                 'OK-IF-ALREADY-EXISTS)))


(add-hook 'org-publish-after-publishing-hook 'e/org-publish-after-message)


(defun e/org-export-buffer ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (when-let (( dir (org-entry-get (point-min) "EXPORT_DIR")))
    (let* (( file (concat (file-name-as-directory
                          (expand-file-name dir))
                         (org-export-output-file-name ".md"))))
      (org-export-to-file 'md file)
      (when-let (( buffer (get-file-buffer file)))
        (with-current-buffer buffer
          (revert-buffer 'IGNORE-AUTO 'NOCONFIRM))))))


;;*** follow link id and point


(org-link-set-parameters
 "id-point"
 :follow 'e/org-roam-id-point-open
 :face 'bold)


(defun e/org-roam-id-point-open ( path _)
  (let* (( id-point (split-string path))
         ( id (car id-point))
         ( point (string-to-number (cadr id-point)))
         ( m (org-roam-id-find id 'marker))
         ( buffer (marker-buffer m)))
    (switch-to-buffer-other-frame buffer)
    (goto-char point)))
    

;;*** org-transclusion

(set-face-attribute
 'org-transclusion-fringe nil
 :foreground "dark green"
 :background "dark green")

(setq org-transclusion-include-first-section nil)

(provide 'org-custom)

;;; org-custom.el ends here
