;;; org-roam-custom --- Org Roam customization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'org-roam-custom)

;;; Code:

(require 'org-roam)
(require 'org-roam-dailies)
(require 'ivy)
(require 'org-roam-publish)
(require 'org-roam-gatsby)

(defvar org-roam-html-server-url "")
(defvar e/org-roam-capture-bypass nil)
(defvar org-roam-hook nil)

(defun e/org-roam-hook-functions ()
  (add-hook 'after-save-hook #'e/org-roam-publish-after-save nil 'local)
  (add-hook 'org-export-before-processing-functions #'e/org-roam-export-preamble nil 'local)
  (add-hook 'org-export-before-processing-functions #'e/org-roam-auto-links-section nil 'local)
  
  (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-mode-map (kbd "C-c n i") 'e/ivy-org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c n h") 'e/org-roam-tree-note-new)
  (define-key org-mode-map (kbd "C-c n a") 'e/org-roam-note-alias-add)
  (define-key org-mode-map (kbd "<C-M-S-mouse-1>") 'e/org-roam-browser-open-current)
  (define-key org-mode-map (kbd "<C-M-S-mouse-5>") 'e/org-roam-browser-open-link-next)
  (define-key org-mode-map (kbd "<C-M-S-mouse-4>") 'e/org-roam-browser-open-link-previous)
  (define-key org-mode-map (kbd "<C-M-S-mouse-3>") 'e/org-roam-browser-open-link-at-mouse)

  (wcount-mode 1))

(add-hook 'org-roam-hook 'e/org-roam-hook-functions)

(defun org-roam-run-hook ()
  (when (org-roam-file-p)
    (run-hooks 'org-roam-hook)))

(add-hook 'org-mode-hook 'org-roam-run-hook)

(cl-defmethod org-roam-node-basename ((node org-roam-node))
  (file-name-nondirectory (org-roam-node-file node)))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  (cond ((or (org-roam-node-refs node)
             (string-match "/refs/" (org-roam-node-file node)))
         "REF")
        ((org-roam-dailies--daily-note-p
          (org-roam-node-file node))
         "DAILY")
        (t
         "BASE")))

(cl-defmethod org-roam-node-parents ((node org-roam-node))
  (let (( parents ""))
    (when (> (org-roam-node-level node) 0)
      (setq parents (concat " p:" (org-roam-node-file-title node)))
      (dolist ( parent (org-roam-node-olp node))
        (setq parents (concat parents "->" parent))))
    parents))

(cl-defmethod org-roam-node-prop-aliases ((node org-roam-node))
  (cdr (assoc "ALIASES"
              (org-roam-node-properties node))))

(cl-defmethod org-roam-node-atime ((node org-roam-node))
  (format-time-string "%Y-%m-%d" (org-roam-node-file-atime node)))

(cl-defmethod org-roam-node-mtime ((node org-roam-node))
  (format-time-string "%Y-%m-%d" (org-roam-node-file-mtime node)))

(cl-defmethod org-roam-node-ntitle ((node org-roam-node))
  "If the title is an alias, provide the actual title as well.

NODE and ORG-ROAM-NODE are necessary."
  (let (( candidate (org-roam-node-title node))
        ( title (caar (org-roam-db-query [:select [title]
                                                  :from nodes
                                                  :where (= id $s1)]
                                         (org-roam-node-id node)))))
    (if (string= candidate title)
        ""
      (propertize (concat " " title) 'face 'warning))))

(cl-defmethod org-roam-node-state ((node org-roam-node))
  "Return value of state keyword for current node.

NODE is reference to the current node."
  (let (( state (cdr (assoc "STATE" (org-roam-node-properties node)))))
    (if state
        (concat " #" state)
      "")))

(setq org-roam-directory (file-truename "~/.emacs.d/org-roam/")
      org-roam-html-server-url "http://127.0.0.1:8080/"
      org-roam-completion-everywhere t
      org-roam-node-display-template
      (concat (propertize "${title}" 'face 'bold)
              "${ntitle}"
              (propertize "${orp}" 'face 'gnus-group-mail-1)
              " " (propertize "${tags}" 'face 'web-mode-annotation-value-face)
              " " (propertize "${id}" 'face 'dired-ignored))
      e/org-roam-capture-bypass
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t
         :immediate-finish t
         :jump-to-captured t
         :kill-buffer nil
         :no-save t)))

;; This needs to read org-roam-directory - defaults to ~/org-roam
(org-roam-db-autosync-mode)

(defun e/org-roam-fontify-buffers ()
  (dolist ( frame (frames-on-display-list))
    (dolist ( window (window-list frame))
      (when (org-roam-buffer-p (window-buffer window))
        (with-selected-window window
          (font-lock-flush)
          (font-lock-ensure))))))

(defvar e/org-roam-max-slug-length 20)

(defun e/org-roam-string-limit-length ( s l)
  (substring s 0 (min l (length s))))

(defun e/org-roam-node-file-nondir ( &optional title)
  (when title
    (setq title (concat "-"
                        (e/org-roam-string-limit-length
                         (org-roam-node-slug (org-roam-node-create :title title))
                         e/org-roam-max-slug-length))))
  (concat (format-time-string "%Y%m%d%H%M%S") title ".org"))

(defun e/org-roam-node-file-name ( &optional title subdir)
  (concat (file-name-as-directory org-roam-directory)
          (when subdir (file-name-as-directory subdir))
          (e/org-roam-node-file-nondir title)))

(defun e/org-roam-node-file-name-short ()
  (interactive)
  (set-buffer (org-capture-target-buffer
               (e/org-roam-node-file-name)))
  (org-id-get-create)
  (goto-char (point-max))
  (insert "#+title:\n\n"))

(defun e/org-roam-node-file-name-ask ()
  (interactive)
  (let (( title (read-string "Title:")))
    (set-buffer (org-capture-target-buffer
                 (e/org-roam-node-file-name title)))
    (org-id-get-create)
    (goto-char (point-max))
    (insert "#+title: " title "\n"))
  (goto-char (point-max)))

(defun e/org-roam-node-from-file ( file)
  (when (org-roam-file-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-roam-node-at-point))))


(defun e/org-roam-node-read--filter-fn ( node)
  (not (string= (file-name-directory (org-roam-node-file node))
                (expand-file-name org-roam-dailies-directory
                                  org-roam-directory))))


(defun e/org-roam-parse-read ( read)
  (when (string-blank-p read)
    (user-error "Empty title not allowed"))
  (when (string-match "\\(.*[^\s)]\\)\s*\\($\\|\s(\\(.*\\))$\\)" read)
    (if (match-string 3 read)
        (cons (match-string 1 read)
              (concat (file-name-as-directory org-roam-directory)
                      (match-string 3 read) ".org"))
      (cons (match-string 1 read) nil))))

(defun e/org-roam-node-open ()
  (interactive)
  (let* (( nodes (org-roam-node-read--completions 'e/org-roam-node-read--filter-fn))
         ( alias (when (region-active-p)
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))))
         ( case-fold-search 'ignore-case)
         ( title-path (e/org-roam-parse-read
                       (completing-read "Search Nodes: "
                                        nodes nil nil alias)))
         ( title (car title-path))
         ( path (cdr title-path))
         ( node (or (e/org-roam-node-from-file path)
                    (org-roam-node-create :title title))))
    (org-roam-capture- :goto '(4) :keys "d" :node node)
    (when (and alias
               (not (string= (downcase alias) (downcase title)))
               (y-or-n-p (concat "Add \"" alias "\" to node \"" title "\":ROAM_ALIASES")))
      (org-roam-property-add "ROAM_ALIASES" alias))))

(defun e/org-roam-base-buffer-p ( &optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (let (( buffers (buffer-list))
        found)
    (while (and buffers (not found))
      (when (eq buffer (buffer-base-buffer (pop buffers)))
        (setq found t)))
    found))

(defun e/org-roam-visited-files ()
  (let ( files)
    (dolist ( buffer (buffer-list))
      (when (org-roam-buffer-p buffer)
          (setq files (cons (buffer-file-name buffer) files))))
    files))

(defun e/org-roam-node-create-copy-backup ()
  (interactive)
  (let* (( id (kill-new (org-id-get-create)))
         ( node (org-roam-node-from-id id))
         ( abbrevs (when node
                     (cdr (assoc "ABBREVIATIONS"
                                 (org-roam-node-properties node))))))
    (org-roam-property-add "ABBREVIATIONS"
                           (read-string (concat "Add Abbreviation"
                                                (when abbrevs (concat " (" abbrevs ")"))
                                                ": ")))
    (save-buffer)))


(defun e/org-roam-node-create-copy ()
  (interactive)
  (let (( id (kill-new (org-id-get-create))))
    (save-buffer)
    id))


(defun e/org-roam-node-other-window ()
  (when (= (length (window-list)) 1)
    (split-window))
  (other-window 1))

(defun e/org-roam-node-new ()
  (interactive)
  (let ( beg end substring)
    (when (region-active-p)
      (setq beg (region-beginning)
            end (region-end)
            substring (buffer-substring-no-properties beg end))
      (delete-region beg end))
    (let* (( id (org-id-uuid))
           ( title (read-string "Node Title: "))
           ( node (org-roam-node-create :id id
                                        :title title)))
      (e/org-roam-node-other-window)
      (org-roam-capture- :keys "z" :node node)
      (when substring
        (goto-char (point-max))
        (insert "\n" substring)))))

(defun e/org-roam-increment-title ( title)
  (let* (( list (nreverse (split-string title "[.]")))
         ( number (string-to-number (pop list))))
    (string-join (nreverse (cons (number-to-string (1+ number))
                                 list))
                 ".")))

(defun e/org-roam-node-new-next ()
  (interactive)
  (let* (( prev-id (save-excursion
                     (goto-char (point-min))
                     (org-id-get-create)))
         ( prev-title (cadar (org-collect-keywords '("TITLE"))))
         ( id (org-id-uuid))
         ( next-p (save-excursion
                    (beginning-of-line)
                    (looking-at "^#\\+next:\s*$")))
         ( title (if next-p
                     (e/org-roam-increment-title prev-title)
                   (concat prev-title ".1"))))
    (insert (org-link-make-string (concat "id:" id) title))
    (e/org-roam-node-other-window)
    (org-roam-capture- :keys "z" :node (org-roam-node-create :id id
                                                             :title title))
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:" nil t)
      (beginning-of-line)
      (insert "#+prev: "
              (org-link-make-string (concat "id:" prev-id)
                                    prev-title)
              "\n#+next:\n"))))

(defun e/org-roam-node-new-link ()
  (interactive)
  (let ( beg end substring)
    (when (region-active-p)
      (setq beg (region-beginning)
            end (region-end)
            substring (buffer-substring-no-properties beg end))
      (delete-region beg end))
    (let* (( id (org-id-uuid))
           ( title (read-string "Node Title: " substring))
           ( buffer (current-buffer))
           ( node (org-roam-node-create :id id
                                        :title title)))
      (insert (org-link-make-string (concat "id:" id) title))
      (e/org-roam-node-other-window)
      (org-roam-capture- :keys "z" :node node)
      (save-buffer)
      (with-current-buffer buffer
        (save-buffer)))))

(defun e/org-roam-node-extract ()
  (interactive)
  (org-back-to-heading-or-point-min)
  (let* (( beginning (point))
         ( title (nth 4 (org-heading-components)))
         ( prop-substring (if (re-search-forward org-property-drawer-re nil t)
                              (buffer-substring-no-properties (match-beginning 0)
                                                              (match-end 0))
                            (user-error "No Property Drawer At Source")))
         ( beg (1+ (point)))
         ( end (or (outline-get-next-sibling) (point)))
         ( substring (buffer-substring-no-properties beg end))
         ( new-node (org-roam-node-create :title title)))
    (delete-region beginning end)
    (save-buffer)
    (org-roam-capture- :keys "z" :node new-node)
    (goto-char (point-min))
    (if (re-search-forward org-property-drawer-re nil t)
        (delete-region (match-beginning 0)  (match-end 0))
      (user-error "No Property Drawer At Target"))
    (insert prop-substring)
    (goto-char (point-max))
    (insert substring)
    (save-buffer)))

(defvar e/org-property-collection
  '("value_shift" "polarity_shift"
    "event" "onstage" "offstage" "conflict"))

(defun e/org-set-property ()
  (interactive)
  (let* (( prop (completing-read "Property: " e/org-property-collection))
         ( value (read-string (concat prop " value: "))))
    (org-set-property prop value)))

(defun e/org-enable-minor-modes ()
  (let (( modes (org-entry-get (point-min) "minor_modes")))
    (when modes
      (dolist ( mode (split-string modes))
        (funcall (intern mode))))))

(defun e/org-roam-auto-links-section ( &optional _)
  (when (or (not (buffer-file-name))
            (org-roam-file-p))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\s+Links" nil t)
        (unless (org-entry-get (point) "HTML_CONTAINER_CLASS")
          (org-set-property "HTML_CONTAINER_CLASS" "link-section"))))))

(provide 'org-roam-custom)

;;; org-roam-custom.el ends here
