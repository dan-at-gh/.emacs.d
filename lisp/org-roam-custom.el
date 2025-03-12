;;; org-roam-custom --- Org Roam customization

;;; Commentary:
;; Load this package with:
;; (require 'org-roam-custom)

;;; Code:

(require 'org-roam)
(require 'org-roam-dailies)
(require 'ivy)


;;**** Set variables, hook, ivy usage

(defvar org-roam-html-server-url "")
(defvar e/org-roam-capture-bypass nil)

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


(setq org-roam-directory (file-truename "~/.emacs.d/org-roam/")
      org-roam-html-server-url "http://127.0.0.1:8080/"
      org-roam-completion-everywhere t
      org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("s" "short default (no slug in filename)" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>.org"
                            "#+title: ${title}\n")
         :unnarrowed t
         :jump-to-captured nil
         :kill-buffer t)
        ("h" "short default (no slug in filename)" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>.org"
                            "#+title: ${title}\n#+filetags: :tree_note:")
         :unnarrowed t
         :jump-to-captured nil
         :kill-buffer t)
        ("z" "zettel" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>.org"
                            "#+title: ${title}\n")
         :unnarrowed t
         :empty-lines-before 1
         :immediate-finish t
         :jump-to-captured t
         :kill-buffer nil
         :no-save t)
        ("n" "node at point" plain "%?"
         :target (node "${id}")
         :unnarrowed t)
        ("r" "reference node" plain "%?"
         :target (file+head "refs/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t
         :empty-lines-before 1
         :immediate-finish t
         :jump-to-captured t
         :kill-buffer nil
         :no-save t))
      org-roam-node-display-template
      (concat (propertize "${title}" 'face 'bold)
              "${ntitle}"
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


;; Only for the org-roam-buffer:
;; (add-hook 'org-roam-mode-hook nil)


(defun e/ivy-org-roam-node-find-old ()
  (interactive)
  (ivy-mode 1)
  (unwind-protect
      (let (( initial-input (when (region-active-p)
                              (capitalize
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))))))
        (org-roam-node-find nil initial-input nil nil
                            :templates e/org-roam-capture-bypass))
    (ivy-mode -1)))


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


;; ( fdf )(
;; ( ( dfd ( dfd))


(defun e/ivy-org-roam-ref-find ()
  (interactive)
  (ivy-mode 1)
  (unwind-protect
      (org-roam-ref-find)
    (ivy-mode -1)))


(defun e/org-roam-fontify-buffers ()
  (dolist ( frame (frames-on-display-list))
    (dolist ( window (window-list frame))
      (when (org-roam-buffer-p (window-buffer window))
        (with-selected-window window
          (font-lock-flush)
          (font-lock-ensure))))))
    


;;**** backlinks and hierarchy


(defface e/org-roam-backlinks-begin
  '((((type x w32 mac))
     (:inherit org-drawer :height 90))
    (((type tty))
     (:foreground "blue")))
    "Face used to display Doxygen comments (light gray)."
    :group 'basic-faces)


(defface e/org-roam-backlinks-end
  '((((type x w32 mac))
     (:inherit org-drawer :height 90))
    (((type tty))
     (:foreground "blue")))
    "Face used to display Doxygen comments (light gray)."
    :group 'basic-faces)


(defun e/org-roam-overlay-control-at-mouse (event)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (let* (( event-start (event-start event))
         ( cmd (mouse-posn-property event-start 'cmd)))
    (funcall cmd)))


(defun e/org-roam-overlay-open-at-mouse (event)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (let (( id (mouse-posn-property (event-start event) 'id))
        ( point (mouse-posn-property (event-start event) 'point)))
    ;; (e/org-roam-open-link-id id nil point)

    (with-temp-buffer
      (insert (org-link-make-string (concat "id:" id)))
      (backward-char 3)
      (org-open-at-point))
    (when point
      (goto-char point))))


(defun e/org-roam-overlay-open-at-mouse-ow (event)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (let (( id (mouse-posn-property (event-start event) 'id))
        ( point (mouse-posn-property (event-start event) 'point)))
    (e/org-roam-open-link-id id 'other-window point)))


;; (defun e/org-roam-backlinks-anchor-at-point ()
;;   (org-previous-visible-heading 1)
;;   (let (( bound (e/org-roam-heading-end-pos)))
;;     (when (re-search-forward ":ID:" bound t)
;;       (re-search-forward ":END:\s*\n" bound t)
;;       (while (looking-at "#\\+")
;;         (beginning-of-line 2))
;;       (point))))


(defun e/org-roam-backlinks-anchor-at-point ()
  (goto-char (point-min))
  (if (re-search-forward "^#\sbacklinks\s*\n" nil t)
    `(,(match-beginning 0) . ,(match-end 0))
    (when (re-search-forward "^\s*$" nil t)
      (beginning-of-line))
    `(,(point) . ,(point))))


(defun e/org-roam-heading-end-pos ()
  (save-excursion
    (or (org-next-visible-heading 1) (point))))


(defun e/org-roam-backlinks-all ( &optional closed)
  (interactive)
  (e/org-roam-backlinks-remove-all)
  (goto-char (point-min))
  (while (re-search-forward ":ID:" nil t)
    (if closed
        (e/org-roam-backlinks-closed)
      (e/org-roam-backlinks))))


(defun e/org-roam-backlinks-remove-all ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'type 'backlink))


(defun e/org-roam-join ( l &optional split)
  (unless split
    (setq split " "))
  (let ( s)
    (dolist ( subs l)
      (setq s (concat s
                      (propertize split 'face 'success)
                      (propertize subs 'face 'dired-ignored))))
    s))


(defun e/org-roam-backlinks-remove-at-point ()
  (let (( beg-end (save-excursion
                    (org-previous-visible-heading 1)
                    (cons (point) (or (org-next-visible-heading 1)
                                      (point))))))
    (remove-overlays (car beg-end) (1+ (cdr beg-end))
                     'type 'backlink)))


(defun e/org-roam-backlinks-indent ()
  (save-excursion
    (org-previous-visible-heading 1)
    (if (org-before-first-heading-p)
        ""
      (make-string (+ (car (org-heading-components)) 1) ? ))))


(defun e/org-roam-backlinks-hierarchy ( title)
  (let (( level 0)
        text)
    (dolist ( parent (e/org-roam-hierarchy-title-expand title))
      (setq text (concat text
                         (when (> level 1)
                           (concat (make-string (* (1- level) 2) ? )))
                         (when (> level 0)
                           (propertize "↳ " 'face '(:foreground "black")))
                         (cond ((eq (cdr parent) 'current-id)
                                (propertize (car parent)
                                            'face '(:foreground "black"
                                                    :inherit 'bold)))
                               ((unless (cdr parent)
                                  (car parent)))
                                (t
                                 (propertize (car parent)
                                             'keymap map
                                             'face 'button
                                             'mouse-face 'highlight
                                             'id (cdr parent))))
                         "\n"))
      (setq level (1+ level)))
    text))
      

(defun e/org-roam-backlinks ()
  (interactive)
  (e/org-roam-backlinks-remove-at-point)
  (let* (( indent (e/org-roam-backlinks-indent))
         ( current-node (org-roam-node-at-point))
         ( current-title (org-roam-node-title current-node))
         ( backlinks (when current-node
                       (org-roam-backlinks-get current-node)))
         ( map (make-sparse-keymap))
         ( ctrl-map (make-sparse-keymap))
         ( anchor (e/org-roam-backlinks-anchor-at-point))
         ( overlay (make-overlay (car anchor) (cdr anchor)))
         ( text (concat indent (propertize "Backlinks ▼"
                                    'keymap ctrl-map
                                    'face 'e/org-roam-backlinks-begin
                                    'mouse-face 'highlight
                                    'cmd 'e/org-roam-backlinks-closed)
                        (propertize "\n" 'face 'e/org-roam-backlinks-begin)))
         ( window (selected-window))
         ( back-list (car (cdr (assoc window e/org-roam-id-list))))
         ( level 0)
         backlink-node backlink-properties)
    (define-key map (kbd "<mouse-1>") 'e/org-roam-overlay-open-at-mouse)
    (define-key map (kbd "<mouse-3>") 'e/org-roam-overlay-open-at-mouse-ow)
    (define-key ctrl-map (kbd "<mouse-1>") 'e/org-roam-overlay-control-at-mouse)
    (dolist ( backlink backlinks)
      (setq backlink-node (org-roam-backlink-source-node backlink)
            backlink-props (org-roam-backlink-properties backlink)
            backlink-point (org-roam-backlink-point backlink)
            text (concat text indent
                         (propertize (org-roam-node-title backlink-node)
                                     'keymap map
                                     'face 'button
                                     'mouse-face 'highlight
                                     'id (org-roam-node-id backlink-node)
                                     'point backlink-point)
                         ;; " "
                         ;; (e/org-roam-join (plist-get backlink-props
                         ;;                             :outline) "->")
                         "\n"))
      (setq text (concat text (e/org-roam-backlinks-hierarchy
                               (org-roam-node-title backlink-node)))))
    (unless backlinks
      (setq text (concat text
                         indent
                         (propertize "ROOT NODE: No Backlinks.\n" 'face 'warning))))
    ;; (dolist ( backlink back-list)
    ;;   (setq text (concat text
    ;;                      indent
    ;;                      (propertize "->" 'face 'success)
    ;;                      (propertize (nth 1 backlink)
    ;;                                  'keymap map
    ;;                                  'face 'button
    ;;                                  'mouse-face 'highlight
    ;;                                  'id (nth 0 backlink)))))
    ;; (unless back-list
    ;;   (setq text (concat text
    ;;                      indent
    ;;                      (propertize "WINDOW: No Past Links." 'face 'warning))))
    ;; (setq text (concat text "\n"))
    (setq text (concat text
                       indent
                       (propertize "▲\n" 'face 'e/org-roam-backlinks-end)))
    (if (= (car anchor) (cdr anchor))
        (overlay-put overlay 'after-string text)
      (overlay-put overlay 'display text))
    (overlay-put overlay 'type 'backlink)))


(defun e/org-roam-backlinks-closed ()
  (interactive)
  (e/org-roam-backlinks-remove-at-point)
  (let* (( indent (e/org-roam-backlinks-indent))
         ( ctrl-map (make-sparse-keymap))
         ( anchor (e/org-roam-backlinks-anchor-at-point))
         ( overlay (make-overlay (car anchor) (cdr anchor)))
         ( text (concat indent
                        (propertize "Backlinks ▶"
                                    'keymap ctrl-map
                                    'face 'e/org-roam-backlinks-begin
                                    'mouse-face 'highlight
                                    'cmd 'e/org-roam-backlinks)
                        (propertize "\n" 'face 'e/org-roam-backlinks-begin))))
    (define-key ctrl-map (kbd "<mouse-1>") 'e/org-roam-overlay-control-at-mouse)
    (if (= (car anchor) (cdr anchor))
        (overlay-put overlay 'after-string text)
      (overlay-put overlay 'display text))
    (overlay-put overlay 'type 'backlink)))


(defun e/org-roam-hierarchy-anchor ()
  (goto-char (point-min))
  (if (re-search-forward "^#\shierarchy\s*\n" nil t)
    `(,(match-beginning 0) . ,(match-end 0))
    (when (re-search-forward "^\s*$" nil t)
      (beginning-of-line))
    `(,(point) . ,(point))))


(defun e/org-roam-hierarchy ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'type 'hierarchy)
  (let* (( current-node (org-roam-node-at-point))
         ( current-title (org-roam-node-title current-node))
         ( map (make-sparse-keymap))
         ( ctrl-map (make-sparse-keymap))
         ( anchor (e/org-roam-hierarchy-anchor))
         ( overlay (make-overlay (car anchor) (cdr anchor)))
         ( text (concat (propertize "Hierarchy ▼"
                                    'keymap ctrl-map
                                    'face 'e/org-roam-backlinks-begin
                                    'mouse-face 'highlight
                                    'cmd 'e/org-roam-hierarchy-closed)
                        (propertize "\n" 'face 'e/org-roam-backlinks-begin)))
         ( level 0))
    (define-key map (kbd "<mouse-1>") 'e/org-roam-overlay-open-at-mouse)
    (define-key map (kbd "<mouse-3>") 'e/org-roam-overlay-open-at-mouse-ow)
    (define-key ctrl-map (kbd "<mouse-1>") 'e/org-roam-overlay-control-at-mouse)
    (dolist ( parent (e/org-roam-hierarchy-title-expand current-title))
      (setq text (concat text
                         (when (> level 1)
                           (concat (make-string (* (1- level) 2) ? )))
                         (when (> level 0)
                           (propertize "↳ " 'face '(:foreground "black")))
                         (cond ((eq (cdr parent) 'current-id)
                                (propertize (car parent)
                                            'face '(:foreground "black"
                                                    :inherit 'bold)))
                               ((unless (cdr parent)
                                  (car parent)))
                                (t
                                 (propertize (car parent)
                                             'keymap map
                                             'face 'button
                                             'mouse-face 'highlight
                                             'id (cdr parent))))
                         "\n"))
      (setq level (1+ level)))
    ;; (setq text (concat text "\n"
    ;;                    (propertize "▲\n" 'face 'e/org-roam-backlinks-end)))
    (if (= (car anchor) (cdr anchor))
        (overlay-put overlay 'after-string text)
      (overlay-put overlay 'display text))
    (overlay-put overlay 'type 'hierarchy)))


(defun e/org-roam-hierarchy-closed ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'type 'hierarchy)
  (let* (( ctrl-map (make-sparse-keymap))
         ( anchor (e/org-roam-hierarchy-anchor))
         ( overlay (make-overlay (car anchor) (cdr anchor)))
         ( text (concat (propertize "Hierarchy ▶"
                                    'keymap ctrl-map
                                    'face 'e/org-roam-backlinks-begin
                                    'mouse-face 'highlight
                                    'cmd 'e/org-roam-hierarchy)
                        (propertize "\n" 'face 'e/org-roam-backlinks-begin))))
    (define-key ctrl-map (kbd "<mouse-1>") 'e/org-roam-overlay-control-at-mouse)
    (if (= (car anchor) (cdr anchor))
        (overlay-put overlay 'after-string text)
      (overlay-put overlay 'display text))
    (overlay-put overlay 'type 'hierarchy)))


(define-minor-mode org-roam-hierarchy-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " hi" :keymap nil
  (unless (org-roam-file-p)
    (user-error "Not in a ORoam buffer."))
  (cond (org-roam-hierarchy-mode
         (e/org-roam-hierarchy)
         (e/org-roam-backlinks-closed))
        (t
         (remove-overlays (point-min) (point-max) 'type 'backlink)
         (remove-overlays (point-min) (point-max) 'type 'hierarchy))))


(defun e/org-roam-hierarchy-title-expand ( node-title &optional backlink)
  (let (( color "#add8e6") ;; default light blue
        s id parents)
    (dolist ( atom (split-string node-title "\\."))
      (setq s (concat s (and s ".") atom)
            id (caar (org-roam-db-query [:select [id] :from nodes
                                       :where (= title $s1)]
                                        s)))
      (let (( node (when id
                     (org-roam-populate
                      (org-roam-node-create :id id)))))
        (setq parents (cons (if node
                                `(,(e/org-roam-note-join
                                    (org-roam-node-aliases node)
                                    " | " "[0-9]+:")
                                  . ,(if (and (string= s node-title)
                                              (not backlink))
                                         'current-id
                                       id))
                              `(,atom . nil))
                            parents)
              color (or (when node
                          (cdr (assoc "BACKGROUND_COLOR"
                                      (org-roam-node-properties node))))
                        color))))
    (list color (nreverse parents))))


(defun e/org-roam-hierarchy-insert ( title &optional backlink)
  (let* (( level 0)
         ( color-parents (e/org-roam-hierarchy-title-expand title backlink))
         ( color (car color-parents))
         ( parents (cadr color-parents)))
    (dolist ( parent parents)
      (when (> level 1)
        (insert (make-string (* (1- level) 2) ? )))
      (when (> level 0)
        (insert "↳ "))
      (insert (cond ((and (eq (cdr parent) 'current-id)
                          (not backlink))
                     (propertize (car parent)
                                 'font-lock-face 'bold))
                    ((unless (cdr parent)
                       (car parent)))
                    (t
                     (org-link-make-string (concat "id:" (cdr parent))
                                           (car parent))))
              "\n")
      (setq level (1+ level)))
    (when backlink
      (insert (make-string (* (1- level) 2) ? ))
      (insert "↳ ")
      (let (( source-node (org-roam-backlink-source-node backlink))
            ( source-point (org-roam-backlink-point backlink)))
        (insert (org-link-make-string
                 (concat "id-point:"
                         (org-roam-node-id source-node) " "
                         (number-to-string source-point))
                 (e/org-roam-hierarchy-backlink-description backlink)))))
    (insert "\n")
    color))

    
(defun e/org-roam-hierarchy-backlink-description ( backlink)
  (let* (( source-node (org-roam-backlink-source-node backlink))
         ( source-id (org-roam-node-id source-node))
         ( buffers (buffer-list))
         ( m (org-roam-id-find source-id 'marker))
         ( buffer (marker-buffer m))
         description)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (org-roam-backlink-point backlink))
        (let (( element (org-element-context)))
          (setq description (buffer-substring-no-properties
                             (org-element-property :contents-begin element)
                             (org-element-property :contents-end element))))))
    (unless (member buffer buffers)
      (kill-buffer buffer))
    (or description source-id)))
              

(defun e/org-roam-hierarchy-level-count ( title)
  (if title
      (let (( orig (length title))
            ( repl (replace-regexp-in-string "\\." "" title)))
        (1+ (- orig (length repl))))
    1))


(defun e/org-roam-hierarchy-buffer ()
  (let* (( inhibit-read-only t)
         ( node (org-roam-node-at-point))
         ( id (org-roam-node-id node))
         ( hierarchy-buffer (concat "*" id "*"))
         ( title (org-roam-node-title node))
         ( aliases (split-string-and-unquote
                    (or (org-entry-get (point-min) "ROAM_ALIASES") "")))
                   ;; (car (org-property-values "ROAM_ALIASES"))
         ( backlinks (org-roam-backlinks-get node))
         ( color (org-entry-get (point-min) "BACKGROUND_COLOR"))
         ( window (get-buffer-window))
         color)
    (if (get-buffer-window hierarchy-buffer)
        (pop-to-buffer hierarchy-buffer)
      (setq window
            (split-window-vertically
             (+ (e/org-roam-hierarchy-level-count title)
                3)))
      (switch-to-buffer hierarchy-buffer
                        'NORECORD 'FORCE-SAME-WINDOW))
    (erase-buffer)
    (remove-overlays)
    (org-mode)    
    (setq cursor-type nil
          mode-line-format "")
    (set-window-fringes nil 0 0)
    (if title
        (setq color (e/org-roam-hierarchy-insert title))
      (insert (propertize
               (e/org-roam-note-join aliases " | " "[0-9]+:")
               'font-lock-face 'bold)
              "\n"))
    (insert (propertize "Backlinks "
                        'font-lock-face 'shadow)
            (number-to-string (length backlinks)) ":\n")
    (dolist ( backlink backlinks)
      (e/org-roam-hierarchy-insert
       (org-roam-node-title (org-roam-backlink-source-node backlink))
       backlink))
    (insert "\n")
    (let ((o (make-overlay (point-min) (point-max)
			               (current-buffer) nil t)))
	  (overlay-put o 'face `(:background ,color :extend t))
	  (overlay-put o 'hierarchy-buffer t))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (read-only-mode 1)
    (select-window window)))


(defun e/org-roam-hierarchy-buffer-show ( &optional only-init)
  (interactive)
  (when (and org-roam-hierarchy-buffer-mode
             e/org-roam-hierarchy-enable
             (get-buffer-window)
             (org-roam-id-at-point)
             (org-roam-node-title (org-roam-node-at-point))
             (org-roam-file-p))
    (if only-init
        (unless (get-buffer-window
                 (concat "*" (org-roam-id-at-point) "*"))
          (e/org-roam-hierarchy-buffer))
      (e/org-roam-hierarchy-buffer))))


(defun e/org-roam-hierarchy-buffer-close ()
  (let* (( node (org-roam-node-at-point))
         ( hierarchy-buffer (concat "*"
                                   (org-roam-node-id node)
                                   "*")))
    (delete-window (get-buffer-window  hierarchy-buffer))))


(defun e/org-roam-hierarchy-buffer-kill ()
  (when (and (org-roam-file-p)
             (org-roam-id-at-point))
    (let* (( node (org-roam-node-at-point))
           ( hierarchy-buffer (concat "*"
                                      (org-roam-node-id node)
                                      "*")))
      (when (get-buffer hierarchy-buffer)
        (kill-buffer hierarchy-buffer)))))


;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (e/org-roam-frame-thumbnail)
;;             (e/org-roam-hierarchy-buffer-show 'only-init)))


(setq e/org-roam-hierarchy-enable nil)


(add-hook 'desktop-after-read-hook
          (lambda ()
            (setq e/org-roam-hierarchy-enable t)))


(define-minor-mode org-roam-hierarchy-buffer-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " hierarchy" :keymap nil
  (unless (org-roam-file-p)
    (user-error "Hierarchy: Not in a Org Roam buffer."))
  (cond (org-roam-hierarchy-buffer-mode
         (e/org-roam-hierarchy-buffer-show)
         (add-hook 'after-save-hook
                   'e/org-roam-hierarchy-buffer-show nil 'local)
         (add-hook 'kill-buffer-hook
                   'e/org-roam-hierarchy-buffer-kill nil 'local)
         )
        (t
         (remove-hook 'kill-buffer-hook
                      'e/org-roam-hierarchy-buffer-kill 'local)
         (remove-hook 'after-save-hook
                      'e/org-roam-hierarchy-buffer-show 'local)
         (e/org-roam-hierarchy-buffer-close))))


;;**** utility functions


(setq e/org-roam-max-slug-length 20)


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
    (user-error "Empty title not allowed."))
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


(defun e/org-roam-other-window-buffer-list ()
  (let ( buffers)
    (dolist ( window (window-list))
      (let* (( window-buffer (window-buffer window))
             ( mode (buffer-local-value 'major-mode window-buffer)))
        (when (and (or (org-roam-buffer-p window-buffer)
                       (eq mode 'oroam-mode))
                   (not (eq window-buffer (current-buffer))))
          (setq buffers (cons window-buffer buffers)))))
    buffers))


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


(defun e/org-roam-push-file ()
  (interactive)
  (let* (( path (buffer-file-name))
         ( directory (file-name-directory path))
         ( file (file-name-nondirectory path))
         ( basename (file-name-base file))
         ( roam-path (read-file-name "Roam File: "
                                     (file-name-as-directory org-roam-directory)
                                     nil nil
                                     (concat (format-time-string "%Y%m%d%H%M%S")
                                             "-" basename ".org"))))
    (find-file roam-path)
    (insert "#+title: " (upcase-initials
                         (replace-regexp-in-string "-\\|_" " "
                         basename))
            "\n\n")
    (insert-file-contents path)
    (goto-char (point-min))
    (org-id-get-create)
    (e/org-roam-backlinks-closed)))


(defun e/org-roam-frame-title ()
  (save-excursion
    (goto-char (point-min))
    (when-let (( node (org-roam-node-at-point)))
      (set-frame-name
       (e/org-roam-note-join (org-roam-node-aliases node)
                             " | " "[0-9]+:")))))


(defun e/org-roam-frame-thumbnail ()
  (when (and (org-roam-file-p)
             (get-buffer-window)
             (org-roam-id-at-point)
             (org-roam-node-title (org-roam-node-at-point))
             e/org-roam-hierarchy-enable)
    (when-let (( node (org-roam-node-at-point)))
      (let* (( aliases (e/org-roam-note-join
                        (or (org-roam-node-aliases node) '("No Alias"))
                                             " | " "[0-9]+:"))
             ( color (car (e/org-roam-hierarchy-title-expand
                           (org-roam-node-title node))))
             ( output (shell-command-to-string
                       "xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW"))
             ( window-id (when (string-match "[\s\t]\\(0x[0-9a-f]+\\)"
                                             output)
                           (match-string-no-properties 1 output)))
             ( file (expand-file-name (concat "~/.fvwm/roam-icons/icon.tmp."
                                              window-id ".png"))))
        (shell-command (concat "convert -background '" color "'"
                               " -fill black"
                               " -pointsize 12 label:\"" aliases "\""
                               " " file))))))


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
         ( id (org-id-get-create))
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
    ;; (when (y-or-n-p "Insert Link? ")
    ;;   (insert (org-link-make-string (concat "id:" id) title) "\n\n")
    ;;   (save-buffer))
    (org-roam-capture- :keys "z" :node new-node)
    (goto-char (point-min))
    (if (re-search-forward org-property-drawer-re nil t)
        (delete-region (match-beginning 0)  (match-end 0))
      (user-error "No Property Drawer At Target"))
    (insert prop-substring)
    (goto-char (point-max))
    (insert substring)
    (save-buffer)))
    

(defun e/org-roam-copy-id-after-save ()
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))
      (kill-new (org-id-get-create)))))


;; (add-hook 'after-save-hook 'e/org-roam-copy-id-after-save nil 'local)


;;**** directed graph


(defun e/org-roam-directed-edge-property-add ( node-point to-from id)
  (save-excursion
    (let (( prop-value (org-entry-get node-point to-from))
          found)
      (when prop-value
        (setq found (member id
                            (split-string prop-value))))
      (unless found
        (goto-char node-point)
        (re-search-forward org-property-end-re nil t)
        (beginning-of-line)
        (insert ":" to-from "+: " id "\n")))))


(defun e/org-roam-directed-edge-remove-prop ( id to-from to-from-id)
  (goto-char (point-min))
  (re-search-forward (concat ":ID:\s+" id) nil t)
  (let (( found t))
  (while found
    (let (( limit (re-search-forward org-property-end-re nil t)))
      (org-back-to-heading-or-point-min 'invisible-ok)
      (if (re-search-forward (concat ":" to-from "\\+?:\s+" to-from-id)
                             limit t)
          (delete-region (line-beginning-position)
                         (line-beginning-position 2))
        (setq found nil))))))


(defun e/org-roam-directed-edge-outline-path ()
  (let (( path (org-get-outline-path)))
    (string-join (add-to-list 'path
                 (nth 4 (org-heading-components))
                 'append) "/")))


(defun e/org-roam-directed-edge-active-id ()
  (let (( overlays (overlays-in (point-min) (point-max)))
        root-id active-ids)
    (save-excursion
      (dolist ( overlay overlays)
        (when (eq (overlay-get overlay 'type) 'directed-root)
          (goto-char (overlay-start overlay))
          (setq root-id (overlay-get overlay 'directed-root)
                active-ids (cons (cons (e/org-roam-directed-edge-outline-path)
                                       (e/org-roam-directed-edge-heading-id))
                                 active-ids)))))
    ;; (while (and overlays (not root-id))
    ;;   (setq root-id (overlay-get (pop overlays) 'directed-root)))
    (when root-id
      (list root-id active-ids))))


(defun e/org-roam-directed-edge-add-ask ()
  (interactive)
  (let* (( active-ids (e/org-roam-directed-edge-active-id))
         ( root-id (car active-ids))
         ( from-id (or (when active-ids
                         (cdr (assoc (completing-read "Connect to Node: "
                                                      (cadr active-ids))
                                     (cadr active-ids))))
                       (car active-ids)
                       ;; (car (e/org-roam-directed-edge-active-id))
                       (current-kill 0)))
         ( to-id (org-id-get-create))
         ( from-m (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward (concat ":ID:\s+" from-id) nil t)
                        (org-back-to-heading-or-point-min 'invisible-ok)
                      (user-error "Source ID Not Found.")))))
    (e/org-roam-directed-edge-property-add (point) "FROM" from-id)
    (e/org-roam-directed-edge-property-add from-m "TO" to-id)
    (e/org-roam-directed-edge-show nil root-id)))


(defun e/org-roam-directed-edge-add ()
  (interactive)
  (let* (( root-id (car (e/org-roam-directed-edge-active-id)))
         ( from-id (or root-id
                       (current-kill 0)))
         ( to-id (org-id-get-create))
         ( from-m (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward (concat ":ID:\s+" from-id) nil t)
                        (org-back-to-heading-or-point-min 'invisible-ok)
                      (user-error "Source ID Not Found.")))))
    (e/org-roam-directed-edge-property-add (point) "FROM" from-id)
    (e/org-roam-directed-edge-property-add from-m "TO" to-id)
    (e/org-roam-directed-edge-show nil root-id)))


(defun e/org-roam-directed-edge-heading ()
  (save-excursion
    (org-back-to-heading-or-point-min 'invisible-ok)
    (point)))


(defun e/org-roam-directed-edge-path ( &optional root-id)
  (when root-id
    (goto-char (point-min))
    (re-search-forward (concat ":ID:\s+" root-id) nil t))
  (let ( buffer-to-ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ":ID:\s+\\([0-9a-z-]+\\)" nil t)
        (setq buffer-to-ids (cons (cons (match-string-no-properties 1)
                                        (e/org-roam-directed-edge-heading))
                                  buffer-to-ids))))
    (let* (( points (list (e/org-roam-directed-edge-heading)))
           ( all-points points)
           id-point next-points)
      (while points
        (dolist ( point points)
          (let (( to-ids (split-string (or (org-entry-get point "TO")
                                           ""))))
            (dolist ( id to-ids)
              (setq id-point (assoc id buffer-to-ids)
                    next-points (cons (cdr id-point) next-points)))))
        (setq points next-points
              all-points (append points all-points)
              next-points nil))
      (delq nil (delete-dups all-points)))))


(defun e/org-roam-directed-edge-heading-id ()
  (save-excursion
    (org-back-to-heading-or-point-min 'invisible-ok)
    (when (re-search-forward ":ID:\s+\\([0-9a-z-]+\\)"
                             (save-excursion
                               (or (outline-next-heading) (point-max)))
                             t)
      (match-string-no-properties 1))))


(defun e/org-roam-directed-edge-show ( overview &optional root-id entry children)
  (interactive "P")
  (unless root-id
    (setq root-id 
          (or (org-entry-get (point) "ID")
              (if (y-or-n-p "Create Directed Graph Node? ")
                  (org-id-get-create)
                (user-error "No Node at Point.")))))
  (kill-new root-id)
  (remove-overlays (point-min) (point-max) 'type 'directed-root)
  (when overview
    (org-overview))
  (save-excursion
    (dolist ( point (e/org-roam-directed-edge-path root-id))
      (goto-char point)
      (let (( ov (make-overlay point (line-end-position)))
            ( id (e/org-roam-directed-edge-heading-id) ))
        (overlay-put ov 'face (if (string= id root-id)
                                  'secondary-selection
                                'highlight))
        (overlay-put ov 'type 'directed-root)
        (overlay-put ov 'directed-root root-id)
        (overlay-put ov 'directed-id (org-entry-get point "ID")))
      (org-show-context nil)
      (when children
        (org-show-children))
      (when entry
        (org-show-entry)))))


(defun e/org-roam-directed-edge-up-heading ()
  (when (> (car (org-heading-components)) 1)
    (outline-up-heading 1)))


(defun e/org-roam-directed-edge-show-mouse ( event &optional overview entry children)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (if (outline-on-heading-p)
          (e/org-roam-directed-edge-show overview nil entry children)
        (remove-overlays (point-min) (point-max) 'type 'directed-root)))))


(defun e/org-roam-directed-edge-show-overview-mouse ( event)
  (interactive "e")
  (e/org-roam-directed-edge-show-mouse event 'overview)
  (save-excursion
    (e/org-roam-directed-edge-up-heading)
    (org-show-children)))


(defun e/org-roam-directed-edge-show-entries-mouse ( event)
  (interactive "e")
  (e/org-roam-directed-edge-show-mouse event 'overview 'entry))


(defun e/org-roam-directed-edge-show-children-mouse ( event)
  (interactive "e")
  (e/org-roam-directed-edge-show-mouse event 'overview 'entry 'children))


(defun e/org-roam-directed-edge-remove ()
  (interactive)
  (let (( id (org-roam-id-at-point))
        ( overlays (overlays-at (point)))
        root-id del-from-ids)
    (while (and overlays (not root-id))
      (setq root-id (overlay-get (pop overlays) 'directed-root)))
    (unless root-id
      (user-error "No Root ID Found."))
    ;; (goto-char (org-roam-id-find root-id 'marker))
    (dolist ( point (e/org-roam-directed-edge-path root-id))
      (when (member id (split-string (or (org-entry-get point "TO")
                                         "")))
        (goto-char point)
        (setq del-from-ids (cons (org-entry-get point "ID") del-from-ids))))
    (dolist ( del-from-id del-from-ids)
      (e/org-roam-directed-edge-remove-prop del-from-id "TO" id)
      (e/org-roam-directed-edge-remove-prop id "FROM" del-from-id))
    (e/org-roam-directed-edge-show nil root-id)))


(defun e/org-roam-directed-edge-remove-broken ()
  (interactive)
  (let ( ids del-lines)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ":ID:\s+\\([0-9a-z-]+\\)" nil t)
        (setq ids (cons (match-string-no-properties 1) ids)))
      (goto-char (point-min))
      (while (re-search-forward ":\\(FROM\\|TO\\)\\+?:\s+\\([0-9a-z-]+\\)" nil t)
        (unless (member (match-string-no-properties 2) ids)
          (setq del-lines (cons (cons (line-beginning-position)
                                      (line-beginning-position 2))
                                del-lines)))))
    (when (y-or-n-p (concat "Remove " (number-to-string (length del-lines))
                            " Lines? "))
      (dolist ( beg-end del-lines)
        (delete-region (car beg-end) (cdr beg-end))))))
    

;;**** open link


(setq e/org-roam-id-list nil
      e/org-roam-id-list-length 10)


(defun e/org-roam-open-link-id ( link-id &optional other-window point)
  (let* (( initial-window (selected-window))
         ( window-state t)
         ( m (org-roam-id-find link-id 'marker))
         ( buffer (marker-buffer m))
         kill-candidate )
    (when other-window
      (other-window 1)
      (setq window-state (not (eq (selected-window) initial-window))))
    (when (and (org-roam-buffer-p)
               window-state)
      (setq kill-candidate (current-buffer))
      (when (or (buffer-modified-p)
                (> (length (get-buffer-window-list kill-candidate nil t)) 1)
                (e/org-roam-base-buffer-p kill-candidate))
        (setq kill-candidate nil))
      (let* (( back-list-window (selected-window))
             ( back-list-node (org-roam-node-at-point))
             ( back-list-id (org-roam-node-id back-list-node))
             ( back-list-title (org-roam-node-title back-list-node))
             ( back-list (cons (list back-list-id back-list-title)
                               (car (cdr (assoc back-list-window e/org-roam-id-list))))))
        (setq e/org-roam-id-list (assoc-delete-all back-list-window e/org-roam-id-list))
        (when (> (length back-list) e/org-roam-id-list-length)
          (setq back-list (butlast back-list)))
        (setq e/org-roam-id-list (cons (list back-list-window back-list) e/org-roam-id-list))))
    (if window-state
        (switch-to-buffer buffer)
      (switch-to-buffer-other-window buffer))
    (when (e/org-roam-dailies--daily-note-p)
      (run-hooks 'org-roam-dailies-find-file-hook))
    (if point
        (goto-char point)
      (goto-char m))
    (select-window initial-window)
    (when (and kill-candidate
               (= (length (get-buffer-window-list kill-candidate)) 0))
      (kill-buffer kill-candidate))))


(defun e/org-roam-open-link ( &optional other-window)
  (let* (( link (org-element-context))
         ( key (org-element-property :key link))
         ( value (org-element-property :value link))
         ( link-type (org-element-property :type link))
         ( link-id (org-element-property :path link)))
    (if (string= link-type "id")
        (e/org-roam-open-link-id link-id other-window)
      (if (string= key "ROAM_REFS")
          (let* (( ref-file (save-excursion
                              (re-search-backward "\s"
                                                  (line-beginning-position)
                                                  nil)
                              (when (looking-at "\s\\(file:[^\s\n]+\\)")
                                (match-string 1)))))
            (if ref-file
                (with-temp-buffer
                  (insert (concat "[["
                                  (e/org-roam-link-url-decode ref-file)
                                  "]]"))
                  (backward-char 3)
                  (org-open-at-point))
              (org-open-at-point)))
        (org-open-at-point)))))


(defun e/org-roam-open-at-mouse ( ev)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (mouse-set-point ev)
  (e/org-roam-open-link))


(defun e/org-roam-open-at-mouse-ow ( ev)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (mouse-set-point ev)
  (e/org-roam-open-link 'other-window))


(defun e/org-roam-back-link ()
  (interactive)
  (when (not (org-roam-buffer-p))
    (user-error "Not in a roam buffer."))
  (let* (( current-buffer (current-buffer))
         ( window (selected-window))
         ( back-list (or (car (cdr (assoc window e/org-roam-id-list)))
                         (user-error "Back list is emtpy.")))
         ( id (car (pop back-list)))
         ( kill-candidate (unless (or (buffer-modified-p)
                                      (> (length (get-buffer-window-list)) 1)
                                      (e/org-roam-base-buffer-p))
                            (current-buffer))))
    (setq e/org-roam-id-list (assoc-delete-all window e/org-roam-id-list))
    (setq e/org-roam-id-list (cons (list window back-list) e/org-roam-id-list))
    (switch-to-buffer (marker-buffer (org-roam-id-find id 'marker)))
    (when (e/org-roam-dailies--daily-note-p)
      (run-hooks 'org-roam-dailies-find-file-hook))
    (when (and kill-candidate
               (= (length (get-buffer-window-list kill-candidate)) 0))
      (kill-buffer kill-candidate))))


(defun e/org-roam-link-aliases ( &optional downcase title)
  (let (( aliases (org-entry-get nil "ROAM_ALIASES")))
    (when downcase
      (setq aliases (downcase aliases)))
    (split-string-and-unquote aliases)))


(defun e/org-roam-link-yank ()
  (interactive)
  (let (( node (or (org-roam-node-from-id (current-kill 0))
                 (user-error "No Roam ID in current kill."))))
    (insert (org-link-make-string (concat "id:" (org-roam-node-id node))
                                  (org-roam-node-title node)))))


(defun e/org-roam-link-yank-short ()
  (interactive)
  (let* (( id (current-kill 0))
         ( node (or (org-roam-node-from-id id)
                    (user-error "No Roam ID in current kill.")))
         ( title (org-roam-node-title node))
         ( abbrevs (cdr (assoc "ABBREVIATIONS"
                               (org-roam-node-properties node))))
         ;; ( short (when (string-match "[^.]+$" title)
         ;;           (match-string 0 title)))
         ( short (if abbrevs
                     (car (split-string-and-unquote abbrevs))
                   title)))
    (insert (org-link-make-string (concat "id:" id)
                                  (concat "[" short "]")))))


(defun e/org-roam-link-yank-abbr ()
  (interactive)
  (let* (( id (current-kill 0))
         ( node (or (org-roam-node-from-id id)
                    (user-error "No Roam ID in current kill.")))
         ( title (org-roam-node-title node))
         ( short (when (string-match "[^.]+$" title)
                   (match-string 0 title))))
    (insert (org-link-make-string (concat "id:" id)
                                  (concat "[" short "]")))))


(defun e/org-roam-link-heading-other-window ()
  (interactive)
  (when (not (org-roam-buffer-p))
    (user-error "Not in a roam buffer."))
  (let (( buffers (or (e/org-roam-other-window-buffer-list)
                      (user-error "No other window with roam buffers.")))
        id beg end title alias)
    (when (region-active-p)
      (setq beg (region-beginning)
            end (region-end)
            alias (buffer-substring-no-properties beg end))) 
    (with-current-buffer (car buffers)
      (setq id (org-id-get-create)
            title (if (org-before-first-heading-p)
                      (cadar (org-collect-keywords '("title")))
                    (nth 4 (org-heading-components))))
      (when (and (not (org-roam-id-find id))
                 (buffer-modified-p)
                 (y-or-n-p (format "ID not found. Save buffer %s?" (buffer-name))))
        (save-buffer))
      (when (and alias
                 (not (member (downcase alias)
                              (e/org-roam-link-aliases 'downcase title)))
                 (y-or-n-p (concat "Add \"" alias "\" to node \"" title "\":ROAM_ALIASES")))
          (org-roam-property-add "ROAM_ALIASES" alias)))
    (when alias
      (delete-region beg end))
    (insert (org-link-make-string (concat "id:" id) (or alias title)))))


(defun e/org-roam-link-remove ()
  (interactive)
  (unless (eq (car (org-element-context)) 'link)
    (org-next-link))
  (e/org-roam-link-copy-id)
  (let* (( link (org-element-context))
         ( begin (org-element-property :begin link))
         ( description-begin (org-element-property :contents-begin link))
         ( description-end (org-element-property :contents-end link))
         ( description (when description-begin
                         (buffer-substring-no-properties
                          description-begin description-end))))
    (delete-region (goto-char begin)
                   (re-search-forward org-link-bracket-re nil t))
    (when description
      (insert description))))
    

(defun e/org-roam-link-url-decode ( encoded-url)
  (replace-regexp-in-string "%20" " " encoded-url))


(defun e/org-roam-link-description ()
  (let* (( element (org-element-context))
         ( type (car element)))
    (when (eq type 'link)
      (let* (( begin (org-element-property :contents-begin element))
             ( end (org-element-property :contents-end element))
             ( id (org-element-property :path element))
             ( node (org-roam-node-from-id id)))
        (when node
          (let (( description (completing-read "Set link description: "
                                               (cons (org-roam-node-title node)
                                                     (org-roam-node-aliases node)))))
            (goto-char begin)
            (delete-region begin end)
            (insert description)))))))


(defun e/org-roam-link-ref-open ()
  "Open first bracket link from node contents without visiting.

This function can be used to bypass the step of visiting the node
in order to open the first link of the node's contents."
  (let* (( link-id (if (eq major-mode 'oroam-mode)
                       (get-text-property (point) 'id)
                     (org-element-property :path (org-element-context))))
         ( buffers (buffer-list))
         ( m (org-roam-id-find link-id 'marker))
         ( buffer (marker-buffer m)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (re-search-forward org-link-bracket-re nil t)
        (backward-char)
        (org-open-at-point)))
    (unless (member buffer buffers)
      (kill-buffer buffer))))


(defun e/org-roam-link-ref-open-at-mouse ( ev)
  "Open first bracket link from node contents without visiting.

This function can be used to bypass the step of visiting the node
in order to open the first link of the node's contents."
  (interactive "e")
  (mouse-set-point ev)
  (e/org-roam-link-ref-open))


(defun e/org-roam-link-copy-id ()
  (interactive)
  (let* (( element (org-element-context))
         ( id (cond ((and (eq (car element) 'link)
                          (string= (org-element-property :type element) "id"))
                     (org-element-property :path element))
                    ((or (eq major-mode 'oroam-mode)
                         (eq major-mode 'roam-tree-mode))
                     (get-text-property (point) 'id))
                    (t
                     (org-roam-id-at-point)))))
    (kill-new id)
    id))


(defun e/org-roam-link-next ( &optional previous)
  (cond ((plist-get (car (org-context)) :table)
         (let (( col (org-table-current-column)))
           (beginning-of-line (if previous 0 2))
           (org-table-goto-column col)
           (while (and (not (eq (car (org-element-context)) 'link))
                       (plist-get (car (org-context)) :table))
             (beginning-of-line (if previous 0 2))
             (org-table-goto-column col))))
        (t
         (org-next-link previous))))


(defun e/org-roam-link-scan ( &optional up)
  (interactive)
  (if org-roam-note-list-scan-mode
      (cond ((eq major-mode 'oroam-mode)
             (beginning-of-line (if up 0 2)))
            ((org-roam-buffer-p)
             (e/org-roam-link-next up)))
    (org-roam-note-list-scan-mode 1))
  (let (( id (e/org-roam-link-copy-id)))
    (when (= (length (window-list)) 1)
      (split-window))
    (e/org-roam-open-link-id id 'other-window)))


(defun e/org-roam-link-scan-up ()
  (interactive)
  (e/org-roam-link-scan 'up))


(define-minor-mode org-roam-note-list-scan-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " scan"
  :keymap 
  `(,(cons (kbd "RET") 'org-roam-note-list-scan-mode)
    ,(cons (kbd "<down>") 'e/org-roam-link-scan)
    ,(cons (kbd "<up>") 'e/org-roam-link-scan-up))
  (unless (or (eq major-mode 'oroam-mode)
              (org-roam-buffer-p))
    (user-error "Not in a ORoam buffer."))
  (cond (org-roam-note-list-scan-mode
         )
        (t
         )))

  
;;**** file repository


(setq org-roam-file-repository "/home/dan/library/org-roam")


(defun e/org-roam-file-repository-link ( path)
  (let* (( file (file-name-nondirectory path))
         ( directory (file-name-directory path))
         ( uuid (org-id-uuid))
         ( uuid-path (concat (file-name-as-directory org-roam-file-repository)
                             uuid "-" file)))
    (unless (string-empty-p file)
      (rename-file path uuid-path)
      (make-symbolic-link uuid-path path)
      uuid)))


(defun e/org-roam-file-repository-rename ( path)
  (let* (( file (file-name-nondirectory path))
         ( extension (file-name-extension path 'period))
         ( directory (file-name-directory path))
         ( uuid (org-id-uuid))
         ( uuid-path (concat (file-name-as-directory org-roam-file-repository)
                             uuid extension)))
    (unless (string-empty-p file)
      (rename-file path uuid-path)
      uuid)))


(defun e/org-roam-file-repository-node ( uuid)
  (let (( title (read-string "File Title: " nil nil "File Node")))
    (org-roam-capture- :goto nil
                       :keys "f"
                       :node (org-roam-node-create :title title)))
  )


(defun e/org-roam-file-repository-dired ()
  (e/org-roam-file-repository-node
   (e/org-roam-file-repository-rename (dired-get-filename))))


(defun e/org-roam-bibtex-entry-end-position ()
  (save-excursion
    (bibtex-end-of-entry)))


(defun e/org-roam-bibtex-field-value ( field value)
  (when (re-search-forward (concat field "\s*=\s*{")
                     (e/org-roam-bibtex-entry-end-position) t)
    (insert value)))


(defun e/org-roam-bibtex-insert-field ( field value)
  "Insert new field in current BibTeX entry."
  (interactive)
  (let (( valueColumn 17))
    (bibtex-end-of-entry)
    (beginning-of-line)
    (insert "\n")
    (beginning-of-line 0)
    (insert (format "  %s = " field))
    (when (< (current-column) valueColumn)
      (move-to-column valueColumn t))
    (insert (format "{%s}," value))))


(defun e/org-roam-dired-repository-link ()
  (let* (( path (dired-get-filename))
         ( attributes (file-attributes path 'string))
         ( atime (format-time-string "%Y-%m-%d %H:%M:%S"
                                     (file-attribute-access-time attributes)))
         ( mtime (format-time-string "%Y-%m-%d %H:%M:%S"
                                     (file-attribute-modification-time attributes)))
         ( stime (format-time-string "%Y-%m-%d %H:%M:%S"
                                     (file-attribute-status-change-time attributes)))
         ( uuid (e/org-roam-file-repository-link path)))
    (when uuid
      (revert-buffer)
      (find-file-other-window (car reftex-default-bibliography))
      (goto-char (point-max))
      (insert "\n")
      (bibtex-Misc)
      (bibtex-beginning-of-entry)
      (re-search-forward "@Misc{" (line-end-position) t)
      (insert uuid)
      (e/org-roam-bibtex-insert-field "atime" atime)
      (e/org-roam-bibtex-insert-field "mtime" mtime)
      (e/org-roam-bibtex-insert-field "stime" stime))))


(defun e/org-roam-recursive-file-nodes ()
  (dolist ( file (directory-files-recursively
                  org-roam-file-repository ".*"
                  'include-directories))
    (unless (string-match "org-roam-id:\s\\(.*\\)\n"
                          (shell-command-to-string
                           (concat "xattr -l "
                                   (shell-quote-argument file))))
      (shell-command (concat "xattr -w user.org-roam-id \""
                             (org-id-uuid) "\" " (shell-quote-argument file))))))
  

;;**** ref nodes handling


(setq e/org-roam-ref-directory "refs")


(defun e/org-roam-ref-buffer-p ( &optional buffer)
  (string= (concat (file-name-as-directory org-roam-directory)
                   (file-name-as-directory e/org-roam-ref-directory))
           (file-name-directory (buffer-file-name buffer))))


(defun e/org-roam-ref-node-p ( &optional node)
  (unless node
    (setq node (org-roam-node-at-point)))
  (org-roam-node-refs node))


(defun e/org-roam-ref-read--completions ()
  "Return an alist for ref completion.
The car is the ref, and the cdr is the corresponding node for the ref."
  (let ((rows (org-roam-db-query
               [:select [id ref type nodes:file pos title]
                :from refs
                :left-join nodes
                :on (= refs:node-id nodes:id)])))
    (cl-loop for row in rows
             collect (pcase-let* ((`(,id ,ref ,type ,file ,pos ,title) row)
                                  (node (org-roam-node-create :id id
                                                              :file file
                                                              :point pos
                                                              :title title)))
                       (cons
                        (propertize ref 'node node 'type type)
                        node)))))


(defun e/org-roam-ref-node-from-browser ()
  (interactive)
  (let* (( refs (e/org-roam-ref-read--completions))
         ( url-title (grab-x-link-firefox))
         ( url (car url-title))
         ( title (cdr url-title))
         ( title-template (when (org-roam-file-p)
                            (org-roam-node-title
                             (org-roam-node-at-point))))
         ( host (url-host (url-generic-parse-url url)))
         ( ref (concat "//"
                       host
                       (url-filename (url-generic-parse-url url))))
         ( file (e/org-roam-node-file-name title "refs"))
         ( node (org-roam-node-create :file file
                                      :title (concat "zref."
                                                     title-template))))
    (select-frame (make-frame-command))
    (if-let (( found (assoc ref refs)))
        (org-roam-node-visit (cdr found))
      (org-roam-capture- :keys "r" :node node)
      (org-roam-ref-add url)
      (org-roam-alias-add host)
      (org-roam-alias-add title)
      (insert (concat "* Aliases\n"
                      "  - " host "\n"
                      "  - " title "\n\n"
                      "* Links\n"
                      "  - http(s): " (org-link-make-string url host))))))


(defun e/org-link-from-browser ()
  (interactive)
  (let* (( url-title (grab-x-link-firefox))
         ( url (car url-title))
         ( domain (url-host (url-generic-parse-url url))))
    (insert (org-link-make-string url domain))))


(defun e/org-link-from-browser-title ()
  (interactive)
  (let* (( url-title (grab-x-link-firefox))
         ( url (car url-title))
         ( title (cdr url-title))
         ( domain (url-host (url-generic-parse-url url))))
    (insert (org-link-make-string url domain) " " title))
  (jit-lock-refontify)
  (org-fill-paragraph))
    

(defun e/org-roam-ref-bibtex-reformat ( entry &optional author)
  (unless author (setq author "%2a"))
  (replace-regexp-in-string
   "\s\s+" " "
   (replace-regexp-in-string
    "\\\\\\|{\\|}\\|it" ""
    (reftex-format-citation
     entry (if (string-blank-p (reftex-get-bib-field "year" entry))
               author
             (concat author " %y"))))))


(defun e/org-roam-ref-node-from-bibtex ()
  (interactive)
  (let* (( refs (e/org-roam-ref-read--completions))
         ( key (car (reftex-citation 'no-insert)))
         ( entry (reftex-parse-bibtex-entry
                  (reftex-pop-to-bibtex-entry key
                                              reftex-default-bibliography
                                              nil nil nil t)))
         ( doi (reftex-get-bib-field "doi" entry))
         ( url (replace-regexp-in-string "\\\\" ""
                                         (reftex-get-bib-field "url" entry)))
         ( title (reftex-get-bib-field "title" entry))
         ( year (reftex-get-bib-field "year" entry))
         ( author (reftex-get-bib-field "author" entry))
         ( host (url-host (url-generic-parse-url url)))
         ( file (e/org-roam-node-file-name title "refs"))
         ( node (org-roam-node-create :file file :title title))
         ( citation (e/org-roam-ref-bibtex-reformat entry))
         ( citation-full (e/org-roam-ref-bibtex-reformat entry "%a")))
    (select-frame (make-frame-command))
    (if-let (( found (assoc key refs)))
        (org-roam-node-visit (cdr found))
      (org-roam-capture- :keys "r" :node node)
      (org-roam-ref-add (concat "cite:" key))
      (org-roam-alias-add citation)
      (org-roam-alias-add title)
      (org-roam-alias-add key)
      (unless (string-blank-p doi)
        (org-roam-ref-add (concat "doi:" doi)))
      (unless (string-blank-p url)
        (org-roam-ref-add url))
      (insert (concat "* Aliases\n"
                      "  - citation: " citation "\n"
                      "  - citation-full: " citation-full "\n"
                      "  - title: " title "\n"
                      "  - key: " key "\n\n"

                      "* Links\n"
                      "  - cite: " (org-link-make-string (concat "cite:" key)
                                                         citation) 
                      (unless (string-blank-p doi)
                        (concat "\n"
                                "  - doi: " (org-link-make-string (concat "doi:" doi) doi)))
                      (unless (string-blank-p url)
                        (concat "\n"
                                "  - url: " (org-link-make-string url host))))))))


(defun e/org-link-from-bibtex ()
  (interactive)
  (let* (( key (car (reftex-citation 'no-insert)))
         ( entry (reftex-parse-bibtex-entry
                  (reftex-pop-to-bibtex-entry key
                                              reftex-default-bibliography
                                              nil nil nil t)))
         ( citation (e/org-roam-ref-bibtex-reformat entry))
         ( citation-full (e/org-roam-ref-bibtex-reformat entry "%a")))
    (insert (org-link-make-string (concat "cite:" key) citation))))


(defun e/org-roam-ref-choices ( choices)
  (switch-to-buffer "*Choices*")
  (let ( chars)
    (dolist ( choice choices)
      (setq chars (cons (car choice) chars))
      (unless (eq (car choice) ?-)
        (insert "[" (car choice) "] "))
      (insert (cdr choice) "\n"))
    (let (( char (read-char-from-minibuffer "Choice: " chars)))
      (kill-buffer "*Choices*")
      (cond ((eq char ?a)
             (read-file-name "File Name: " (file-name-as-directory
                                            (expand-file-name "~"))))
            ((eq char ?q)
             (user-error "Aborted."))
            (t
             (cdr (assoc char choices)))))))


(defun e/org-roam-ref-dired-filenames ()
  (let ( files)
    (dolist ( window (window-list))
      (let (( buffer (window-buffer window)))
        (with-current-buffer buffer
          (when (and (eq major-mode 'dired-mode)
                     (dired-get-filename nil 'no-error))
            (setq files (cons (dired-get-filename nil 'no-error) files))))))
    files))


(defun e/org-roam-ref-clipboard-filename ()
  (let (( clipboard (url-filename
                     (url-generic-parse-url
                      (gui-get-primary-selection)))))
  (when (and clipboard
             (file-exists-p clipboard))
    clipboard)))


(defun e/org-roam-wmctrl-filenames ()
  (let (( lines (split-string
                 (shell-command-to-string "wmctrl -lx") "\n"))
        app-files)
    (dolist ( line lines)
      (when (string-match "\\.\\([^\s]+\\)\s[^/]*\\(/.*\\.[a-zA-Z]*\\)" line)
        (setq app-files (cons (cons (match-string 1 line)
                                    (match-string 2 line))
                              app-files))))
    app-files))


(defun e/org-roam-ref-gather-files ()
  (let (( choices '((?- . "======================")
                    (?a . "Ask for filename")
                    (?q . "Abort")))
        ( i 0)
        file)
    (dolist ( file (e/org-roam-ref-dired-filenames))
      (setq choices (cons `(,(aref (number-to-string i) 0) . ,file)
                          choices)
            i (+ i 1)))
    (when (setq file (e/org-roam-ref-clipboard-filename))
      (setq choices (cons `(,(aref (number-to-string i) 0) . ,file)
                          choices)
            i (+ i 1)))
    choices))


(defun e/org-roam-ref-node-from-file ()
  (interactive)
  (let* (( refs (e/org-roam-ref-read--completions))
         ( external-path (expand-file-name
                          (e/org-roam-ref-choices
                           (e/org-roam-ref-gather-files))))
         ( external-file (file-name-nondirectory external-path))
         ( external-base (file-name-base external-file))
         ( external-extension (file-name-extension external-file))
         ( title (concat external-base
                         (when external-extension
                           (concat " (" external-extension "-File)"))))
         ( file (e/org-roam-node-file-name title "refs"))
         ( node (org-roam-node-create :file file :title title)))
    (if-let (( found (assoc external-path refs)))
        (org-roam-node-visit (cdr found))
      (org-roam-capture- :keys "r" :node node)
      (insert "\n* Links")
      (insert "\n\n** File Path:")
      (setq external-path (concat "file:" external-path))
      (org-roam-ref-add (url-encode-url external-path))
      (insert "\n- " (org-link-make-string external-path external-file)))))


;;**** link section mode


(setq e/org-roam-link-section-point-line nil)


(defun e/org-roam-link-section-open ()
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive)
  (let (( id (get-text-property (point) 'link-section-id)))
    (e/org-roam-open-link-id id)))


(defun e/org-roam-link-section-open-ow ()
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive)
  (let (( id (get-text-property (point) 'link-section-id)))
    (e/org-roam-open-link-id id 'other-window)))


(defun e/org-roam-link-section-fringe ()
  (save-excursion
    (let* (( map (make-sparse-keymap))
           ( fringe (propertize "x"
                                'display '(left-fringe org-transclusion-fringe-bitmap
                                                       org-transclusion-fringe))))
      (goto-char (point-min))
      (end-of-line)
      (insert fringe)
      (while (search-forward "\n" nil t)
        (end-of-line)
        (insert fringe)))))


(defun e/org-roam-link-section-set-keymap ()
  (let (( map (make-sparse-keymap)))
    (define-key map (kbd "o") 'e/org-roam-link-section-open)
    (define-key map (kbd "RET") 'e/org-roam-link-section-open)
    (define-key map (kbd "O") 'e/org-roam-link-section-open-ow)
    (define-key map (kbd "SPC") 'e/org-roam-link-section-open-ow)
    (define-key map (kbd "g") 'e/org-roam-link-section-refresh)
    (define-key map (kbd "d") 'e/org-roam-link-section-remove)
    (define-key map (kbd "t") 'e/org-roam-link-section-remove)
    map))


(defun e/org-roam-link-section-goto-node ( id)
    (goto-char (point-min))
    (when (search-forward id nil t)
      (org-back-to-heading-or-point-min)))


(defun e/org-roam-link-section-end-of-file-head ()
  (when (re-search-forward ":ID:" nil t)
    (re-search-forward ":END:\s*\n" nil t)
    (while (looking-at "#\\+")
      (beginning-of-line 2))
    (point)))


(defun e/org-roam-link-section-indent-fill ( indent)
  (indent-rigidly (point-min) (point-max)
                  (- indent
                     (indent-rigidly--current-indentation
                      (point-min) (point-max))))
  (goto-char (point-min))
  (font-lock-ensure)
  (org-fill-paragraph)
  (while (< (forward-line) 1)
    (org-fill-paragraph)))


(defun e/org-roam-link-section ()
  (interactive)
  (unless org-roam-link-section-mode
    (user-error "org-roam-link-section-mode not active."))
  (beginning-of-line)
  (unless (re-search-forward "\\[\\(>\\)\\]\s*\\[\\[id:" (line-end-position) t)
    (user-error "Not in a LINK SECTION line."))
  (unless (get-text-property (point) 'link-section-link)
    (let* (( modified (buffer-modified-p))
           ( current-indent (if (org-before-first-heading-p)
                                0
                              (+ (car (org-heading-components)) 1)))
           ( link (org-element-context))
           ( link-id (org-element-property :path link))
           ( link-beg (line-beginning-position))
           ( link-end (line-beginning-position 2))
           ( buffers (buffer-list))
           ( e/org-roam-link-section-inhibit t)
           ( source-buffer (marker-buffer
                            (org-roam-id-find link-id 'marker))) ;; visits file
           ( sentence-end-double-space nil)
           ( inhibit-message t)
           entry)
      (with-current-buffer source-buffer
        (save-excursion
          (if (= (point-min)
                 (e/org-roam-link-section-goto-node link-id))
              (e/org-roam-link-section-end-of-file-head)
            (org-end-of-meta-data))
          (setq entry (buffer-substring (point)
                                        (or (org-next-visible-heading 1)
                                            (point))))))
      (unless (member source-buffer buffers)
        (kill-buffer source-buffer))
      (with-temp-buffer
        (org-mode)
        (insert entry)
        (delete-region (point) (+ (re-search-backward "[^\s\n]" nil t) 1))
        (goto-char (point-min))
        (delete-blank-lines)
        (delete-blank-lines)
        (e/org-roam-link-section-indent-fill current-indent)
        (e/org-roam-link-section-fringe)
        (setq entry (propertize (concat (buffer-string) "\n")
                                'link-section-id link-id
                                'local-map (e/org-roam-link-section-set-keymap)
                                'read-only t
                                'front-sticky t
                                'rear-nonsticky t
                                )))
      (add-text-properties link-beg link-end
                           '( link-section-link t
                              display ""))
      (goto-char link-end)
      (insert entry)
      (goto-char (- (point) 1))
      (unless modified
        (set-buffer-modified-p nil)))))


(defun e/org-roam-link-section-all ( &optional after)
  (let (( window-line (cdr (nth 6 (posn-at-point))))
        ( inhibit-read-only t))
    (unless (or after
                (= (point) (buffer-end 1)))
      (put-text-property (point) (1+ (point)) 'link-section-point t))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\(>\\)\\]\s*\\[\\[id:" nil t)
      (when (or (not after) (get-text-property (point) 'link-section-before))
        (e/org-roam-link-section)))
    (if after
        (goto-char (car e/org-roam-link-section-point-line))
      (goto-char (point-min))
      (when (text-property-search-forward 'link-section-point)
        (goto-char (- (point) 1))))
    (when (eq (current-buffer) (window-buffer))
      (recenter (if after
                    (cdr e/org-roam-link-section-point-line)
                  window-line)))
    (remove-text-properties (point-min) (point-max)
                            '( link-section-point nil
                               link-section-before nil)))
  (restore-buffer-modified-p nil))


(defun e/org-roam-link-section-remove ()
  (interactive)
  (when (get-text-property (point) 'link-section-id)
    (let (( match-link (text-property-search-backward 'link-section-link))
          ( match-section (text-property-search-forward 'link-section-id))
          ( inhibit-read-only t))
      (delete-region (prop-match-beginning match-section)
                     (prop-match-end match-section))
      (remove-text-properties (prop-match-beginning match-link)
                              (prop-match-end match-link)
                              '( link-section-link nil
                                 display nil
                                 read-only nil)))
    (re-search-backward ">\\]\s*\\[\\[id:" nil t))
  (restore-buffer-modified-p nil))


(defun e/org-roam-link-section-remove-all ( &optional before)
  (when before
    (setq e/org-roam-link-section-point-line
        (cons (point) (cdr (nth 6 (posn-at-point))))))
  (when (get-text-property (point) 'link-section-id)
    (re-search-backward ">\\]\s*\\[\\[id:" nil t))
  (let (( window-line (cdr (nth 6 (posn-at-point))))
        ( inhibit-read-only t)
        match)
    (unless (= (point) (point-max))
      (put-text-property (point) (+ (point) 1)
                         'link-section-remove t))
    (goto-char (point-min))
    (while (text-property-search-forward 'link-section-id)
      (goto-char (1- (point)))
      (e/org-roam-link-section-remove)
      (when before
        (put-text-property (line-beginning-position)
                           (line-end-position) 'link-section-before t)))
    (goto-char (point-min))
    (if (text-property-search-forward 'link-section-remove)
        (goto-char (- (point) 1))
      (goto-char (point-max)))
    (when (eq (current-buffer) (window-buffer))
      (recenter window-line))
    (remove-text-properties (point-min) (point-max)
                            '( link-section-remove nil)))
  (restore-buffer-modified-p nil))


(defun e/org-roam-link-section-refresh ()
  (interactive)
  (let (( point (point))
        ( window-line (cdr (nth 6 (posn-at-point)))))
    (e/org-roam-link-section-remove)
    (e/org-roam-link-section)
    (goto-char point)
    (recenter window-line)))


(defun e/org-roam-link-section-before-save ()
  (e/org-roam-link-section-remove-all 'before))


(defun e/org-roam-link-section-after-save ()
  (e/org-roam-link-section-all 'after))


(setq e/org-roam-link-section-inhibit nil)


(define-minor-mode org-roam-link-section-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " LS" :keymap nil
  (unless (org-roam-buffer-p)
    (user-error "Not in a ORG ROAM buffer."))
  (cond ( org-roam-link-section-mode
          (unless e/org-roam-link-section-inhibit
            (add-hook 'before-save-hook #'e/org-roam-link-section-before-save nil t)
            (add-hook 'after-save-hook #'e/org-roam-link-section-after-save nil t)
            (e/org-roam-link-section-all)
            (let (( map (make-sparse-keymap)))
              (define-key map (kbd "<mouse-1>") 'e/org-roam-link-section)
              (define-key map (kbd "t") 'e/org-roam-link-section)
              (font-lock-add-keywords nil
                                      `(("\\[\\(>\\)\\]\s*\\[\\[id:" 1
                                         '(face org-link
                                           keymap ,map
                                           mouse-face highlight)))))))
        (t
         (remove-hook 'before-save-hook #'e/org-roam-link-section-before-save t)
         (remove-hook 'after-save-hook #'e/org-roam-link-section-after-save t)
         (e/org-roam-link-section-remove-all)
         (let (( map (make-sparse-keymap)))
           (define-key map (kbd "<mouse-1>") 'e/org-roam-link-section)
           (define-key map (kbd "t") 'e/org-roam-link-section)
           (font-lock-remove-keywords nil
                                      `(("\\[\\(>\\)\\]\s*\\[\\[id:" 1
                                         '(face org-link
                                           keymap ,map
                                           mouse-face highlight))))))))


(defun e/org-roam-link-section-buffers-update ()
  (interactive)
  (let (( current-id (org-roam-id-at-point)))
    (dolist ( buffer (buffer-list))
      (when (and (not (eq buffer (current-buffer)))
                 (org-roam-buffer-p buffer))
        (with-current-buffer buffer
          (when org-roam-link-section-mode
            (goto-char (point-min))
            (while (setq match-section (text-property-search-forward 'link-section-id))
              (when (string= (prop-match-value match-section) current-id)
                (goto-char (1- (point)))
                (e/org-roam-link-section-remove)
                (e/org-roam-link-section)
                (goto-char (1+ (point)))))))))))


(defun e/org-roam-link-section-copy ()
  (interactive)
  (let (( active (get-text-property (point) 'link-section-id)))
    (when active
      (e/org-roam-link-section-remove))
    (beginning-of-line)
    (re-search-forward (concat "\\[\\(>\\)\\]\s*" org-link-bracket-re)
                       (line-end-position) t)
    (copy-region-as-kill (match-beginning 0) (match-end 0))
    (when active
      (e/org-roam-link-section))))


(defun e/org-roam-link-section-cut ()
  (interactive)
  (e/org-roam-link-section-remove)
  (beginning-of-line)
  (re-search-forward (concat "\\[\\(>\\)\\]\s*" org-link-bracket-re)
                     (line-end-position) t)
  (kill-region (match-beginning 0) (match-end 0)))


(defun e/org-roam-link-section-yank ()
  (interactive)
  (yank)
  (e/org-roam-link-section))
  

;;**** note zettelkasten


(defun e/org-roam-note-alias-short ( alias)
  (let* (( case-fold-search nil)
         ( caps-p (string-match "[A-Z]" alias))
         ( words (split-string alias)))
    (cond ((= (length words) 1)
           (downcase (substring (car words) 0 2)))
          (t
           (let (( short ""))
             (dolist ( word words)
               (let (( first (substring word 0 1)))
                 (if caps-p     
                     (when (string-match "[A-Z]" first)
                       (setq short (concat short first)))
                   (setq short (concat short first)))))
             (downcase short))))))
                 

(defun e/org-roam-note-new ( &optional other-window)
  (interactive)
  (e/org-roam-note-list-last-title)
  (let* (( dir (file-name-directory (get-text-property (point) 'file)))
         ( alias (read-string (concat "(in " (abbreviate-file-name dir)
                                      ") Node Alias: ")))
         ( title (read-string "Node Title: "
                              (concat (get-text-property (point) 'title)
                                      "."
                                      (e/org-roam-note-alias-short alias))))
         ( nondir (e/org-roam-node-file-nondir title))
         ( node (org-roam-node-create :file (concat dir nondir)
                                      :title title))
         new-buffer)
    (org-roam-capture- :keys "z" :node node)
    (setq new-buffer (current-buffer))
    (goto-char (point-min))
    (org-roam-property-add "ROAM_ALIASES" alias)
    (switch-to-buffer "*ORoam*")
    (switch-to-buffer-other-frame new-buffer))
  (re-search-backward "#\\+title:" nil t)
  (end-of-line))


(defun e/org-roam-tree-note-new ()
  (interactive)
  (let* (( title (cadar (org-collect-keywords '("TITLE"))))
         ( file (e/org-roam-node-file-name))
         ( node (org-roam-node-create 
                 :title "Note")))
    (org-roam-capture- :keys "h"
                       :node node
                       :props (list :link-description "Note"
                                    :finalize 'insert-link))))


(defun e/org-roam-note-new-ow ()
  (interactive)
  (e/org-roam-note-new 'other-window))


(defun e/org-roam-note-new-from-current ( &optional link)
  (interactive)
  (let ( beg end substring)
    (when (region-active-p)
      (setq beg (region-beginning)
            end (region-end)
            substring (buffer-substring-no-properties beg end))
      (delete-region beg end))
    (let* (( id (e/org-roam-link-copy-id))
           ( id-new (org-id-uuid))
           ( current-node (org-roam-node-from-id id))
           ( file (org-roam-node-file current-node))
           ( dir (file-name-directory file))
           ( alias (read-string (concat "(in " (abbreviate-file-name dir)
                                        ") Node Alias: ")
                                (and link substring)))
           ( title (read-string (concat "(\"" alias "\") Node Title: ")
                                (concat (org-roam-node-title current-node)
                                        "."
                                        (e/org-roam-note-alias-short alias))))
           ( nondir (e/org-roam-node-file-nondir title))
           ( node (org-roam-node-create :id id-new
                                        :file (concat dir nondir)
                                        :title title)))
      (when link
        (insert (org-link-make-string
                 (concat "id:" id-new)
                 (or substring
                     (concat "["
                             (when (string-match "[^.]+$" title)
                               (match-string 0 title))
                             "]")))))
      (select-frame (make-frame-command))
      (org-roam-capture- :keys "z" :node node)
      (goto-char (point-min))
      (org-roam-property-add "ROAM_ALIASES" alias)
      (goto-char (point-max))
      (when (and substring
                 (not link))
        (insert "\n" substring)))))


(defun e/org-roam-note-new-from-current-link ()
  (interactive)
  (e/org-roam-note-new-from-current 'link))


(defun e/org-roam-note-sort-format ( a)
  (replace-regexp-in-string "[0-9]+"
                            (lambda ( m)
                              (format "%010d" (string-to-number m)))
                            a))


(defun e/org-roam-note-sort-next ( a b)
  (let (( a-title (e/org-roam-note-sort-format (car a)))
        ( a-file (file-name-base (cadr a)))
        ( b-title (e/org-roam-note-sort-format (car b)))
        ( b-file (file-name-base (cadr b))))
    (cond ((string= a-title b-title)
           (string< a-file b-file))
          (t
           (string< a-title b-title)))))


(defun e/org-roam-note-sort-prev ( a b)
  (cond ((string= (car a) (car b))
         (string> (file-name-base (cadr a)) (file-name-base (cadr b))))
        (t
         (string> (car a) (car b)))))


(defun e/org-roam-note-next ( &optional prev other-window first-last)
  (interactive)
  (when (buffer-modified-p)
    (user-error "Node modified."))
  (let* (( titles (sort (org-roam-db-query [:select [title file id]
                                            :from nodes
                                            :where (not (like file $s1))]
                                           (concat "%" org-roam-dailies-directory "%"))
                        (if prev
                            'e/org-roam-note-sort-prev
                          'e/org-roam-note-sort-next)))
         ( title-file-id (car titles)))
    (when (and (org-roam-file-p)
               (not first-last))
      (let* (( node (org-roam-node-at-point))
             ( title (org-roam-node-title node))
             ( file (org-roam-node-file node))
             ( id (org-roam-node-id node)))
        (setq title-file-id (cadr (member (list title file id)
                                          titles)))))
    (message "%s" title-file-id)
    (e/org-roam-open-link-id (caddr title-file-id) other-window)))


(defun e/org-roam-note-prev ( &optional other-window)
  (interactive)
  (e/org-roam-note-next 'prev other-window))


(defun e/org-roam-note-next-ow ()
  (interactive)
  (e/org-roam-note-next nil 'other-window))


(defun e/org-roam-note-prev-ow ()
  (interactive)
  (e/org-roam-note-prev 'other-window))


(defun e/org-roam-string-regexp-count ( string regexp &optional subexp)
  (setq string (string-trim string nil "[ \t\n\r.]+"))
  (let (( length (length string)))
    (setq string (replace-regexp-in-string regexp "" string nil nil subexp))
    (- length (length string))))


(defun e/org-roam-note-join ( list &optional separator trim-left)
  (unless separator
    (setq separator " | "))
  (let ( joined)
    (dolist ( item list)
      (setq joined (concat joined (when joined separator)
                           (string-trim item trim-left))))
    (or joined "")))


(defun e/org-roam-note-make-string ( num string)
  (let (( new-string ""))
    (dotimes ( i num)
      (setq new-string (concat string new-string)))
    new-string))


(setq e/org-roam-note-list-separator "*"
      e/org-roam-note-fold-char "▸"
      e/org-roam-note-fold-unfold-char "▼"
      ;; e/org-roam-note-fold-unfolded-titles nil
      e/org-roam-note-list-last-title nil
      e/org-roam-note-fold-title-regexp
      (concat "^[^" (regexp-quote e/org-roam-note-list-separator)
              "]+[^" (regexp-quote e/org-roam-note-list-separator) "\s]"))


(defun e/org-roam-note-match ( space-sep-regexps string)
  (let (( regexp-list (and space-sep-regexps
                           (split-string space-sep-regexps)))
        ( found t)
        regexp)
    (while (and found
                (setq regexp (pop regexp-list)))
      (setq found (and (string-match-p regexp string) t)))
    found))


(defun e/org-roam-note-list-title-propertize ( title)
  (let (( i 0)
        ( faces '( org-link
                   compilation-line-number
                   dired-header
                   dired-mark))
        proptitle)
    (dolist ( subtitle (split-string title "\\." 'omit-nulls "\s"))
      (setq subtitle (propertize subtitle 'font-lock-face (nth i faces))
            proptitle (concat proptitle (and proptitle ".") subtitle)
            i (1+ i)))
    proptitle))


(defun e/org-roam-note-list ( &optional fold-all goto-id)
  (interactive)
  (unless goto-id
    (when (org-roam-buffer-p)
      (save-excursion
        (goto-char (point-min))
        (setq goto-id (org-roam-id-at-point)))))
  (unless (eq major-mode 'oroam-mode)
    (switch-to-buffer "*ORoam*"))
  (let (( window-line (cdr (nth 6 (posn-at-point))))
        ( point (point))
        ( inhibit-read-only t)
        ( results (sort
                   (org-roam-db-query "SELECT
title, file, id,
'(' || group_concat(alias, ' ') || ')',
refs,
properties
FROM (SELECT
  title, file, id, 
  alias,
  '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs,
  properties
  FROM nodes
  LEFT JOIN aliases ON aliases.node_id = nodes.id
  LEFT JOIN refs ON refs.node_id = nodes.id
  GROUP BY nodes.id, aliases.alias ORDER BY alias ASC)
GROUP BY id ORDER BY id")
                   'e/org-roam-note-sort-next))
        ;; ( titles (e/org-roam-note-fold-unfolded-titles))
        ( ids (e/org-roam-note-fold-unfolded-ids))
        title title-prev file id aliases refs level color colors parent-title highlight)
    (erase-buffer)
    (remove-overlays)
    (dolist (result results)
      (setq title (e/org-roam-note-list-title-propertize (nth 0 result))
            file (nth 1 result)
            id (nth 2 result)
            aliases (e/org-roam-note-join (nth 3 result) " | " "[0-9]+:")
            aliases-help (e/org-roam-note-join (nth 3 result) "\n" "[0-9]+:")
            refs (nth 4 result)
            color (cdr (assoc "BACKGROUND_COLOR"
                                   (nth 5 result)))
            level (e/org-roam-string-regexp-count title "\\."))
      (if color
          (setq colors (cons (cons title color) colors))
        (setq parent-title title)
        (while (and (not (string-empty-p parent-title))
                    (not (setq color (cdr (assoc parent-title colors)))))
          (setq parent-title (string-trim parent-title nil "\\.?[^.]*$")))
        (when (string-empty-p parent-title)
          (setq colors nil)))
      (setq highlight (if color
                          `(:background ,color :extend t)
                        'highlight))
      (insert
       (concat (propertize (format "%-45s" title)
                           'title title
                           'id id
                           'refs (and refs t)
                           'file file
                           ;; 'font-lock-face 'org-link
                           'rear-nonsticky (list 'font-lock-face)
                           'mouse-face highlight
                           'help-echo (concat aliases-help "\n" id "\n" file))
               (propertize e/org-roam-note-list-separator
                           'font-lock-face (cond (refs 'custom-face-tag)
                                                 ((string-match "daily" file)
                                                  'warning)
                                                 (t 'compilation-info))
                           'mouse-face highlight
                           'separator t
                           'id id)
               (propertize " " 'mouse-face highlight)
               (propertize (e/org-roam-note-make-string level "│ ")
                           'font-lock-face 'window-divider-first-pixel
                           'mouse-face highlight)
               (propertize aliases
                           'font-lock-face `(:background ,(or color "white") :extend t)
                           'mouse-face highlight
                           'id id
                           'help-echo aliases-help)
               "\n")))
    (oroam-mode)
    (e/org-roam-note-fold-all)
    (if fold-all
        (goto-char (point-min))
      ;; (e/org-roam-note-fold-unfolded-titles-restore titles)
      (e/org-roam-note-fold-unfolded-ids-restore ids)
      (goto-char point)
      (recenter window-line)
      (when goto-id
        (e/org-roam-note-list-goto-id goto-id)))
    (set-buffer-modified-p nil)
    (read-only-mode 1)))


(defun e/org-roam-note-list-open-at-mouse ( event &optional other-window)
  (interactive "e")
  (mouse-set-point event)
  (e/org-roam-note-list-last-title)
  (let (( org-link-frame-setup `(( file . ,(if other-window
                                               'find-file-other-window
                                             'find-file))))
        ( id (mouse-posn-property (event-start event) 'id))
        ( point (mouse-posn-property (event-start event) 'point)))
    (with-temp-buffer
      (insert (org-link-make-string (concat "id:" id)))
      (goto-char (point-min))
      (org-open-at-point))))


(defun e/org-roam-note-list-open-at-mouse-ow ( event)
  (interactive "e")
  (e/org-roam-note-list-open-at-mouse event 'other-window))


(defun e/org-roam-note-alias-add ()
  (interactive)
  (org-roam-alias-add
   (read-string "Alias: "
                (when (region-active-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))))))


(define-derived-mode oroam-mode outline-mode "ORoam"
    "Major mode for listing emacs buffers."
    (unless (eq (current-buffer) (get-buffer "*ORoam*"))
      (error "ORoam: current buffer is no ORoam buffer."))
    (setq truncate-lines t)
    (add-to-invisibility-spec '(oroam . nil))
    (define-key oroam-mode-map (kbd "g") 'e/org-roam-note-list)
    (define-key oroam-mode-map (kbd "n") 'e/org-roam-note-new)
    (define-key oroam-mode-map (kbd "s") 'e/org-roam-note-list-isearch)
    (define-key oroam-mode-map (kbd "c") 'e/org-roam-note-set-color)
    (define-key oroam-mode-map (kbd "i") 'org-roam-note-list-insert-mode)
    (define-key oroam-mode-map (kbd "C-c c") 'e/org-roam-link-copy-id)
    (define-key oroam-mode-map (kbd "C-c s") 'e/org-roam-link-scan)
    (define-key oroam-mode-map
      (kbd "<C-mouse-1>") 'e/org-roam-link-ref-open-at-mouse)
    (define-key oroam-mode-map
      (kbd "<mouse-2>") 'e/org-roam-note-list-open-at-mouse)
    (define-key oroam-mode-map
      (kbd "<mouse-3>") 'e/org-roam-note-list-open-at-mouse-ow)
    (define-key oroam-mode-map
      (kbd "<C-M-mouse-2>") 'e/org-roam-note-fold-unfold-all)
    (define-key oroam-mode-map
      (kbd "<C-M-mouse-3>") 'e/org-roam-note-fold-all)
    (define-key oroam-mode-map
      (kbd "<S-mouse-2>") 'e/org-roam-note-fold-mouse-toggle))


(defun e/org-roam-note-fold-region ()
  "Find start and end positions for folding the current node.

Returns start and end position as a cons cell (start . end) or
nil. This function therefore returns the start and end position
of the invisible part of the buffer. Start position refers to the
beginning of the next line and end position is the beginning of
the next line that is not part of the fold region. This function
returns nil, when there is no parent node at current line."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at e/org-roam-note-fold-title-regexp)
      (beginning-of-line 2)
      (let (( prefix (concat (match-string-no-properties 0)
                             "."))
            ( begin (point)))
        (while (looking-at (regexp-quote prefix))
          (beginning-of-line 2))
        (when (< begin (point))
          (cons begin (point)))))))


(defun e/org-roam-note-fold-char-width ( char)
  (insert char)
  (backward-char)
  (let (( width (aref (aref (font-get-glyphs (font-at (point))
                                             (point)
                                             (1+ (point)))
                            0)
                      4)))
    (delete-char 1)
    width))


(defun e/org-roam-note-fold-symbol ( symbol)
  (remove-overlays (line-beginning-position) (line-end-position))
  (beginning-of-line)
  (re-search-forward e/org-roam-note-fold-title-regexp (line-end-position) t)
  (let* (( inhibit-read-only t)
         ( symbol-width (e/org-roam-note-fold-char-width symbol))
         ( default-width (e/org-roam-note-fold-char-width "m"))
         ( width (- (* 2 default-width) symbol-width))
         ( space (propertize " " 'display `(space . (:width (,width)))))
         ( overlay (make-overlay (point) (+ (point) 2))))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'after-string (concat symbol space))))

     
(defun e/org-roam-note-fold ()
  (save-excursion
    (unless (e/org-roam-note-fold-p)
      (when-let (( begin-end (e/org-roam-note-fold-region)))
        (let (( inhibit-read-only t)
              ( overlay (make-overlay (car begin-end) (cdr begin-end))))
          (overlay-put overlay 'invisible 'oroam)
          (overlay-put overlay 'isearch-open-invisible 'delete-overlay))
        (e/org-roam-note-fold-symbol e/org-roam-note-fold-char)))))


(defun e/org-roam-note-fold-block ( &optional start end)
  (unless end
    (unless (setq end (cdr (e/org-roam-note-fold-region)))
      (user-error "No folding limit specified.")))
  (unless start
    (setq start (line-beginning-position)))
  (save-excursion
    (goto-char end)
    (while (> (point) start)
      (beginning-of-line 0)
      (e/org-roam-note-fold))))


(defun e/org-roam-note-fold-all ()
  (interactive)
  (e/org-roam-note-fold-block (point-min) (point-max)))


(defun e/org-roam-note-fold-unfold ()
  (save-excursion
    (end-of-line)
    (let (( inhibit-read-only t)
          ( pos (next-overlay-change (point))))
      (delete-overlay (car (overlays-at pos))))
    (e/org-roam-note-fold-symbol e/org-roam-note-fold-unfold-char)))


(defun e/org-roam-note-fold-unfold-block ( &optional start end)
  (unless end
    (unless (setq end (cdr (e/org-roam-note-fold-region)))
      (user-error "No folding limit specified.")))
  (unless start
    (setq start (line-beginning-position)))
  (remove-overlays start end)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (e/org-roam-note-fold-region)
        (e/org-roam-note-fold-symbol e/org-roam-note-fold-unfold-char))
      (beginning-of-line 2))))


(defun e/org-roam-note-fold-unfold-all ()
  (interactive)
  (e/org-roam-note-fold-unfold-block (point-min) (point-max)))


(defun e/org-roam-note-fold-p ()
  (save-excursion
    (beginning-of-line 2)
    (when-let (( overlays (overlays-at (point))))
      (let ( found)
        (while overlays
          (when (= (overlay-start (pop overlays)) (point))
            (setq found t
                  overlays nil)))
        found))))


(defun e/org-roam-note-fold-region-p ()
  (save-excursion
    (when-let (( end (cdr (e/org-roam-note-fold-region))))
      (let ( found)
        (while (and (< (point) end)
                    (not (setq found (e/org-roam-note-fold-p))))
          (beginning-of-line 2))
        found))))
        

(defun e/org-roam-note-list-last-title ( &optional set)
  (save-excursion
    (beginning-of-line)
    (when (looking-at e/org-roam-note-fold-title-regexp)
      (setq e/org-roam-note-list-last-title
            (when set (match-string-no-properties 0)))
      (match-string-no-properties 0))))


(defun e/org-roam-note-fold-toggle ()
  (interactive)
  (cond ((and (not (e/org-roam-note-fold-p))
              (string= e/org-roam-note-list-last-title
                       (e/org-roam-note-list-last-title))
              (e/org-roam-note-fold-region-p))
         (e/org-roam-note-fold-unfold-block))
        ((e/org-roam-note-fold-p)
         (e/org-roam-note-fold-unfold))
        (t
         (e/org-roam-note-fold-block)))
  (e/org-roam-note-list-last-title 'set))


(defun e/org-roam-note-fold-mouse-toggle ( event)
  "Toggle visibility of current fold by mouse.

EVENT refers to the mouse button press."
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (e/org-roam-note-fold-toggle))))


(defun e/org-roam-note-fold-unfolded-titles ()
  ;; (setq e/org-roam-note-fold-unfolded-titles nil)
  (goto-char (point-max))
  (let ( titles)
    (while (> (point) (point-min))
      (beginning-of-line 0)
      (when (and (looking-at e/org-roam-note-fold-title-regexp)
                 (e/org-roam-note-fold-region)
                 (not (e/org-roam-note-fold-p)))
        (setq titles (cons (match-string-no-properties 0)
                           titles))))
    titles))


(defun e/org-roam-note-fold-unfolded-ids ()
  (goto-char (point-max))
  (let ( ids)
    (while (> (point) (point-min))
      (beginning-of-line 0)
      (when (and (e/org-roam-note-fold-region)
                 (not (e/org-roam-note-fold-p)))
        (setq ids (cons (get-text-property (point) 'id) ids))))
    ids))


(defun e/org-roam-note-fold-unfolded-titles-restore ( titles)
  (goto-char (point-min))
  (dolist (title titles)
    (when (re-search-forward (concat "^"
                                     (regexp-quote title)
                                     "\s*"
                                     e/org-roam-note-list-separator)
                             nil t)
      (e/org-roam-note-fold-unfold))))


(defun e/org-roam-note-fold-unfolded-ids-restore ( ids)
  (goto-char (point-min))
  (dolist ( id ids)
    (when (text-property-search-forward 'id id)
      (e/org-roam-note-fold-unfold)
      (beginning-of-line 2))))


(defun e/org-roam-note-list-rename ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward e/org-roam-note-fold-title-regexp
                             (line-end-position) t)
      (let (( new-title (match-string-no-properties 0)))
        (text-property-search-forward 'separator)
        (let* (( id (get-text-property (1- (point)) 'id))
               ( title (org-roam-node-title
                        (org-roam-node-from-id id))))
          (unless (string= new-title title)
            (let* (( buffers (buffer-list))
                   ( m (org-roam-id-find id 'marker))
                   ( buffer (marker-buffer m)))
              (with-current-buffer buffer
                (goto-char (point-min))
                (re-search-forward "#\\+title:\\(.*\\)$" nil t)
                (replace-match (concat " " new-title) t nil nil 1)
                (save-buffer))
              (unless (member buffer buffers)
                (kill-buffer buffer)))))))))


(defun e/org-roam-note-list-rename-visible ()
  (goto-char (point-max))
  (let (( i 0))
    (while (> (point) (point-min))
      (beginning-of-line 0)
      (when (not (get-char-property (point) 'invisible))
        (e/org-roam-note-list-rename)
        (setq i (1+ i))))
    i))
      

(define-minor-mode org-roam-note-list-insert-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " insert"
  :keymap `(,(cons (kbd "RET") 'org-roam-note-list-insert-mode))
  (unless (eq major-mode 'oroam-mode)
    (user-error "Not in a ORoam buffer."))
  (cond (org-roam-note-list-insert-mode
         (read-only-mode 0)
         (setf (cdr oroam-mode-map) nil))
        (t
         (when (y-or-n-p "Apply changes to all modified node titles? ")
           (e/org-roam-note-list-rename-visible))
         (e/org-roam-note-list))))


(defun e/org-roam-note-header-line ()
  (save-excursion
    (point-min)
    (when-let (( node (org-roam-node-at-point)))
      (let* (( face-bg-alist '(( base . "green")
                               ( ref . "cyan")
                               ( daily . "orange")))
             ( title (concat (org-roam-node-title node) ": "))
             ( aliases (e/org-roam-note-join (org-roam-node-aliases node)
                                             " | " "[0-9]+:"))
             ( face-bg (cond ((org-roam-dailies--daily-note-p)
                              (run-hooks 'org-roam-dailies-find-file-hook)
                              nil) ;; 'company-template-field
                             ((e/org-roam-ref-buffer-p)
                              (cdr (assoc 'ref face-bg-alist)))
                             (t
                              (cdr (assoc 'base face-bg-alist))))))
        (when face-bg
          (setq header-line-format
                (list (format "%-70s"
                              (concat
                               (propertize title
                                           'face `(:background ,face-bg
                                                   :slant italic
                                                              ))
                               (propertize aliases
                                           'face `(:background ,face-bg
                                                   :weight bold
                                                              )))))))))))
          

(defun e/org-roam-note-list-isearch ()
  (interactive)
  (let ( string)
    (if (region-active-p)
        (setq string (buffer-substring-no-properties
                      (region-beginning) (region-end)))
      (setq string (read-string "I-Search ORoam Buffer: ")))
    (e/org-roam-note-list 'fold-all)
    (isearch-forward nil 1)
    (isearch-yank-string string)))


(defun e/org-roam-note-list-goto-id ( id)
  (let (( point (point)))
    (goto-char (point-min))
    (if (text-property-search-forward 'id id)
        (dolist ( ov (overlays-at (point)))
          (delete-overlay ov))
      (goto-char point))))


(defun e/org-roam-note-set-color ()
  (interactive)
  (let (( buffer (current-buffer))
        ( buffers (buffer-list))
        ( color (completing-read
                 "Color: "
                 `(,(propertize "#ffff00"
                                'face '(:background "yellow"))
                   ,(propertize "#00ffff"
                                'face '(:background "cyan"))
                   ,(propertize "#f4a460"
                                'face '(:background "sandy brown"))
                   ,(propertize "#f0e68c"
                                'face '(:background "khaki"))
                   ,(propertize "#7fffd4"
                                'face '(:background "aquamarine"))
                   ,(propertize "#5cacee"
                                'face '(:background "SteelBlue2"))
                   ,(propertize "#dda0dd"
                                'face '(:background "plum"))
                   ,(propertize "#b4eeb4"
                                'face '(:background "DarkSeaGreen2"))
                   ,(propertize "#ffc0cb"
                                'face '(:background "pink"))
                   ,(propertize "#eedd82"
                                'face '(:background "light goldenrod"))
                   ,(propertize "#add8e6"
                                'face '(:background "light blue"))
                   ,(propertize "#f08080"
                                'face '(:background "light coral"))))))
    (when (eq major-mode 'oroam-mode)
      (setq buffer (marker-buffer (org-roam-id-find (get-text-property (point)
                                                                       'id)
                                                    'marker))))
    (with-current-buffer buffer 
      (save-excursion
        (goto-char (point-min))
        (org-set-property "BACKGROUND_COLOR" color))
      (save-buffer)
      (unless (member buffer buffers)
        (kill-buffer)))))


(defun e/org-roam-node-find ()
  (interactive)
  (let* (( results (mapcar (lambda ( item)
                             (list (format "%-10s %-10s %-20s"
                                     (car item)
                                     (string-join (nth 3 item) " ")
                                     (propertize
                                      (file-relative-name (cadr item) org-roam-directory))
                                     'face 'font-lock-comment-face)
                                   item))
                           (sort
                            (org-roam-db-query "SELECT
title, file, id,
'(' || group_concat(alias, ' ') || ')',
refs,
properties
FROM (SELECT
  title, file, id, 
  alias,
  '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs,
  properties
  FROM nodes
  LEFT JOIN aliases ON aliases.node_id = nodes.id
  LEFT JOIN refs ON refs.node_id = nodes.id
  GROUP BY nodes.id, aliases.alias ORDER BY alias ASC)
GROUP BY id ORDER BY id")
                   'e/org-roam-note-sort-next)))
         ( key (ivy-completing-read "Node: " results)))
    (let (( node (cadr (assoc key results))))
      (if node
          (find-file (cadr node))
        (org-roam-capture-
         :node (org-roam-node-create :title key)
         :props '(:finalize find-file))))))


(defun e/org-roam-node-list ()
  (let* (( results (sort
                    (org-roam-db-query "SELECT
title, file, id,
'(' || group_concat(alias, ' ') || ')',
refs,
properties
FROM (SELECT
  title, file, id, 
  alias,
  '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs,
  properties
  FROM nodes
  LEFT JOIN aliases ON aliases.node_id = nodes.id
  LEFT JOIN refs ON refs.node_id = nodes.id
  GROUP BY nodes.id, aliases.alias ORDER BY alias ASC)
GROUP BY id ORDER BY id")
                    'e/org-roam-note-sort-next)))
    (switch-to-buffer "*Node List*")
    (erase-buffer)
    (dolist ( result results)
      (let (( title (nth 0 result))
            ( aliases (string-join (nth 3 result) " ")))
        (insert (format "%-10s %-10s\n"
                        (propertize title 'face 'bold 'id (nth 2 result))
                        aliases))))))


(defun e/org-roam-isearch-update-post ()
  (when isearch-success
    (save-excursion
      (message "%s" (line-beginning-position)))))


;; (add-hook 'isearch-update-post-hook 'e/org-roam-isearch-update-post)


;;**** org-roam-dailies


(defun e/org-roam-dailies-time-prompt ()
  (let* (( h (number-to-string (nth 2 (decode-time))))
         ( m (concat (when (= (nth 1 (decode-time)) 0) "0")
                     (number-to-string (nth 1 (decode-time)))))
         ( hm (read-string (concat "Set Time (default " h ":" m "): ") nil nil
                           (concat h ":" m))))
    (unless (string-match ":" hm)
      (setq hm (cond ((or (= (length hm) 1) (= (length hm) 2))
                      (concat hm ":00"))
                     ((or (= (length hm) 3) (= (length hm) 4))
                      (concat (substring hm nil -2) ":" (substring hm -2 nil)))
                     (t hm))))
    (concat "* " hm "\n%?")))


(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>%?"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A, %d. %B %Y>\n")
         :unnarrowed t :jump-to-captured nil :kill-buffer t)))


(defun e/org-roam-dailies-goto-today ()
  (interactive)
  (org-roam-dailies-goto-today "d"))


(defun e/org-roam-dailies--daily-note-p ()
  (when (and (org-roam-dailies--daily-note-p)
             (buffer-file-name)
             (string= (file-name-directory (buffer-file-name))
                      (expand-file-name org-roam-dailies-directory
                                        org-roam-directory)))
    (let (( decoded-time (parse-time-string
                          (file-name-base
                           (buffer-file-name)))))
      (and (nth 3 decoded-time)
           (nth 4 decoded-time)
           (nth 5 decoded-time)))))


(defun e/org-roam-dailies-buffer-time ()
  (unless (e/org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (let* (( decoded-date (parse-time-string (file-name-base (buffer-file-name))))
         (inhibit-message t))
    (encode-time (decoded-time-set-defaults decoded-date))))


(defun e/org-roam-dailies-buffer-delta-time ( type delta)
  (unless (e/org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (let (( decoded-date (parse-time-string (file-name-base (buffer-file-name))))
        ( decoded-delta (make-decoded-time type delta))
        (inhibit-message t))
    (encode-time (decoded-time-set-defaults
                  (decoded-time-add decoded-date
                                    decoded-delta)))))


(defun e/org-roam-dailies-scroll ( type delta &optional other-window override-time)
  (unless (e/org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (let (( initial-buffer (current-buffer))
        ( initial-window (selected-window))
        ( time (or override-time (e/org-roam-dailies-buffer-delta-time type delta)))
        ( visited-roam-files (e/org-roam-visited-files))
        ( org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
        ( org-roam-dailies-directory "./")
        kill-candidate)
    (when other-window
      (other-window 1))
    (when (e/org-roam-dailies--daily-note-p)
      (setq kill-candidate (current-buffer)
            time (or override-time (e/org-roam-dailies-buffer-delta-time type delta)))
      (when (or (buffer-modified-p)
                (> (length (get-buffer-window-list kill-candidate)) 1)
                (e/org-roam-base-buffer-p kill-candidate))
        (setq kill-candidate nil)))
    (org-roam-capture- :goto '(4)
                       :keys "w"
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time time))
    (run-hooks 'org-roam-dailies-find-file-hook)
    (unless (member (buffer-file-name) visited-roam-files)
      (set-buffer-modified-p nil))
    (select-window initial-window)
    (when (and kill-candidate
               (= (length (get-buffer-window-list kill-candidate)) 0))
      (kill-buffer kill-candidate))))


(defun e/org-roam-dailies-header-mouse ( &optional event other-window)
  (interactive "e")
  (let* (( event-start (event-start event))
         ( cmd (mouse-posn-property event-start 'command))
         ( unit (mouse-posn-property event-start 'unit))
         ( incr (mouse-posn-property event-start 'increment)))
    (cond ((eq unit 'today)
           (e/org-roam-dailies-scroll nil nil other-window (current-time)))
          ((eq unit 'day)
           (e/org-roam-dailies-scroll :day incr other-window))
          ((eq unit 'month)
           (e/org-roam-dailies-scroll :month incr other-window))
          ((eq unit 'year)
           (e/org-roam-dailies-scroll :year incr other-window))
          (t
           (when cmd
             (funcall cmd))))))


(defun e/org-roam-dailies-header-mouse-ow ( &optional event)
  (interactive "e")
  (e/org-roam-dailies-header-mouse event 'other-window))


(defun e/org-roam-dailies-header-line ()
  (interactive)
  (let (( map (make-sparse-keymap))
        ( face 'success))
    (define-key map [header-line mouse-1] 'e/org-roam-dailies-header-mouse)
    (define-key map [header-line mouse-3] 'e/org-roam-dailies-header-mouse-ow)
    (setq header-line-format (list
                            "   "
                            (propertize "-Year"
                                'unit 'year
                                'increment -1
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "-Month"
                                'unit 'month
                                'increment -1     
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "-Week"
                                'unit 'day
                                'increment -7
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "-Day"
                                'unit 'day
                                'increment -1
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "<Today>"
                                'unit 'today
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "+Day"
                                'unit 'day
                                'increment 1
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "+Week"
                                'unit 'day
                                'increment 7
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "+Month"
                                'unit 'month
                                'increment 1
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "+Year"
                                'unit 'year
                                'increment 1
                                'face face
                                'mouse-face 'highlight
                                'keymap map)
                            "      "
                            (propertize "Capture"
                                'command 'e/org-roam-dailies-buffer-capture
                                'face 'italic
                                'mouse-face 'highlight
                                'keymap map)
                            "   "
                            (propertize "Kill"
                                'command 'kill-buffer
                                'face 'italic
                                'mouse-face 'highlight
                                'keymap map)))))


(defun e/org-roam-dailies-buffer-capture ()
  "Create an entry in the daily-note for the current buffer.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed."
  (interactive)
  ;; (org-roam-dailies--capture (e/org-roam-dailies-buffer-time) nil keys)
  (let (( org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
        ( org-roam-dailies-directory "./"))
    (org-roam-capture- :goto nil
                       :keys nil
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time (e/org-roam-dailies-buffer-time)
                                    :immediate-finish t
                                    :jump-to-captured t
                                    :kill-buffer nil
                                    :no-save t))
    (run-hooks 'org-roam-dailies-find-file-hook)))


(add-hook 'org-roam-dailies-find-file-hook 'e/org-roam-dailies-header-line)


;;**** hide file header


(defun e/org-roam-file-header-hide ()
  (when (org-roam-file-p)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#\\+title:\s*" nil t)
      (remove-overlays (point-min) (point))
      (let (( overlay (make-overlay (point-min) (point))))
        (add-to-invisibility-spec '(file-header . nil))
        (overlay-put overlay 'invisible 'file-header)
        (overlay-put overlay
                     'isearch-open-invisible
                     'delete-overlay))))))


(defun e/org-roam-file-header-show ()
  (remove-overlays (point-min) (point-max) 'invisible 'file-header))


(defun e/org-roam-file-header-refresh ()
  (when org-roam-file-header-hide-mode
    (e/org-roam-file-header-show)
    (e/org-roam-file-header-hide)
    (remove-hook 'window-state-change-hook
                 'e/org-roam-file-header-refresh)))


(define-minor-mode org-roam-file-header-hide-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " hide"
  :keymap nil
  (unless (org-roam-file-p)
    (user-error "Hide File Header: Not in a Org Roam buffer."))
  (cond (org-roam-file-header-hide-mode
         (add-hook 'window-state-change-hook
                   'e/org-roam-file-header-refresh)
         (e/org-roam-file-header-hide))
        (t
         (remove-hook 'window-state-change-hook
                      'e/org-roam-file-header-refresh)
         (e/org-roam-file-header-show))))



(provide 'org-roam-custom)

;;; org-roam-custom.el ends here
