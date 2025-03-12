;;; org-collect-links --- Org collect links

;;; Commentary:
;; Load this package with:
;; (require 'org-collect-links)

;;; Code:
;;*** concat files, libreoffice hyperlink


;; =HYPERLINK("/home/dan/.emacs.d/org/set_up_scenario.org";"SET UP A SCENARIO")&T(STYLE("Orglink"))


(defun e/org-insert-lo-calc-hyperlink ()
  (interactive)
  (let* (( doc-title (cadar (org-collect-keywords '("TITLE"))))
         ( heading (nth 4 (org-heading-components)))
         ( style "Orglink")
         ( hyperlink (concat "=HYPERLINK(\"" (buffer-file-name)
                             "\";\"" (or doc-title heading)
                             "\")&T(STYLE(\"" style "\"))")))
    (gui-set-selection nil hyperlink)
    (insert hyperlink)
    hyperlink))


(defun e/org-concat-files ( &rest files)
  (find-file (concat (file-name-directory (car files))
                     "concat.org"))
  (org-concat-mode 1)
  (set-window-buffer nil (current-buffer))
  ;; (delete-region (point-min) (point-max))
  (let (( inhibit-read-only t)
        file-string)
    (erase-buffer)
    (dolist ( file files)
      (with-temp-buffer
        (insert-file-contents file)
        (delete-region (point-min)
                       (if (re-search-forward "^\\*" nil t)
                           (goto-char (1- (point)))
                         (point-min)))
        (goto-char (point-max))
        (delete-region (if (re-search-backward "[^\s\t\n]" nil t)
                           (goto-char (1+ (point)))
                         (point-max))
                       (point-max))
        (put-text-property (point-min) (point-max) 'source file)
        (setq file-string (buffer-string)))
      (goto-char (point-max))
      (insert file-string "\n\n")))
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (read-only-mode 1))


(defun e/org-concat-edit ()
  (interactive)
  (find-file-other-frame (get-text-property (point) 'source))
  (set-window-buffer nil (current-buffer)))


(defun e/org-concat-refresh ()
  (interactive)
  (let (( point (point))
        files match)
    (goto-char (point-min))
    (while (setq match (text-property-search-forward 'source))
      (push (prop-match-value match) files))
    (apply 'e/org-concat-files (nreverse files))
    (goto-char point)))
  

(define-minor-mode org-concat-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " concat"
  :keymap `(,(cons (kbd "e") 'e/org-concat-edit)
    ,(cons (kbd "g") 'e/org-concat-refresh))
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an ORG buffer.")))


(setq e/org-collect-last-match-title nil
      e/org-collect-last-match-id nil
      e/org-collect-tag-inheritance-toggle nil)


(defun e/org-collect-entry-old ()
  (let (( entry (buffer-substring-no-properties
                 (point)
                 (save-excursion
                   (or (outline-next-heading) (point-max))))))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (when (re-search-forward org-heading-regexp nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-property-drawer-re nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-block-regexp nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-keyword-regexp nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (match-string 2) t nil nil 0))
      (goto-char (point-min))
      (while (re-search-forward "^\\+\s" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      ;; Shrink empty lines to one empty line
      (goto-char (point-min))
      (while (re-search-forward "^\\(\s*\n\\)+" nil t)
        (replace-match "\n"))
      ;; Delete whitespace at beginning and end
      (setq entry (string-trim (buffer-string)))
      (setq entry (buffer-string)))
    (when (string-match "[^\s\n\t]" entry)
      entry)))


(defun e/org-collect-headings ( &optional update context)
  (interactive)
  (let (( match (e/org-sparse-tree-match update))
        ( text "")
        ( bold 'bold)
        ( org-use-tag-inheritance e/org-sparse-tree-tag-inheritance-toggle))
    (unless update
      (setq e/org-collect-last-match-title
            (upcase (nth 4 (org-heading-components)))))
    (org-map-entries
     (lambda ()
       (let (( entry (e/org-collect-entry))
             ( title (upcase (nth 4 (org-heading-components))))
             ( id (org-id-get-create)))
         (setq text (concat text (if (eq bold 'bold) "\n" "\n\n")
                            (propertize title
                                        'face `( :underline t :weight ,bold)
                                        'mouse-face 'highlight
                                        'id id)
                            "\n  " (car (last (org-heading-components)))))
         (when entry
           (setq text (concat text "\n"
                              (propertize entry 'id id)))))
       (when context
         (org-show-context))
       (setq bold 'normal))
     match)
    (when (buffer-modified-p)
      (save-buffer))
    (save-selected-window
      (switch-to-buffer-other-window
       (format "*%s*" e/org-collect-last-match-title))
      (let (( inhibit-read-only t)
            ( window-line (cdr (nth 6 (posn-at-point))))
            ( point (point)))
        (erase-buffer)
        (insert text)
        (if (not update)
            (goto-char (point-min))
          (goto-char point)
          (recenter window-line))
        (set-buffer-modified-p nil)
        (org-read-mode)))))


(defun e/org-collect-headings-at-mouse (ev)
  (interactive "e")
  (mouse-set-point ev)
  (e/org-collect-headings))


(defun e/org-collect-update ()
  (interactive)
  (let* (( id (or e/org-sparse-tree-last-match-id
                  (user-error "No Last Match Found.")))
         ( m (org-roam-id-find id 'marker)))
    (with-current-buffer (marker-buffer m)
      (save-excursion
        (goto-char m)
        (e/org-collect-headings 'update)))))


(defun e/org-collect-after-save ()
  (when (and (org-roam-file-p)
             e/org-collect-last-match-id
             e/org-collect-last-match-title
             (get-buffer-window (format "*%s*" e/org-collect-last-match-title) t))
    (e/org-collect-update)))


(remove-hook 'after-save-hook 'e/org-collect-after-save)
;; (add-hook 'after-save-hook 'e/org-collect-after-save)


(defun e/org-collect-open-at-point ( &optional entry)
  (interactive)
  (let* (( id (get-text-property (point) 'id))
         ( m (org-roam-id-find id 'marker)))
    (switch-to-buffer-other-window (marker-buffer m))
    (goto-char m))
  (org-show-context)
  (when entry
    (org-show-entry)))


(defun e/org-collect-entry-open-at-point ()
  (interactive)
  (e/org-collect-open-at-point 'entry))


(defun e/org-collect-open-at-mouse (ev)
  (interactive "e")
  (mouse-set-point ev)
  (e/org-collect-open-at-point))


(defun e/org-collect-entry-open-at-mouse (ev)
  (interactive "e")
  (mouse-set-point ev)
  (e/org-collect-open-at-point 'entry))


(define-derived-mode org-read-mode text-mode "Org-Read"
  "Major mode for listing emacs buffers."
  (read-only-mode)
  (setq left-margin-width 10
        ;; right-margin-width 10
        fringes-outside-margins t
        left-fringe-width 40
        right-fringe-width 40)
  (set-window-buffer nil (current-buffer))
  (define-key org-read-mode-map (kbd "RET") 'e/org-collect-open-at-point)
  (define-key org-read-mode-map (kbd "M-RET") 'e/org-collect-entry-open-at-point)
  (define-key org-read-mode-map (kbd "m") 'e/org-collect-links-open-master)
  (define-key org-read-mode-map (kbd "<C-S-mouse-1>") 'e/org-collect-open-at-mouse)
  (define-key org-read-mode-map (kbd "<C-S-mouse-2>") 'e/org-collect-entry-open-at-mouse)
  )


(setq e/org-collect-links-last-buffer nil)


(defun e/org-collect-numbering ( level numbering)
  (let (( current (make-list level 0))
        new)
    (setcar (nthcdr (1- level) current) 1)
    (dolist (i current)
      (setq new (cons (+ (or (pop numbering) 0) i) new)))
    (nreverse new)))


(defun e/org-collect-join ( numbering)
  (let ( text)
    (dolist (number numbering)
      (setq text (concat text
                         (number-to-string number) ".")))
    text))
    
 
(defun e/org-collect-links-under-headings ( &optional update)
  (interactive)
  (setq e/org-collect-links-last-buffer (current-buffer))
  (let (( master-id (save-excursion
                      (goto-char (point-min))
                      (org-id-get-create)))
        ( weight 'bold)
        text numbering)
    (org-map-entries
     (lambda ()
       (let (( end (save-excursion
                     (or (outline-next-heading) (point-max))))
             ( section (upcase (nth 4 (org-heading-components))))
             ( level (car (org-heading-components))))
         (setq numbering (e/org-collect-numbering level numbering)
               section (concat (e/org-collect-join numbering) " "
                               section)
               text (concat text "\n\n"
                            (propertize section
                                        'face `( :underline t :weight ,weight))))
         (while (and (eq (org-next-link) t)
                     (< (point) end))
           (let* (( id (e/org-roam-link-copy-id))
                  ( m (org-roam-id-find id 'marker)))
             (with-current-buffer (marker-buffer m)
               (save-excursion
                 (goto-char m)
                 (let* (( entry (e/org-collect-entry))
                        ( aliases (org-entry-get (point-min) "ROAM_ALIASES"))
                        ( title (concat
                                 (if (org-before-first-heading-p)
                                     (cadar (org-collect-keywords '("title")))
                                   (nth 4 (org-heading-components)))
                                 (when aliases " ")
                                 aliases)))
                   (setq numbering (e/org-collect-numbering (1+ level) numbering))
                   (setq text (concat text "\n\n"
                                      (e/org-collect-join numbering) " "
                                      (propertize (upcase title)
                                                  'face `( :underline nil)
                                                  'mouse-face 'highlight
                                                  'id id)))
                   (when entry
                     (setq text (concat text "\n"
                                        (propertize entry 'id id)))))))))))
     nil 'tree)
    (save-selected-window
      (switch-to-buffer-other-window "*Org Read*")
      (let (( inhibit-read-only t)
            ( window-line (cdr (nth 6 (posn-at-point))))
            ( point (point)))
        (erase-buffer)
        (insert (propertize text 'master-id master-id))
        (if (not update)
            (goto-char (point-min))
          (goto-char point)
          (recenter window-line))
        (set-buffer-modified-p nil)
        (org-read-mode)))))


(defun e/org-collect-links ( &optional update)
  (interactive)
  (let (( master-id (save-excursion
                      (goto-char (point-min))
                      (org-id-get-create)))
        ( weight 'bold)
       ( end (save-excursion
               (or (outline-next-heading) (point-max))))
       text)
    (while (re-search-forward org-link-bracket-re nil t)
      (let* (( id (replace-regexp-in-string
                   "^id:" ""
                   (match-string-no-properties 1)))
             ( m (org-roam-id-find id 'marker)))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char m)
            (let* (( entry (e/org-collect-entry))
                   ( props (org-entry-properties))
                   ( aliases (org-entry-get (point-min) "ROAM_ALIASES"))
                   ( title (concat
                            (if (org-before-first-heading-p)
                                (cadar (org-collect-keywords '("title")))
                              (nth 4 (org-heading-components)))
                            (when aliases " ")
                            aliases)))
                   (setq text (concat text "\n\n"
                                      (propertize (upcase title)
                                                  'face `( :underline nil)
                                                  'mouse-face 'highlight
                                                  'id id)))
                   (when entry
                     (setq text (concat text "\n"
                                        (propertize entry 'id id)))))))))
    (save-selected-window
      (switch-to-buffer-other-window "*Org Read*")
      (let (( inhibit-read-only t)
            ( window-line (cdr (nth 6 (posn-at-point))))
            ( point (point)))
        (erase-buffer)
        (insert (propertize text 'master-id master-id))
        (if (not update)
            (goto-char (point-min))
          (goto-char point)
          (recenter window-line))
        (set-buffer-modified-p nil)
        (org-read-mode)))))


(defun e/org-collect-links-properties ()
  (save-excursion
    (goto-char (point-min))
    (let ( properties)
      (while (re-search-forward org-link-bracket-re nil t)
        (when (string-match-p "^id:" (match-string-no-properties 1))
          (setq properties
                (append (mapcar 'car (org-roam-node-properties
                         (org-roam-node-from-id
                          (replace-regexp-in-string
                           "^id:" "" (match-string-no-properties 1)))))
                        properties))))
      (delete-dups properties))))


(defun e/org-collect-links-at-heading ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (( props (completing-read-multiple
                   "Show Properties: "
                   (e/org-collect-links-properties)))
          ( re "^*+\s+\\|^[0-9]+\\.\s+"))
      (while (re-search-forward re nil t)
        (when (re-search-forward org-link-bracket-re (line-end-position) t)
          (let* (( id (replace-regexp-in-string
                       "^id:" ""
                       (match-string-no-properties 1)))
                 ( node (org-roam-node-from-id id)))
            (replace-match (org-make-link-string (concat "id:" id)
                                                 (org-roam-node-title node)))
            (let (( begin (line-beginning-position 2))
                  ( end (save-excursion
                          (if (re-search-forward re nil t)
                              (line-beginning-position)
                            (point-max)))))
              (delete-region begin end)
              (beginning-of-line 2)
              (dolist ( prop (org-roam-node-properties node))
                (when (member (car prop) props)
                  (insert (make-string 4 ? ) (car prop) ": " (cdr prop) "\n"))))))))))


(defun e/org-collect-links-open-master ()
  (interactive)
  (let* (( id (get-text-property (point) 'master-id))
         ( m (org-roam-id-find id 'marker)))
    (switch-to-buffer-other-window (marker-buffer m))))


(defun e/org-collect-links-update ()
  (interactive)
  (with-current-buffer e/org-collect-links-last-buffer
    (e/org-collect-links 'update)))


(defun e/org-collect-links-after-save ()
  (when (and (org-roam-file-p)
             e/org-collect-links-last-buffer
             (get-buffer-window "*Org Read*" t))
    (e/org-collect-links-update)))


(remove-hook 'after-save-hook 'e/org-collect-links-after-save)
(add-hook 'after-save-hook 'e/org-collect-links-after-save)


(setq e/org-roam-note-list-plain-last-window nil)


(defun e/org-roam-note-list-plain ( &optional exclude-ids)
  (interactive)
  (unless (eq major-mode 'oroam-mode)
    (if (window-live-p e/org-roam-note-list-plain-last-window)
        (select-window e/org-roam-note-list-plain-last-window)
      (when (< (length (window-list)) 2)
        (split-window))
      (other-window 1)
      (setq e/org-roam-note-list-plain-last-window (selected-window)))
    (switch-to-buffer "*ORoam*"))
  (let (( window-line (cdr (nth 6 (posn-at-point))))
        ( point (point))
        ( inhibit-read-only t)
        ( results (sort
                   (org-roam-db-query "SELECT
title, file, id,
'(' || group_concat(alias, ' ') || ')',
refs,
properties,
olp,
level
FROM (SELECT
  title, file, id, 
  alias,
  '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs,
  properties,
  olp,
  level
  FROM nodes
  LEFT JOIN aliases ON aliases.node_id = nodes.id
  LEFT JOIN refs ON refs.node_id = nodes.id
  GROUP BY nodes.id, aliases.alias ORDER BY alias ASC)
GROUP BY id ORDER BY id")
                   'e/org-roam-note-sort-next)))
    (erase-buffer)
    (remove-overlays)
    (let (( i 0))
      (dolist (result results)
        (let (( title (nth 0 result))
              ( file (nth 1 result))
              ( id (nth 2 result))
              ( aliases (e/org-roam-note-join (nth 3 result) " | " "[0-9]+:"))
              ( aliases-help (e/org-roam-note-join (nth 3 result) "\n" "[0-9]+:"))
              ( refs (nth 4 result))
              ( properties (nth 5 result))
              ( olp (e/org-roam-note-join (nth 6 result) "->"))
              ( level (if (> (nth 7 result) 0) "H" "F")))
          (unless (or (member id exclude-ids)
                      (string-match-p "refs/\\|daily/" file))
            (setq i (1+ i))
            (insert
             (concat (format "%4d" i) " "
                     level " "
                     (propertize (if (or (not title)
                                         (string-empty-p title))
                                     id title)
                                 'title title
                                 'id id
                                 'refs (and refs t)
                                 'file file
                                 'font-lock-face 'org-link
                                 'rear-nonsticky (list 'font-lock-face)
                                 'mouse-face 'highlight
                                 'help-echo (concat aliases-help "\n" 
                                                    id "\n"
                                                    file "\n"
                                                    olp))
                     (propertize " " 'mouse-face 'highlight)
                     (propertize aliases
                                 'mouse-face 'highlight
                                 'id id
                                 'help-echo aliases-help)
                     "\n"))))))
    (oroam-mode)
    (goto-char point)
    (recenter window-line)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))


(defun e/org-roam-note-list-plain-exclude ()
  (interactive)
  (let ( id-list)
    (dolist ( file (directory-files "/home/dan/.emacs.d/org-roam/master" t "^[^.]" t))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward org-link-bracket-re nil t)
          (when (string-match-p "^id:" (match-string-no-properties 1))
            (let* (( id (replace-regexp-in-string "^id:" ""
                                                  (match-string-no-properties 1))))
              (setq id-list (cons id id-list)))))
        (goto-char (point-min))
        (while (re-search-forward ":ID:\s+\\([0-9a-z-]+\\)" nil t)
          (setq id-list (cons (match-string 1) id-list)))))
    (delete-dups id-list)
    (e/org-roam-note-list-plain id-list)))


(defun e/org-roam-heading-index ( &optional update context)
  (interactive)
  (unless (eq major-mode 'oroam-mode)
    (switch-to-buffer-other-window "*ORoam*"))
  (let (( window-line (cdr (nth 6 (posn-at-point))))
        ( point (point))
        ( inhibit-read-only t)
        ( chars '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
                    "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
                    "U" "V" "W" "X" "Y" "Z"))
        index-list)
    (dolist ( file e/org-roam-note-list-plain-exclude-from-files)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let* (( title (downcase (nth 4 (org-heading-components))))
                  ( index-entry (assoc title index-list))
                  ( id (org-id-get-create)))
             (if index-entry
                 (setq index-list (delete index-entry index-list)
                       index-entry `(,title . ,(cons id (cdr index-entry))))
               (setq index-entry `(,title . ,(list id))))
             (setq index-list (cons index-entry index-list)))))))
    (dolist ( char chars)
      (setq index-list
            (cons `(,(propertize char
                                 'font-lock-face 'bold
                                 'heading t)
                    . nil)
                  index-list)))
    (setq index-list (sort index-list
                           (lambda ( a b) (string< (downcase (car a))
                                                   (downcase (car b))))))
    (erase-buffer)
    (remove-overlays)
    (dolist ( index-entry index-list)
      (unless (get-text-property 0 'heading (car index-entry))
        (insert "  "))
      (insert (capitalize (car index-entry)))
      (let (( i 0))
          (dolist ( id (cdr index-entry))
            (insert " " (propertize (concat "["
                                            (number-to-string (setq i (1+ i)))
                                            "]")
                                  'id id
                                  'font-lock-face 'org-link
                                  'mouse-face 'highlight))))
      (insert "\n"))
    (oroam-mode)
    (goto-char point)
    (recenter window-line)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))


;;*** collect links tree


(defun e/org-collect-links-to-properties ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":link\\+?:" nil t)
      (delete-region (line-beginning-position) (line-beginning-position 2)))
    (re-search-forward org-property-end-re nil t)
    (when (re-search-forward "^\\*\s+Subnodes\s*$" nil t)
      (let (( end (save-excursion
                    (or (outline-get-next-sibling) (point))))
            ids)
        (while (and (re-search-forward org-link-bracket-re nil t)
                    (< (point) end))
          (when (string-match-p "^id:" (match-string-no-properties 1))
            (setq ids (cons (replace-regexp-in-string
                             "^id:" "" (match-string-no-properties 1))
                            ids))))
        (goto-char (point-min))
        (re-search-forward org-property-end-re nil t)
        (beginning-of-line)
        (dolist ( id (nreverse ids))
          (insert ":link+: " id "\n")))))
  (org-hide-drawer-all))


(defun e/org-collect-links-to-properties-before-save ()
  (when (org-roam-file-p)
    (e/org-collect-links-to-properties)))


;; (add-hook 'before-save-hook 'e/org-collect-links-to-properties-before-save)
(remove-hook 'before-save-hook 'e/org-collect-links-to-properties-before-save)


(defun e/org-collect-links-props ( id)
  (let* (( title-props-file
           (car (org-roam-db-query [:select [title properties file]
                                            :from nodes
                                            :where (= id $s1)]
                                   id))))
    title-props-file))


(defun e/org-roam-id-file ( id)
  (caar (org-roam-db-query [:select [file]
                                    :from nodes
                                    :where (= id $s1)]
                           id)))


(defun e/org-roam-insert-file-contents ( file)
  (insert-file-contents file)
  (org-mode)
  (goto-char (point-min)))


(defun e/org-roam-id-insert-file-contents ( id)
  (insert-file-contents
   (caar (org-roam-db-query [:select [file]
                                     :from nodes
                                     :where (= id $s1)]
                            id)))
  (org-mode)
  (goto-char (point-min))
  (re-search-forward id nil t)
  (if (org-before-first-heading-p)
      (goto-char (point-min))
    (org-back-to-heading)))


(defun e/org-collect-links-tree ( &optional expose-id refresh)
  (interactive)
  (setq e/org-collect-links-tree-indent "   ")
  (let (( inhibit-read-only t)
        ( parent-indent "")
        ( next t)
        ( buffer-id (or expose-id
                        (org-entry-get (point-min) "id")
                        (get-text-property (point) 'id)))
        ids root-id)
    (with-current-buffer (find-file-noselect "/home/dan/.emacs.d/org-roam/master/root.org")
      (goto-char (point-min))
      (setq root-id (org-id-get-create))
      (when (re-search-forward "^\\*\s+Subnodes\s*\n" nil t)
        (while (re-search-forward org-link-bracket-re nil t)
          (when (string-match-p "^id:" (match-string-no-properties 1))
            (setq ids (cons (replace-regexp-in-string
                             "^id:" "" (match-string-no-properties 1))
                            ids))))))
    (unless refresh
      (unless (get-buffer-window "*Roam-Tree*")
        (switch-to-buffer-other-window "*Roam-Tree*"))
      (pop-to-buffer "*Roam-Tree*"))
    (erase-buffer)
    (roam-tree-mode)
    (insert (propertize (car (e/org-collect-links-props root-id))
                        'id root-id
                        'parent-id "ROOT"
                        'parent-ids "ROOT"
                        'rear-nonsticky t
                        'font-lock-face 'org-link
                        'help-echo (concat root-id "\n"
                                           "/home/dan/.emacs.d/org-roam/master/root.org")
                        'mouse-face 'highlight)
            "\n")
    (while next
      (setq next nil)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" parent-indent "[^\s]") nil t)
        (let* (( parent-id (get-text-property (point) 'id))
               ( parent-ids (concat
                             (get-text-property (point) 'parent-ids)
                             " " (get-text-property (point) 'id)))
               ( properties (cadr (e/org-collect-links-props parent-id)))
               ( links (or (cdr (assoc "LINK" properties)) "")))
          (end-of-line)
          (dolist ( id (split-string links))
            (setq next t)
            (let* (( title-props-file (e/org-collect-links-props id))
                   ( title (or (car title-props-file) "*FAILED*"))
                   ( props (or (cadr title-props-file) "*FAILED*"))
                   ( file (or (caddr title-props-file) "*FAILED*")))
              (when title-props-file
                (insert "\n"
                        (concat parent-indent e/org-collect-links-tree-indent)
                        (propertize title
                                    'id id
                                    'file file
                                    'parent-id parent-id
                                    'parent-ids parent-ids
                                    'rear-nonsticky t
                                    'font-lock-face 'org-link
                                    'help-echo (concat id "\n" file)
                                    'mouse-face 'highlight)))))))
      (setq parent-indent (concat parent-indent e/org-collect-links-tree-indent)))
    (e/org-collect-links-fold-all)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (read-only-mode 1)
    (when buffer-id
      (e/org-collect-links-show-id buffer-id))))


(defun e/org-collect-links-fold-region ()
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at "^\s*")
      (let (( parent-indent (match-string-no-properties 0))
            ( beg (line-end-position)))
        (beginning-of-line 2)
        (while (looking-at (concat "^" parent-indent e/org-collect-links-tree-indent))
          (beginning-of-line 2))
        (cons beg (line-end-position 0))))))


(defun e/org-collect-links-fold-p ()
  (save-excursion
    (end-of-line)
    (when-let (( overlays (overlays-at (point))))
      (let ( found)
        (while overlays
          (when (= (overlay-start (pop overlays)) (point))
            (setq found t
                  overlays nil)))
        found))))


(defun e/org-collect-links-fold ()
  (save-excursion
    (unless (e/org-collect-links-fold-p)
      (when-let (( begin-end (e/org-collect-links-fold-region)))
        (let (( inhibit-read-only t)
              ( overlay (make-overlay (car begin-end) (cdr begin-end))))
          (overlay-put overlay 'invisible 'roam-tree)
          (overlay-put overlay 'isearch-open-invisible 'delete-overlay))))))


(defun e/org-collect-links-fold-block ( &optional start end)
  (unless end
    (unless (setq end (cdr (e/org-collect-links-fold-region)))
      (user-error "No folding limit specified.")))
  (unless start
    (setq start (line-beginning-position)))
  (save-excursion
    (goto-char end)
    (while (> (point) start)
      (beginning-of-line 0)
      (e/org-collect-links-fold))))


(defun e/org-collect-links-fold-all ()
  (interactive)
  (let (( beg (save-excursion
                (goto-char (point-min))
                (line-beginning-position 2))))
    (e/org-collect-links-fold-block beg (point-max))))


(defun e/org-collect-links-fold-unfold-all ()
  (interactive)
  (remove-overlays))


(defun e/org-collect-links-fold-toggle ()
  (interactive)
  (cond ((e/org-collect-links-fold-p)
         (e/org-collect-links-fold-unfold))
        (t
         (e/org-collect-links-fold-block))))


(defun e/org-collect-links-fold-mouse-toggle ( event)
  "Toggle visibility of current fold by mouse.

EVENT refers to the mouse button press."
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (e/org-collect-links-fold-toggle))))


(defun e/org-collect-links-fold-unfold ()
  (save-excursion
    (end-of-line)
    (let (( inhibit-read-only t)
          ( overlays (overlays-at (point))))
      (while (setq overlay (pop overlays))
        (when (= (overlay-start overlay) (point))
          (delete-overlay overlay)
          (setq overlays nil))))))


(define-derived-mode roam-tree-mode outline-mode "RoamTree"
    "Major mode for listing emacs buffers."
    (unless (eq (current-buffer) (get-buffer "*Roam-Tree*"))
      (error "ORoam: current buffer is no ORoam buffer."))
    (setq truncate-lines t)
    (add-to-invisibility-spec '(roam-tree . t))
    (define-key roam-tree-mode-map (kbd "g") 'e/org-collect-links-tree)
    (define-key roam-tree-mode-map (kbd "r") 'e/org-collect-links-tree-read)
    (define-key roam-tree-mode-map (kbd "j") 'e/org-collect-links-tree-subtree-json)
    (define-key roam-tree-mode-map (kbd "u") 'e/org-collect-links-tree-read-refresh)
    (define-key roam-tree-mode-map (kbd "e") 'e/org-collect-links-tree-edit-region)
    (define-key roam-tree-mode-map (kbd "s") 'e/org-roam-note-list-isearch)
    (define-key roam-tree-mode-map (kbd "c") 'e/org-roam-note-set-color)
    (define-key roam-tree-mode-map (kbd "i") 'org-roam-note-list-insert-mode)
    (define-key roam-tree-mode-map (kbd "C-c c") 'e/org-roam-link-copy-id)
    (define-key roam-tree-mode-map (kbd "C-c s") 'e/org-roam-link-scan)
    (define-key roam-tree-mode-map (kbd "C-c C-c") 'e/org-collect-links-show-off)
    (define-key roam-tree-mode-map
      (kbd "<mouse-2>") 'e/org-roam-note-list-open-at-mouse)
    (define-key roam-tree-mode-map
      (kbd "<mouse-3>") 'e/org-roam-note-list-open-at-mouse-ow)
    (define-key roam-tree-mode-map
      (kbd "<C-M-mouse-2>") 'e/org-collect-links-fold-unfold-all)
    (define-key roam-tree-mode-map
      (kbd "<C-M-mouse-3>") 'e/org-collect-links-fold-all)
    (define-key roam-tree-mode-map
      (kbd "<S-mouse-2>") 'e/org-collect-links-fold-mouse-toggle))


(defun e/org-collect-links-show-id ( id)
  (goto-char (point-min))
  (let (( inhibit-read-only t)
        match)
    (while (setq match (text-property-search-forward 'id id t))
      (let (( overlay (make-overlay (prop-match-beginning match)
                                    (prop-match-end match))))
        (overlay-put overlay 'font-lock-face 'secondary-selection)
        (overlay-put overlay 'match t))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "^\s+")
          (let (( indent (string-trim (match-string-no-properties 0) ""
                                      e/org-collect-links-tree-indent)))
            (while (and (>= (length indent) (length e/org-collect-links-tree-indent))
                        (re-search-backward (concat "^" indent "[^\s]") nil t))
              (when (e/org-collect-links-fold-p)
                (e/org-collect-links-fold-unfold))
              (setq indent (string-trim indent ""
                                        e/org-collect-links-tree-indent)))))))))


(defun e/org-collect-links-show-off ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'match t))


(defun e/org-collect-entry ( &optional start export)
  (let (( entry (save-excursion
                  (when start
                    (goto-char start))
                  (buffer-substring-no-properties
                   (point)
                   (or (outline-next-heading) (point-max))))))
    (with-temp-buffer
      (insert entry)
      ;; (goto-char (point-min))
      ;; (when (re-search-forward org-heading-regexp nil t)
      ;;   (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-property-drawer-re nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      ;; (goto-char (point-min))
      ;; (while (re-search-forward org-block-regexp nil t)
      ;;   (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-footnote-re nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-keyword-regexp nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (let* (( type-id (save-match-data
                           (string-split (match-string 1) ":")))
               ( type (car type-id))
               ( link-id (cadr type-id))
               ( description (match-string 2)))
          (cond ((string= type "cite")
                 (cond ((eq export 'latex)
                        (replace-match (concat "\\\\cite{" link-id "}")
                                       t nil nil 0))
                       ((eq export 'html)
                        (replace-match (concat "<a href=\"emacs:cite:" link-id "\">"
                                               description
                                               "</a>")
                                       t nil nil 0))
                       (t
                        (replace-match (concat "\\\\cite{" link-id "}")
                                       t nil nil 0))))
                ((string-prefix-p "http" type 'ignore-case)
                 (replace-match (concat "<a target=\"_blank\" href=\"" type ":" link-id "\">"
                                               description
                                               "</a>")
                                       t nil nil 0))
                ((string= type (match-string 1))
                 (replace-match (match-string 1)
                                t nil nil 0))
                (t
                 (replace-match (or description (match-string 1))
                                t nil nil 0)))))
      ;; Shrink empty lines to one empty line
      (goto-char (point-min))
      (while (re-search-forward "^\\(\s*\n\\)+" nil t)
        (replace-match "\n"))
      ;; Delete whitespace at beginning and end
      (setq entry (string-trim (buffer-string))))
    ;; Return non-empty string or nil
    (when (string-match "[^\s\n\t]" entry)
      entry)))


(defun e/org-parse-description-list ( heading)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\*\s+" heading "\s*$") nil t)
      (when (re-search-forward "^\s*\\([0-9a-zA-Z][0-9]*[.)]\\|-\\)\s+" nil t)
        (mapcar (lambda ( item)
                  (let (( key-value (string-split (car item) "\s+::\s+")))
                    (cons (downcase (car key-value)) (cadr key-value))))
                (cdr (org-list-to-lisp)))))))


(defun e/org-parse-ids-description-list ( ids heading)
  (let ( columns)
    (while (and (not columns) ids)
      (let (( file (e/org-roam-id-file (pop ids))))
        (when file
          (with-temp-buffer
            (e/org-roam-insert-file-contents file)
            (setq columns (e/org-parse-description-list "columns"))))))
    columns))


(defun e/org-roam-ids-to-files ( ids)
  (let ( files)
    (dolist ( id ids)
      (setq files (cons (e/org-roam-id-file id) files)))
    (remove nil files)))
          

(defun e/org-collect-links-get-props ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*\s+props\s*$" nil t)
      (when (re-search-forward "^\s*\\([0-9a-zA-Z][0-9]*[.)]\\|-\\)\s+" nil t)
        (mapcar (lambda ( item)
                  (let (( key-value (string-split (car item) "\s+::\s+")))
                    (cons (upcase (car key-value)) (cadr key-value))))
                (cdr (org-list-to-lisp)))))))
    

(defun e/org-collect-links-get-show ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*\s+show\s*$" nil t)
      (when (re-search-forward "^\s*\\([0-9a-zA-Z][0-9]*[.)]\\|-\\)\s+" nil t)
        (mapcar 'car (cdr (org-list-to-lisp)))))))


(defun e/org-collect-links-set-props ( props show)
  (let (( indent "   ")
        props-string show-prop)
    (dolist ( prop show)
      (when (setq show-prop (assoc (downcase prop) props))
        (setq props-string
              (concat props-string indent "- " (car show-prop) " :: " (cdr show-prop) "\n"))))
    props-string))


(defun e/org-collect-links-tree-read ()
  (interactive)
  (beginning-of-line)
  (setq e/org-collect-links-tree-read-last-point (point))
  (let (( i 0)
        indent master-title master-id text html titles columns)
    (while (re-search-forward (concat "^" indent "\s*")
                              (line-end-position 2) t)
      (setq indent (or indent
                       (concat (match-string-no-properties 0)
                               e/org-collect-links-tree-indent)))
      (let (( id (get-text-property (point) 'id))
            ( parent-id (get-text-property (point) 'parent-id))
            ( parent-ids (string-split (get-text-property (point) 'parent-ids)))
            ( point-bug (point)))
        (with-temp-buffer
          (e/org-roam-id-insert-file-contents id)
          (let* (( title (cadar (org-collect-keywords '("title"))))
                 ( properties (org-entry-properties (point-min)))
                 ( entry (e/org-collect-entry (point-min)))
                 ( props (e/org-parse-description-list "props")))
            (unless master-title
              (setq master-title title
                    master-id id
                    e/org-collect-links-tree-read-last-id id
                    ;; columns (e/org-parse-description-list "columns")
                    columns (e/org-parse-ids-description-list
                             (cons id parent-ids)
                             "columns")
                    html (e/org-html-table-header title id columns)))
            (when entry
              (setq text (concat text "\n\n"
                                 (propertize entry
                                             'id id
                                             'help-echo (concat id "\n" title)))
                    i (1+ i)
                    titles (concat titles "\n" (number-to-string i) ". "
                                   title)
                    html (concat html (e/org-html-table-row-at-point columns i))))))
        (goto-char point-bug)))
    ;; (save-selected-window
    ;;   (switch-to-buffer-other-window "*Org Read*")
    ;;   (let (( inhibit-read-only t)
    ;;         ( window-line (cdr (nth 6 (posn-at-point))))
    ;;         ( point (point)))
    ;;     (erase-buffer)
    ;;     (insert "#+OPTIONS: toc:nil"
    ;;             "\n#+LATEX_HEADER: \\setlist{noitemsep}"
    ;;             "\n#+title: " master-title
    ;;             "\n" titles
    ;;             "\n\\newpage"
    ;;             "\n" text
    ;;             "\n\\bibliographystyle{plainnat}"
    ;;             "\n\\bibliography{/home/dan/library/database/reference}")
    ;;     (goto-char (point-min))
    ;;     ;; (e/org-export-to-latex)     ; -> org-to-latex.tex -> org-to-latex.pdf
    ;;     (set-buffer-modified-p nil)
    ;;     (org-read-mode)))
    (e/org-html-table master-title master-id html)))


(defun e/org-collect-links-tree-read-refresh ( &optional no-tree)
  (interactive)
  (when (and (org-roam-buffer-p)
             (buffer-modified-p))
    (save-buffer))
  (if (and (get-buffer "*Roam-Tree*")
           (boundp 'e/org-collect-links-tree-read-last-id))
      (with-current-buffer "*Roam-Tree*"
        (unless no-tree
          (e/org-collect-links-tree e/org-collect-links-tree-read-last-id 'refresh))
        (goto-char (point-min))
        (if (not (and (text-property-search-forward
                       'id e/org-collect-links-tree-read-last-id)
                      (not (= (point) (point-max)))))
            (user-error "Tree Structure Changed. Please Set New Read Point.")
          ;; (e/org-collect-links-tree-read)
          (e/org-collect-links-tree-subtree-json)))
    (user-error "No Roam-Tree Buffer. Please Open Roam-Tree Buffer and Set Read Point.")))


(defun e/org-export-to-latex ()
  (let* (( dir (expand-file-name "~/.emacs.d/org/latex/"))
         ( outfile (concat dir "org-to-latex.tex"))
         ( tex-buffer (get-file-buffer outfile))
         ( org-latex-pdf-process
           (list (concat "latexmk -bibtex -pdf -f -interaction=nonstopmode"
                   " -outdir=" dir
                   " -pdflatex=\"pdflatex -shell-escape -synctex=1 -interaction=nonstopmode\""
                   " %F"))) ;; %F replace by abs file name passed to org-latex-compile
         ( inhibit-message t))
    (org-export-to-file 'latex outfile nil nil nil nil nil
                        #'org-latex-compile)
    (unless tex-buffer
      (setq tex-buffer (find-file-noselect outfile)))
    (with-current-buffer tex-buffer
      (revert-buffer nil t)
      (TeX-view))
    (org-export-to-file 'md (concat dir "org-to-latex.md"))))


(defun e/org-collect-links-tree-json-to-html ()
  (start-process "org-to-html" nil 
                 "/home/dan/.emacs.d/org/html/org_to_html.py"
                 "/home/dan/.emacs.d/org/html/template.html"
                 "/home/dan/.emacs.d/org/html/subtree.json"
                 "/home/dan/.emacs.d/org/html/out2.html"))


(defun e/org-collect-links-tree-live-server ( &optional file)
  (unless file
    (setq file "~/.emacs.d/org/html/out2.html"))
  (unless (get-process "live-server")
    (let (( default-directory (file-name-directory file)))
      (start-process "live-server" nil "live-server"
                     (concat "--open=" (file-name-nondirectory file))))))


(defun e/org-collect-links-tree-subtree-json ()
  (interactive)
  (let (( json-file (expand-file-name "/home/dan/.emacs.d/org/html/subtree.json"))
        indent array master-id)
    (save-excursion
      (beginning-of-line)
      (setq e/org-collect-links-tree-read-last-point (point))
      (while (re-search-forward (concat "^" indent "\s*")
                                (line-end-position 2) t)
        (setq indent (or indent
                         (concat (match-string-no-properties 0)
                                 e/org-collect-links-tree-indent)))
        (let (( id (get-text-property (point) 'id))
              ( parent-id (get-text-property (point) 'parent-id))
              ( parent-files (e/org-roam-ids-to-files
                              (string-split (get-text-property (point) 'parent-ids))))
              ( file (get-text-property (point) 'file))
              ( point-bug (point)))
          (unless master-id
            (setq master-id id
                  e/org-collect-links-tree-read-last-id id))
          (setq array (cons `(( id . ,id)
                              ( parent-id . ,parent-id)
                              ( file . ,file)
                              ( parent-files . ,parent-files)
                              ( all-parent-files . ,(e/org-collect-links-tree-all-parents id)))
                            array)))))
    (with-temp-file json-file
      (insert (json-encode (nreverse array))))
    (e/org-collect-links-show-off)
    (e/org-collect-links-show-id master-id))
  (e/org-collect-links-tree-json-to-html)
  (e/org-collect-links-tree-live-server))
           


(provide 'org-collect-links)

;;; org-collect-links.el ends here
