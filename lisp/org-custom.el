;;; org-custom --- My custom org configuration -*- lexical-binding: t; -*-


;;; Commentary:


;;; Code:


;; org export to markdown:

(require 'org)
(require 'org-element)
(require 'org-colview)
(require 'ob-core)
(require 'ox-ascii)
(require 'latex)
(require 'org-transclusion)


;;*** Org Babel


(defvar org-tangle-base-buffer "")


(defun org-tangle-file-mode ( mode-str)
  (let (( dir (file-name-directory (buffer-file-name)))
          modes filename)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
                 ":header-args:\s+:tangle\s+\\([^\s\n]+\\)" nil t)
        (setq filename (concat dir (match-string 1))
              modes (file-modes-symbolic-to-number
                        mode-str (file-modes filename)))
        (set-file-modes filename modes)))))


(add-hook 'org-babel-pre-tangle-hook
          (lambda ()
            (setq org-tangle-base-buffer (current-buffer))))


(add-hook 'org-babel-post-tangle-hook
          (lambda ()
            (with-current-buffer org-tangle-base-buffer
              (org-tangle-file-mode "u+x"))))

(defun e/org-comment-dwim (&optional arg)
  (interactive "P")
  (or (org-babel-do-key-sequence-in-edit-buffer (kbd "M-;"))
      (comment-dwim arg)))


;;*** Export current file to html, pdf via tex, txt


(defun org-dan-get-mime-type ( path)
  (mailcap-extension-to-mime (file-name-extension path)))


(defun org-dan-set-file-name (buffer extension &optional path)
  "Extension example: txt
path example: /home/dan/org/"
  (concat path
          (file-name-sans-extension (buffer-name buffer))
          "." extension))


(defun org-ascii-export-as-utf8 ()
  (interactive)
  (save-buffer)
  (setq outDir (concat default-directory "out/"))
  (setq old (org-dan-set-file-name (window-buffer) "txt"))
  (setq new (org-dan-set-file-name (window-buffer) "txt" outDir))
  (org-ascii-export-to-ascii nil nil nil nil '(:ascii-charset utf-8))
  (rename-file old new t)
  (setq output (get-buffer new))
  (when output
        (with-current-buffer output
                             (revert-buffer :ignore-auto
                                            :noconfirm
                                            :preserve-modes))))


(defun wait-for-file (fileName maxIter)
  (setq count 0)
  (while (and (not (file-exists-p fileName)) (< count maxIter))
         (sleep-for 1)
         (setq count (1+ count)))
  (message "File %s exists after %s seconds..." fileName count))


(defun org-dan-choose-export-engine ( fileType &optional subtree-p)
  (cond ((string= fileType "txt")
         (org-ascii-export-to-ascii nil subtree-p nil nil
                                     '(:ascii-charset utf-8)))
        ((string= fileType "html")
         (org-html-export-to-html nil subtree-p nil nil
                                   '(:ascii-charset utf-8)))
        ((string= fileType "pdf")
         (org-latex-export-to-pdf nil subtree-p nil nil
                                   '(:ascii-charset utf-8)))))


(defun org-dan-view-txt ( fullPath)
  (setq output (find-file-noselect fullPath t nil))
  (with-current-buffer output (read-only-mode 1))
  (display-buffer-below-selected output nil))


(defun org-dan-export-current-buffer ()
  (interactive)
  (save-buffer)
  ;; Always use german format for date
  (setq org-display-custom-times t)
  (let* (( bufBase (file-name-sans-extension
                      (buffer-name)))
         ( outDir (concat default-directory
                          (file-name-as-directory "out")))
         ( fileTypes '("html" "pdf" "txt" ))
           outFile old new)
    (dolist ( fileType fileTypes)
      (org-dan-choose-export-engine fileType)
      (setq outFile (concat bufBase "." fileType))
      (setq old (concat default-directory outFile))
      (setq new (concat outDir outFile))
      (wait-for-file old 10)
      (rename-file old new t))
    (rename-file (concat default-directory bufBase ".tex")
                 (concat outDir bufBase ".tex")  t)
    (delete-directory (concat default-directory "auto") t nil))
  (setq org-display-custom-times nil))


;;*** Exporting: Remove Headings. Taken from ox-extra.el


(defun org-export-ignore-headlines (data backend info)
  "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
  (org-element-map data 'headline
    (lambda (object)
      (when (member "ignore" (org-element-property :tags object))
        (let ((level-top (org-element-property :level object))
              level-diff)
          (mapc (lambda (el)
                  ;; recursively promote all nested headlines
                  (org-element-map el 'headline
                    (lambda (el)
                      (when (equal 'headline (org-element-type el))
                        (unless level-diff
                          (setq level-diff (- (org-element-property :level el)
                                              level-top)))
                        (org-element-put-property el
                          :level (- (org-element-property :level el)
                                    level-diff)))))
                  ;; insert back into parse tree
                  (org-element-insert-before el object))
                (org-element-contents object)))
        (org-element-extract-element object)))
    info nil)
  data)


(defun org-export-notignore-headlines (data backend info)
  "Remove headlines not tagged \"notignore\" retaining contents and promoting children.
Each headline tagged \"notignore\" will not be removed retaining its
contents and promoting any children headlines to the level of the
parent."
  (org-element-map data 'headline
    (lambda (object)
      (when (not (member "notignore" (org-element-property :tags object)))
        (let ((level-top (org-element-property :level object))
              level-diff)
          (mapc (lambda (el)
                  ;; recursively promote all nested headlines
                  (org-element-map el 'headline
                    (lambda (el)
                      (when (equal 'headline (org-element-type el))
                        (unless level-diff
                          (setq level-diff (- (org-element-property :level el)
                                              level-top)))
                        (org-element-put-property el
                                                  :level (- (org-element-property :level el)
                                                            level-diff)))))
                  ;; insert back into parse tree
                  (org-element-insert-before el object))
                (org-element-contents object)))
        (org-element-extract-element object)))
    info nil)
  data)


(add-hook 'org-export-filter-parse-tree-functions
          'org-export-ignore-headlines
          ;; 'org-export-notignore-headlines
          )
;; (remove-hook 'org-export-filter-parse-tree-functions
;;              'org-export-ignore-headlines
;;              ;; 'org-export-notignore-headlines
;;              )


;;*** Function for mouse behaviour


(defun e/org-mouse-cycle-subtree (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))) file)
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (org-cycle))))


(defun e/org-mouse-cycle-global (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))) file)
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (org-cycle-internal-global))))


(defun e/org-open-at-point-other-window ( event)
  (interactive "e")
  (let (( org-link-frame-setup '(( file . find-file-other-window))))
    (org-open-at-mouse event)))


;;*** Set variables custom


(setq org-time-stamp-custom-formats (quote
        ("%d.%m.%Y" . "%d.%m.%Y, %H:%M"))
      org-use-sub-superscripts nil
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
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers t
      org-log-note-clock-out t
      org-complete-tags-always-offer-all-agenda-tags t
      org-cite-global-bibliography '("/home/dan/library/database/reference.bib")
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
      org-show-context-detail
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
      org-use-tag-inheritance t
      org-odd-levels-only nil
      org-hide-leading-stars nil
      org-hide-emphasis-markers t
      org-adapt-indentation nil
      org-directory "~/.emacs.d/org"
      e/org-roam-note-list-plain-exclude-from-files
      '("/home/dan/.emacs.d/org-roam/master/story-ref.org"
        "/home/dan/.emacs.d/org-roam/master/pioneer-plant.org"
        "/home/dan/.emacs.d/org-roam/master/story-ideas.org")
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
      org-duration-format 'h:mm
      org-table-duration-hour-zero-padding nil
      org-startup-folded nil)


;;*** Set faces


(defun e/org-set-face-attributes ()
  (let (( height 1.2)
        ( ul nil)
        ( ol nil)
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


;;*** Latex handling, set variables for latex export and latex packages (e.g. minted)


(setq org-latex-listings 'minted
      org-highlight-latex-and-related '(latex)
      org-latex-packages-alist '(("" "minted")
                                 ("version=4" "mhchem")
                                 ("" "enumitem"))
      org-latex-minted-options '(("frame" "lines")))


;; Make sure to use emacs specific expansion strings and not the ones
;; from latexmk. E.g. replace the latexmk filename expansion "%S" for auctex use "%t"

(setq org-latex-pdf-process
      (list (concat "latexmk -bibtex -pdf"
 " -pdflatex=\"pdflatex -shell-escape -synctex=1 -interaction=nonstopmode\""
 " %f"))
      org-latex-pdf-process
      (list "latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
      org-latex-pdf-process
      (list "latexmk -outdir=/tmp/latexmk -f -pdf -%latex -interaction=nonstopmode %F"
            "mv %f /tmp/latexmk"
            "mv /tmp/latexmk/%b.pdf %o"))


(defun e/org-latex-environment-list ()
  (with-temp-buffer
    (insert "\\documentclass{article}
\\usepackage{amsmath}
\\begin{document}
\\end{document}")
    (latex-mode)
    (TeX-auto-parse)
    (LaTeX-environment-list-filtered)))


(defun e/org-latex-insert-ref ()
  (interactive)
  (insert "\\eqref{" (current-kill 0) "}"))


(defun e/org-latex-environment ( arg)
  (interactive "*P")
  (unless LaTeX-environment-list
    (setq LaTeX-environment-list (e/org-latex-environment-list)))
  (LaTeX-environment arg))


;;*** Set variables for Agenda and Notes customization


(setq org-agenda-files
      '("~/.emacs.d/org/goals.org"
        "~/.emacs.d/org/diary.org"
        "~/.emacs.d/org/notes.org"
        "~/.emacs.d/org/journal.org")
      e/org-agenda-files-orig org-agenda-files
      org-agenda-diary-file "~/.emacs.d/diary"
      org-agenda-include-diary t
      org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 3 :tags t :formula "$7=($4+$5+$6)/5;U::@1$7=5d")
      org-agenda-start-with-clockreport-mode nil
      org-agenda-time-grid
      '(( daily today)
        ( 800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-todo-ignore-scheduled 'all)


;;*** Set variables for Capture Templates and Notes


(setq org-capture-templates
      '(
;; --- TASKS ---
        ("t" "Tasks (notes.org)")
        ("ti" "TODO with inactive current date and time (time independent)"
         entry (file+headline "" "Tasks")
		 "* TODO %? %^g\n%U\n%a")
        ("ts" "TODO with scheduled active date"
         entry (file+headline "" "Tasks")
		 "* TODO [#A] %? %^g\nSCHEDULED: %^t")
        ("tS" "TODO with scheduled active date and time"
         entry (file+headline "" "Tasks")
		 "* TODO [#A] %? %^g\nSCHEDULED: %^T")
;; --- NOTES ---
        ("n" "Notes (notes.org), time not relevant")
        ("na" "Active date (find in agenda)"
         entry (file+headline "" "Notes")
         "* %? %^g\n%t"
         :empty-lines-before 1)
        ("nA" "Active date and time (find in agenda)"
         entry (file+headline "" "Notes")
         "* %? %^g\n%T"
         :empty-lines-before 1)
        ("nj" "Inactive date and time (find in tags match, without time in headline)"
         entry (file+headline "" "Notes")
         "* %? %^g\n%U"
         :empty-lines-before 1)
        ;; --- JOURNAL ---
        ("j" "Journal (journal.org), datetree, time of note relevant")
        ("jd" "inactive date in headline (find in tags match)"
         entry (file+olp+datetree "~/.emacs.d/org/journal.org")
         "* %u %? %^g"
         :time-prompt t)
        ("jt" "inactive date and time in headline (find in tags match)"
         entry (file+olp+datetree "~/.emacs.d/org/journal.org")
         "* %U %? %^g"
         :time-prompt t)
;; --- ROAM ---
        ("r" "Roam")
        ("rr" "Roam Node (file: timestamp.org)"
         plain (function e/org-roam-node-file-name-short)
         ""
         :empty-lines-before 1 :unnarrowed t)
        ("rR" "Roam Node with Title (file: timestamp-title.org)"
         plain (function e/org-roam-node-file-name-ask)
         ""
         :empty-lines-before 1 :unnarrowed t))
      org-default-notes-file "~/.emacs.d/org/notes.org")


;;*** refile target selection


(setq e/org-refile-target-location nil
      e/org-refile-source-marker nil)


(defun e/org-refile-target-set ()
  (interactive)
  (setq e/org-refile-target-location
        (list (nth 4 (org-heading-components))
              (buffer-file-name)
              nil
              (line-beginning-position))))


(defun e/org-refile-target-move ()
  (interactive)
  (when e/org-refile-target-location
    (org-refile nil nil e/org-refile-target-location)))


(defun e/org-refile-source-set ()
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min)
    (setq e/org-refile-source-marker (point-marker))))


(defun e/org-refile-source-move ()
  (interactive)
  (let (( target (list (nth 4 (org-heading-components))
                       (buffer-file-name)
                       nil
                       (line-beginning-position)))
        base-buffer)
    (with-current-buffer (marker-buffer e/org-refile-source-marker)
      (goto-char e/org-refile-source-marker)
      (org-refile nil nil target)
      (save-buffer)
      (when (and (setq base-buffer (buffer-base-buffer))
                 (eq (org-capture-get :type 'local) 'entry))
        (kill-buffer)))
    (save-buffer)))
  

;;*** column view reorder


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


;;*** html table export


(defun e/org-roam-network-protocol-emacs:id-old ( link)
  (let* (( protocol-id (string-split link ":"))
         ( protocol (car protocol-id))
         ( id (cadr protocol-id))
         ( opt-id (nth 2 protocol-id))
         ( window (selected-window)))
    (x-focus-frame nil)
    (find-file (caar (org-roam-db-query [:select [file]
                                    :from nodes
                                    :where (= id $s1)]
                                        id)))
    (when opt-id
      (cond ((string= opt-id "tree")
             (e/org-collect-links-tree id))
            ((t
              (goto-char (point-min))
              (while (re-search-forward opt-id nil t))))))
    (select-window window)))


(defun e/org-html-table ( table-rows-html &optional file)
  (with-temp-file (or file "/home/dan/.emacs.d/org/html/out.html")
    (insert-file-contents "/home/dan/.emacs.d/org/html/template.html")
    (goto-char (point-min))
    (search-forward "<!-- org-roam-rows -->")
    (setq table-rows-html (replace-regexp-in-string "\\\\" "" table-rows-html))
    (replace-match table-rows-html)
    (buffer-string)))


(defun e/org-html-table-header ( title id columns)
  (let (( html (concat "<tr><th></th><th>"
                       "<a href=\"emacs:" id "\">" title "</a>"
                       "<input id=\"viewAll\" type=\"button\" value=\"view all\"/>"
                       "<input id=\"hideAll\" type=\"button\" value=\"hide all\"/>"
                       "</th>\n")))
    (dolist ( column columns)
      (setq html (concat html "<th>" (cdr column) "</th>\n")))
    (concat html "</tr>\n")))
    

(defun e/org-html-table-row ( id tags)
  (let* (( file (caar (org-roam-db-query [:select [file]
                                                :from nodes
                                                :where (= id $s1)]
                                         id)))
         ( html "")
         title entry anchor entries)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward id nil t)
        (setq title (if (org-before-first-heading-p)
                        (when (re-search-forward "#\\+title:\s+\\(.*\\)\s*\n" nil t)
                          (match-string-no-properties 1))
                      (re-search-forward org-property-end-re nil t)
                      (beginning-of-line 2)
                      (nth 4 (org-heading-components)))
              anchor (point)
              entry (e/org-collect-entry))
        (dolist ( tag tags)
          (goto-char anchor)
          (when (re-search-forward (concat ":" tag ":") nil t)
            (beginning-of-line)
            (setq entries (cons (cons tag
                                      (cons (nth 4 (org-heading-components))
                                            (e/org-collect-entry)))
                                entries))))))
    (setq html (concat "<tr>\n<td>" title "</td>\n"))
    (dolist ( tag tags)
      (setq html (concat html "<td>" (cadr (assoc tag entries)) "</td>\n")))
    (setq html (concat html "</tr>\n"))
    (setq html (concat html "<tr class=\"toggle\">\n<td><div>" entry "</div></td>\n"))
    (dolist ( tag tags)
      (setq html (concat html "<td>" (cddr (assoc tag entries)) "</td>\n")))
    (setq html (concat html "</tr>"))
    html))


(defun e/org-html-table-org-block ()
  (re-search-forward org-block-regexp nil t)
  (when (string= (match-string-no-properties 1) "org")
    (e/org-html-table-export-entry (match-string-no-properties 4))))


(defun e/org-html-table-export-entry ( text)
  (when text
    (save-match-data
      (with-temp-buffer
        (insert text)
        (set-mark (point-min))
        (goto-char (point-max))
        (org-html-convert-region-to-html)
        (buffer-string)))))


(defun e/org-html-table-get-entry ( &optional start)
  (let (( entry (save-excursion
                  (when start
                    (goto-char start))
                  (buffer-substring-no-properties
                   (point)
                   (or (outline-next-heading) (point-max))))))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (while (re-search-forward org-property-drawer-re nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward org-block-regexp nil t)
        (when (string= (match-string 1) "org")
          (replace-match
           (e/org-html-table-export-entry (match-string-no-properties 4))
           t nil nil 0)))
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
                 (replace-match (concat "<a href=\"emacs:cite:" link-id "\">"
                                        description
                                        "</a>")
                                t nil nil 0))
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
      (concat "<p>"
              (replace-regexp-in-string "^\n" "</p><p>" entry)
              "</p>"))))


(defun e/org-color-name-to-hex ( color)
  (let* (( rgb (color-name-to-rgb color))
         ( r (nth 0 rgb))
         ( g (nth 1 rgb))
         ( b (nth 2 rgb)))
    (color-rgb-to-hex r g b 2)))


(defun e/org-html-table-row-at-point ( columns num)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#\\+title:\s+\\(.*\\)\s*\n" nil t)
    (let (( title (match-string-no-properties 1))
          ( anchor (point))
          ( entry (e/org-collect-entry))
          ( tags (mapcar 'car columns))
          ( child (concat "child-" (number-to-string num)))
          entries html)
      (dolist ( tag tags)
        (goto-char anchor)
        (when (re-search-forward (concat ":" tag ":") nil t)
          (beginning-of-line 2)
          (setq entries (cons (cons tag
                                    (cons (nth 4 (org-heading-components))
                                          (e/org-collect-entry)))
                              entries))))
      (setq html (concat "<tr class=\"parent\" data-child=\"" child "\">\n"
                         "<td>" (number-to-string num) "</td>"
                         "<td class=\"title\">" title "</td>\n"))
      (dolist ( tag tags)
        (let* (( color (e/org-prop-at-tag "color" tag))
               ( hex-color (when color (e/org-color-name-to-hex color))))
          (setq html (concat html
                             "<td"
                             (and hex-color (concat " style=\"background-color:" hex-color "\""))
                             ">\n"
                             "<div class=\"fixed\">" (cadr (assoc tag entries)) "</div>"
                             "</td>\n"))))
      (setq html (concat html "</tr>\n"))
      (setq html (concat html
                         "<tr class=\"toggle hidden " child "\">\n"
                         "<td></td>"
                         "<td>"
                         "<div>" (e/org-html-table-export-entry entry) "</div>"
                         "<a href=\"emacs:" id "\">edit</a>"
                         " <a href=\"emacs:" parent-id ":" id "\">edit parent</a>"
                         " <a href=\"emacs:" id ":tree\">edit tree</a>"
                         "</td>\n"))
      (dolist ( tag tags)
        (setq html (concat html "<td>"
                           "<div class=\"fixed\">"
                           (e/org-html-table-export-entry (cddr (assoc tag entries)))
                           "</div>"
                           "</td>\n")))
      (setq html (concat html "</tr>"))
      html)))


;;*** single file html export


(defun e/org-roam-network-protocol-emacs:id ( link)
  (let* (( file-id (string-split link ":"))
         ( protocol (car file-id))
         ( file (cadr file-id))
         ( id (caddr file-id))
         ( mark (cadddr file-id))
         ( window (selected-window)))
    (cond ((string= file "cite")
           (e/org-cite-open-file id nil))
          ((string= file "id")
           (x-focus-frame nil)
           (pop-to-buffer (find-file-noselect (caar (org-roam-db-query [:select [file]
                                    :from nodes
                                    :where (= id $s1)]
                                        id)))))
          ((string= id "unmark")
           (x-focus-frame nil)
           (pop-to-buffer (find-file-noselect file))
           (remove-overlays nil nil 'match t))
          ((string-prefix-p "c_" id)
           (x-focus-frame nil)
           (pop-to-buffer (find-file-noselect file))
           (goto-char (point-min))
           (re-search-forward "^\\*\s+#\s*columns" nil t)
           (org-fold-show-subtree)
           (re-search-forward id nil t))
          (t
           (x-focus-frame nil)
           (pop-to-buffer (find-file-noselect file))
           ;; (org-cycle-overview) ;; collapse all
           (goto-char (point-min))
           (when (re-search-forward id nil t)
             (org-back-to-heading)
             (if mark
                 (let (( overlay (make-overlay (point)
                                               (line-end-position))))
                   (overlay-put overlay 'font-lock-face 'secondary-selection)
                   (overlay-put overlay 'match t))
               (org-fold-show-subtree)))
           (select-window window)))))


(defun e/org-html-table-header-single-file ( file columns)
  (let (( html (concat "<tr><th></th><th>"
                       "<a href=\"emacs:" file "\">" file "</a>"
                       " <a href=\"emacs:" file ":unmark\">unmark</a>"
                       "<input id=\"viewAll\" type=\"button\" value=\"view all\"/>"
                       "<input id=\"hideAll\" type=\"button\" value=\"hide all\"/>"
                       "</th>\n")))
    (setq columns (delq (assoc "c_master" columns) columns))
    (dolist ( column columns)
      (setq html (concat html
                         "<th>"
                         (cdr column)
                         " <a href=\"emacs:" file ":" (car column) "\">edit</a>"
                         "</th>\n")))
    (concat html "</tr>\n")))


(defun e/org-html-table-single-file-alist-mod ( list key entry)
  (let ( item)
    (if (setq item (assoc key list))
        (let (( sublist (cdr item)))
          (while (assoc key list)
            (setq list (delq (assoc key list) list)))
          (cons `(,key . ,(list (nth 0 sublist) ; title
                                (nth 1 sublist) ; id
                                (nth 2 sublist) ; color
                                (nth 3 sublist) ; hex-color
                                entry))
                list))
      list)))
  

(defun e/org-html-table-single-file-backup ()
  (interactive)
  (e/org-single-file-id-all)
  (save-excursion
    (let* (( columns (e/org-parse-description-list "#columns"))
           ( file (buffer-file-name))
           ( html (e/org-html-table-header-single-file file columns))
           ( num 0)
           repeats)
      (goto-char (point-min))
      (while (re-search-forward "^\\*\s+[^#]" nil t)
        (setq num (1+ num))
        (let (( child (concat "child-" (number-to-string num)))
              ( alist repeats)
              tr-title tr-entry item)
          (org-map-entries
           (lambda ()
             (end-of-line)
             (let* (( components (org-heading-components))
                    ( level (nth 0 components))
                    ( title (nth 4 components))
                    ( tags (string-trim (or (nth 5 components) "c_master") ":" ":"))
                    ( id (org-entry-get (point) "id"))
                    ( color (org-entry-get (point) "color"))
                    ( hex-color (when color (e/org-color-name-to-hex color)))
                    ( export (org-entry-get (point) "export"))
                    ( repeat (org-entry-get (point) "repeat"))
                    ( title-start (if (string= repeat "start")
                                      (concat title "<span style=\"float:right\">&#9660;</span>")
                                    title))
                    ( entry (concat "<p>" (e/org-html-table-get-entry) "</p>")))
               (setq alist (if (and (setq item (cdr (assoc tags repeats)))
                                    (not (string= repeat "start")))
                               (cons `( ,tags . ,(list (pop item) ; title
                                                       (pop item) ; id
                                                       (pop item) ; color
                                                       (pop item) ; hex-color
                                                       entry))
                                     alist)
                             (cons `( ,tags . ,(list title-start id color hex-color entry)) alist)))
               (when repeat
                 (while (assoc tags repeats)
                   (setq repeats (delq (assoc tags repeats) repeats))))
               (when (string= repeat "start")
                 (setq repeats (cons `( ,tags . ,(list title id color hex-color nil)) repeats)))))
           nil 'tree)
          (dolist ( column columns)
            (let ( title id color hex-color entry)
              (when (setq list (assoc (car column) alist))
                (setq key (pop list)
                      title (pop list)
                      id (pop list)
                      color (pop list)
                      hex-color (pop list)
                      entry (pop list)))
              (if (string= (car column) "c_master")
                  (setq tr-title (concat "<td>" (number-to-string num) "</td>\n"
                                         "<td class=\"title\">" title "</td>\n")
                        tr-entry (concat "<td></td>\n"
                                         "<td>"
                                         "<div>" entry "</div>"
                                         "<a href=\"emacs:" file ":" id "\">edit</a>"
                                         "</td>\n"))
                (setq tr-title (concat tr-title
                                       "<td"
                                       (and hex-color (concat " style=\"background-color:" hex-color "\""))
                                       ">"
                                       "<div class=\"fixed\">" title "</div>"
                                       "</td>\n")
                      tr-entry (concat tr-entry
                                       "<td"
                                       (and hex-color (concat " style=\"background-color:" hex-color "\""))
                                       ">"
                                       "<div class=\"fixed\">" entry "</div>"
                                       "</td>\n")))))
          (setq html (concat html
                             "<tr class=\"parent\" data-child=\"" child "\">\n"
                             tr-title
                             "</tr>\n"
                             "<tr class=\"toggle hidden " child "\">\n"
                             tr-entry
                             "</tr>"))))
      (e/org-collect-links-tree-live-server "/home/dan/.emacs.d/org/html/out.html")
      (e/org-html-table html "/home/dan/.emacs.d/org/html/out.html"))))


(defun e/org-single-file-id-all ()
  (let ( ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\s+[^#\s]" nil t)
        (setq ids (cons (org-id-get-create) ids))))
    (nreverse ids)))


(defun e/org-html-table-single-file-show-ids ()
  (let (( all-ids (e/org-single-file-id-all))
        ( show (e/org-parse-description-list "#show"))
        ids)
    (dolist ( filter show)
      (goto-char (point-min))
      (while (re-search-forward (car filter) nil t)
        (when (string= (cdr filter) (nth 4 (org-heading-components)))
          (save-excursion
            (re-search-backward "^\\*\s+[^#\s]" nil t)
            (setq ids (cons (org-id-get-create) ids))))))
    (or (nreverse ids) all-ids)))


(defun e/org-single-file-edit-region-ids ()
  (let (( beg (save-excursion
                (when (region-active-p)
                  (goto-char (region-beginning)))
                (line-beginning-position)))
        ( end (save-excursion
                (when (region-active-p)
                  (goto-char (region-end)))
                (line-end-position)))
        ( filtered-ids (e/org-html-table-single-file-show-ids))
        ids)
    (goto-char end)
    (while (re-search-backward "^\\*\s" beg t)
      (let (( id (org-id-get-create)))
        (when (member id filtered-ids)
          (setq ids (cons id ids)))))
    ids))
    

(defun e/org-html-table-single-file ()
  (interactive)
  (save-excursion
    (let* (( ids (e/org-html-table-single-file-show-ids))
           ( columns (e/org-parse-description-list "#columns"))
           ( file (buffer-file-name))
           ( html (e/org-html-table-header-single-file file columns))
           ( num 0))
      (goto-char (point-min))
      (while (and ids (re-search-forward (pop ids) nil t))
        (setq num (1+ num))
        (let (( child (concat "child-" (number-to-string num)))
              alist tr-title tr-entry item)
          (org-map-entries
           (lambda ()
             (end-of-line)
             (let* (( components (org-heading-components))
                    ( level (nth 0 components))
                    ( title (nth 4 components))
                    ( tags (string-trim (or (nth 5 components) "c_master") ":" ":"))
                    ( id (org-entry-get (point) "id"))
                    ( color (org-entry-get (point) "color"))
                    ( hex-color (when color (e/org-color-name-to-hex color)))
                    ( entry (e/org-html-table-get-entry)))
               (setq alist (cons `( ,tags . ,(list title id color hex-color entry)) alist))))
           nil 'tree)
          (dolist ( column columns)
            (let ( title id color hex-color entry)
              (when (setq list (assoc (car column) alist))
                (setq key (pop list)
                      title (pop list)
                      id (pop list)
                      color (pop list)
                      hex-color (pop list)
                      entry (pop list)))
              (if (string= (car column) "c_master")
                  (setq tr-title (concat "<td>" (number-to-string num) "</td>\n"
                                         "<td class=\"title\">" title "</td>\n")
                        tr-entry (concat "<td></td>\n"
                                         "<td>"
                                         "<div>" entry "</div>"
                                         "<a href=\"emacs:" file ":" id "\">edit</a>"
                                         " <a href=\"emacs:" file ":" id ":mark\">mark</a>"
                                         "</td>\n"))
                (setq tr-title (concat tr-title
                                       "<td"
                                       (and hex-color (concat " style=\"background-color:" hex-color "\""))
                                       ">"
                                       "<div class=\"fixed\">" title "</div>"
                                       "</td>\n")
                      tr-entry (concat tr-entry
                                       "<td"
                                       (and hex-color (concat " style=\"background-color:" hex-color "\""))
                                       ">"
                                       "<div class=\"fixed\">" entry "</div>"
                                       "</td>\n")))))
          (setq html (concat html
                             "<tr class=\"parent\" data-child=\"" child "\">\n"
                             tr-title
                             "</tr>\n"
                             "<tr class=\"toggle hidden " child "\">\n"
                             tr-entry
                             "</tr>"))))
      (e/org-collect-links-tree-live-server "/home/dan/.emacs.d/org/html/out.html")
      (e/org-html-table html "/home/dan/.emacs.d/org/html/out.html"))))


(defun e/org-single-file-edit-headline ( tag title color &optional preserve)
  (let (( anchor (line-beginning-position))
        ( end (save-excursion
                (if (org-goto-sibling)
                    (point)
                  (point-max))))
        entry)
    (when (re-search-forward tag end t)
      (end-of-line)
      (setq entry (e/org-collect-entry))
      (when preserve
        (setq title (or (nth 4 (org-heading-components)) title)
              color (or (org-entry-get (point) "color") color)))
      (org-cut-subtree)
      (goto-char anchor))
    (org-insert-heading-respect-content)
    (insert title)
    (org-demote)
    (org-set-tags-to tag)
    (when entry
      (beginning-of-line 2)
      (insert entry "\n"))
    (org-back-to-heading)
    (when color
      (org-set-property "color" color))
    (goto-char anchor)))


(defun e/org-single-file-edit-region-old ( arg)
  (interactive "P")
  (setq e/org-single-file-ids (e/org-html-table-single-file-show-ids)
        e/org-single-file-preserve (not (equal arg '(4)))
        e/org-single-file-region-buffer (current-buffer)
        e/org-single-file-region-beg (save-excursion
                                       (when (region-active-p)
                                         (goto-char (region-beginning)))
                                       (line-beginning-position))
        e/org-single-file-region-end (save-excursion
                                       (when (region-active-p)
                                         (goto-char (region-end)))
                                       (line-end-position))
        e/org-single-file-region-tag (completing-read "Edit Column Tag: " org-tag-alist)
        e/org-single-file-region-headline (read-string "Cell Title: "))
  (list-colors-display
   nil nil
   (lambda ( color)
     (let (( beg e/org-single-file-region-beg)
           ( end e/org-single-file-region-end)
           ( tag e/org-single-file-region-tag)
           ( headline e/org-single-file-region-headline)
           ( preserve e/org-single-file-preserve))
       (kill-buffer "*Colors*")
       (pop-to-buffer e/org-single-file-region-buffer)
       (goto-char end)
       (while (re-search-backward "^\\*\s" beg t)
         (e/org-single-file-edit-headline tag headline color preserve))))))


(defun e/org-single-file-tag-headlines ( tag)
  (let ( headlines)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat ":" tag ":") nil t)
        (setq headlines (cons
                         `(,(nth 4 (org-heading-components)) . ,(org-entry-get (point) "color"))
                         headlines))))
    (nreverse headlines)))


(defun e/org-single-file-edit-region ( arg)
  (interactive "P")
  (setq e/org-single-file-ids (e/org-single-file-edit-region-ids)
        e/org-single-file-preserve (not (equal arg '(4)))
        e/org-single-file-region-buffer (current-buffer)
        e/org-single-file-region-tag (completing-read "Edit Column Tag: "
                                                      (org-get-buffer-tags)))
  (let* (( headlines (e/org-single-file-tag-headlines
                      e/org-single-file-region-tag))
         ( headline (completing-read "Cell Title: " headlines))
         ( goto-color (cdr (assoc headline headlines))))
    (setq e/org-single-file-region-headline headline)
    (list-colors-display
     nil nil
     (lambda ( color)
       (let (( ids e/org-single-file-ids)
             ( tag e/org-single-file-region-tag)
             ( headline e/org-single-file-region-headline)
             ( preserve e/org-single-file-preserve))
         (kill-buffer "*Colors*")
         (pop-to-buffer e/org-single-file-region-buffer)
         (goto-char (point-min))
         (while (and ids (re-search-forward (pop ids) nil t))
           (org-back-to-heading)
           (e/org-single-file-edit-headline tag headline color preserve)))
       ;; (org-element-cache-reset)
       (org-element--cache-sync (current-buffer))
       (e/org-html-table-single-file)
       ))
    (when goto-color
      (re-search-forward goto-color nil t))))


(defun e/org-single-file-remove-region-old ()
  (interactive)
  (let (( ids (e/org-single-file-edit-region-ids))
        ( beg (save-excursion
                (when (region-active-p)
                  (goto-char (region-beginning)))
                (line-beginning-position)))
        ( end (save-excursion
                (when (region-active-p)
                  (goto-char (region-end)))
                (line-end-position)))
        ( tag (completing-read "Remove Column Tag: " org-tag-alist)))
    (goto-char end)
    (while (re-search-backward "^\\*\s" beg t)
      (save-excursion
        (when (re-search-forward tag
                                 (save-excursion
                                   (if (org-goto-sibling)
                                       (point)
                                     (point-max)))
                                 t)
          (org-cut-subtree)))))
  (e/org-html-table-single-file))


(defun e/org-single-file-remove-region ()
  (interactive)
  (let (( ids (e/org-single-file-edit-region-ids))
        ( tag (completing-read "Remove Column Tag: " (org-get-buffer-tags))))
    (goto-char (point-min))
    (while (and ids (re-search-forward (pop ids) nil t))
      (save-excursion
        (when (re-search-forward tag
                                 (save-excursion
                                   (if (org-goto-sibling)
                                       (point)
                                     (point-max)))
                                 t)
          (org-cut-subtree)))))
  (e/org-html-table-single-file))


(defun e/org-single-file-export-json ()
  (interactive)
  (e/org-single-file-id-all)
  (save-excursion
    (let* (( columns (e/org-parse-description-list "#columns"))
           ( file (buffer-file-name))
           ( html (e/org-html-table-header-single-file file columns))
           ( num 0)
           ( global-array (list `("columns" . ,columns) `("file" . ,(buffer-file-name))))
           entry-array)
      (goto-char (point-min))
      (while (re-search-forward "^\\*\s+[^#]" nil t)
        (setq num (1+ num))
        (let (( child (concat "child-" (number-to-string num)))
              alist)
          (org-map-entries
           (lambda ()
             (end-of-line)
             (let* (( components (org-heading-components))
                    ( level (nth 0 components))
                    ( title (nth 4 components))
                    ( tags (string-trim (or (nth 5 components) "c_master") ":" ":"))
                    ( id (org-entry-get (point) "id"))
                    ( color (org-entry-get (point) "color"))
                    ( hex-color (when color (e/org-color-name-to-hex color)))
                    ( entry (e/org-collect-entry)))
               (setq alist (cons `( ,tags . ,(list `("title" . ,title)
                                                   `("id" . ,id)
                                                   `("color" . ,color)
                                                   `("hex-color" . ,hex-color)
                                                   `("entry" . ,entry)))
                                 alist))))
           nil 'tree)
          (setq entry-array (cons alist entry-array))))
      (setq global-array (cons `("entries" . ,(nreverse entry-array)) global-array))
      (with-temp-file (expand-file-name "/home/dan/.emacs.d/org/html/subtree.json")
        (insert (json-encode (nreverse global-array))))))
  (e/org-single-file-json-to-html)
  (e/org-collect-links-tree-live-server "/home/dan/.emacs.d/org/html/out3.html"))


(defun e/org-single-file-json-to-html ()
  (start-process "json-to-html" nil 
                 "/home/dan/.emacs.d/org/html/json_to_html.py"
                 "/home/dan/.emacs.d/org/html/template.html"
                 "/home/dan/.emacs.d/org/html/subtree.json"
                 "/home/dan/.emacs.d/org/html/out3.html"))


;;*** all parents of id


(defun e/org-collect-links-tree-all-parents ( id)
  (save-excursion
    (goto-char (point-min))
    (let ( parent-ids)
      (while (text-property-search-forward 'id id t)
        (setq parent-ids (cons (split-string (get-text-property (1- (point)) 'parent-ids))
                               parent-ids)))
      (setq parent-ids (sort parent-ids
                             (lambda ( a b) (< (length a) (length b)))))
      (e/org-roam-ids-to-files (delete-dups (apply #'append parent-ids))))))


;;*** color,headline associate with roam node


(defun e/org-prop-at-tag ( prop tag)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat ":" tag ":") nil t)
      (org-entry-get (point) prop))))


(defun e/org-set-color-property ()
  (interactive)
  (setq e/org-tmp-current-buffer (current-buffer))
  (list-colors-display
   nil nil
   (lambda ( color)
     (kill-buffer "*Colors*")
     (pop-to-buffer e/org-tmp-current-buffer)
     (org-set-property "color" color))))


(defun e/org-roam-set-color ( &optional scope)
  (interactive)
  (setq e/org-roam-links-highlight-set-color-buffer (current-buffer)
        e/org-set-color-scope scope
        e/org-tag (completing-read "Tag: " org-tag-alist))
  (list-colors-display
   nil nil
   (lambda ( color)
     (kill-buffer "*Colors*")
     (pop-to-buffer e/org-roam-links-highlight-set-color-buffer)
     (cond ((eq e/org-set-color-scope 'node)
            (e/org-collect-links-tree-set-color color e/org-tag))
           ((eq e/org-set-color-scope 'region)
             (e/org-collect-links-tree-color-region color e/org-tag))
           ((eq e/org-set-color-scope 'subtree)
            (e/org-collect-links-tree-subtree-color color))
           ((t
             (goto-char (point-min))
             (org-set-property "color" color)))))))


(defun e/org-collect-links-tree-copy-color ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\s*" nil t)
      (let (( color (or (cdr (assoc "COLOR"
                                    (cadr (e/org-collect-links-props
                                           (get-text-property (point) 'id)))))
                        (user-error "No Color at Node."))))
        (kill-new color)
        color))))


(defun e/org-collect-links-tree-yank-color ()
  (let (( color (current-kill 0)))
    (if (color-defined-p color)
        (e/org-collect-links-tree-set-color color)
      (user-error "No Color in Kill Ring."))))


(defun e/org-collect-links-tree-set-color ( color tag)
  (let* (( id (get-text-property (point) 'id))
         ( buffers (buffer-list))
         ( buffer (find-file-noselect
                   (caar (org-roam-db-query [:select [file]
                                                     :from nodes
                                                     :where (= id $s1)]
                                            id)))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat ":" tag ":") nil t)
          (org-set-property "color" color)
          (remove-hook 'after-save-hook 'e/org-roam-copy-id-after-save)
          (remove-hook 'after-save-hook 'e/org-roam-copy-id-after-save 'local)
          (save-buffer)
          )))
    (unless (member buffer buffers)
      (kill-buffer buffer))))


(defun e/org-roam-id-set-subtree ( id tag headline color)
  (let* (( buffers (buffer-list))
         ( buffer (find-file-noselect
                   (caar (org-roam-db-query [:select [file]
                                                     :from nodes
                                                     :where (= id $s1)]
                                            id)))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat ":" tag ":") nil t)
            (org-edit-headline headline)
          (goto-char (point-max))
          (insert "\n* " headline)
          (org-set-tags tag))
        (org-set-property "color" color))
      (save-buffer))
    (unless (member buffer buffers)
      (kill-buffer buffer))))


(defun e/org-collect-links-tree-subtree-color ( color)
  (beginning-of-line)
  (let ( indent)
    (while (re-search-forward (concat "^" indent "\s*")
                              (line-end-position 2) t)
      (setq indent (or indent
                       (concat (match-string-no-properties 0)
                               e/org-collect-links-tree-indent)))
      (e/org-collect-links-tree-set-color color))))


(defun e/org-collect-links-tree-color-region ( color tag)
  (let (( beg (if (region-active-p)
                  (region-beginning)
                (point)))
        ( end (if (region-active-p)
                  (region-end)
                (point))))
    (goto-char beg)
    (beginning-of-line)
    (setq end (save-excursion
                (goto-char end)
                (line-end-position)))
    (while (re-search-forward "^\s*" end t)
      (e/org-collect-links-tree-set-color color tag))))


(defun e/org-collect-links-tree-edit-region ()
  (interactive)
  (setq e/org-collect-links-tree-edit-region-beg
        (if (region-active-p)
            (save-excursion
              (goto-char (region-beginning))
              (line-beginning-position))
          (line-beginning-position))
        e/org-collect-links-tree-edit-region-end
        (if (region-active-p)
            (save-excursion
              (goto-char (region-end))
              (line-end-position))
          (line-end-position))
        e/org-collect-links-tree-edit-region-tag
        (completing-read "Tag: " org-tag-alist)
        e/org-collect-links-tree-edit-region-headline
        (read-string "Headline: "))
  (list-colors-display
   nil nil
   (lambda ( color)
     (let (( beg e/org-collect-links-tree-edit-region-beg)
           ( end e/org-collect-links-tree-edit-region-end)
           ( tag e/org-collect-links-tree-edit-region-tag)
           ( headline e/org-collect-links-tree-edit-region-headline))
     (kill-buffer "*Colors*")
     (pop-to-buffer "*Roam-Tree*")
     (goto-char beg)
     (while (re-search-forward "^\s*" end t)
       (e/org-roam-id-set-subtree (get-text-property (point) 'id)
                                  tag headline color)))
     (e/org-collect-links-tree-read-refresh 'no-tree))))


(defun e/org-collect-links-tree-set-subtree-color ()
  (interactive)
  (e/org-roam-set-color 'subtree))


(defun e/org-collect-links-tree-set-region-color ()
  (interactive)
  (e/org-roam-set-color 'region))


(defun e/org-collect-links-tree-set-node-color ()
  (interactive)
  (e/org-roam-set-color 'node))


;;*** property add

(setq e/org-property-collection
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


;;*** index node create


(defun e/org-roam-node-index ( &optional exclude-ids)
  (interactive)
  (unless (eq major-mode 'oroam-mode)
    (switch-to-buffer-other-window "*ORoam*"))
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
    (let (( i 0)
          ( chars '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
                    "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
                    "U" "V" "W" "X" "Y" "Z")) 
          index-list no-alias-list)
      (dolist (result results)
        (let (( title (nth 0 result))
              ( file (nth 1 result))
              ( id (nth 2 result))
              ( aliases (nth 3 result))
              ( aliases-help (e/org-roam-note-join (nth 3 result) "\n" "[0-9]+:"))
              ( refs (nth 4 result))
              ( properties (nth 5 result))
              ( olp (e/org-roam-note-join (nth 6 result) "->"))
              ( level (if (> (nth 7 result) 0) "H" "F")))
          (unless (or refs
                      (string= (cdr (assoc "TYPE" properties)) "outline"))
            (unless aliases
              (setq no-alias-list
                    (cons (propertize title
                                      'id id
                                      'mouse-face 'highlight
                                      'font-lock-face 'org-link)
                                      no-alias-list)))
            (dolist ( alias aliases)
              (setq alias (capitalize alias))
              (let (( index-entry (assoc alias index-list)))
                (if index-entry
                    (setq index-list (delete index-entry index-list)
                          index-entry `(,alias . ,(cons id (cdr index-entry))))
                  (setq index-entry `(,alias . ,(list id))))
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
      (insert (propertize "INDEX"
                          'font-lock-face '(:underline t
                                            :weight 'bold))
              "\n\n")
      (dolist ( index-entry index-list)
        (unless (get-text-property 0 'heading (car index-entry))
                  (insert "  "))
        (insert (car index-entry))
        (let (( i 0))
          (dolist ( id (cdr index-entry))
            (insert " " (propertize (concat "["
                                            (number-to-string (setq i (1+ i)))
                                            "]")
                                  'id id
                                  'font-lock-face 'org-link
                                  'mouse-face 'highlight)))
          (insert "\n")))
      (insert "\n\n")
      (insert (propertize "TITLES NOT IN INDEX"
                          'font-lock-face '(:underline t
                                            :weight 'bold))
              "\n\n")
      (dolist ( title no-alias-list)
        (insert title "\n")))
    (oroam-mode)
    (goto-char point)
    (recenter window-line)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))


;;*** links title update, mark links in other buffer


(defun e/org-roam-links-update-old ()
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (when (string-match-p "^id:" (match-string-no-properties 1))
          (let* (( id (replace-regexp-in-string "^id:" ""
                                                (match-string-no-properties 1)))
                 ( title (caar (org-roam-db-query [:select [title]
                                                           :from nodes
                                                           :where (= id $s1)]
                                                  id)))
                 ( node (org-roam-node-from-id id)))
            (if (not node)
                (replace-match (or (match-string-no-properties 2)
                                   (match-string-no-properties 1)))
              (let (( title (org-roam-node-title node)))
                (replace-match (org-link-make-string (concat "id:" id)
                                                     title))))))))))


(defun e/org-roam-links-update ( &optional backend)
  (when (or (not (buffer-file-name))
            (org-roam-file-p))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (when (string-match-p "^id:" (match-string 1))
          (let* (( id (replace-regexp-in-string
                       "^id:" "" (match-string-no-properties 1)))
                 ( title (caar (org-roam-db-query [:select [title]
                                                           :from nodes
                                                           :where (= id $s1)]
                                                  id))))
            (if (not title) ;; broken link
                (replace-match (or (match-string-no-properties 2)
                                   (match-string-no-properties 1)))
              (replace-match (org-link-make-string (concat "id:" id)
                                                   title)))))))))


(defun e/org-roam-aliases ()
  (when (org-in-regexp org-link-bracket-re 1)
    (let* (( id (replace-regexp-in-string
                 "^id:" "" (match-string-no-properties 1)))
           ( current (match-string-no-properties 2))
           ( query (car (org-roam-db-query [:select [title properties file]
                                                    :from nodes
                                                    :where (= id $s1)]
                                           id)))
           ( title (car query))
           ( aliases (save-match-data
                       (split-string-and-unquote
                        (or (cdr (assoc "ALIASES" (cadr query))) ""))))
           ( file (caddr query))
           ( description (save-match-data
                           (completing-read "Link Description: "
                                                    (cons title aliases)))))
      (replace-match description
                     nil nil nil 2)
      (setq aliases (remove title (add-to-list aliases description 'append)))
      (string-join (mapcar (lambda (alias)
                             (concat "\"" alias "\""))
                           aliases)
                   " "))))


(defun e/org-roam-aliases-add ( file aliases)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (org-set-property "ALIASES" aliases)))
  

(defun e/org-roam-auto-bibliography ( &optional backend)
  (when (or (not (buffer-file-name))
            (org-roam-file-p))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "[^[]\\[cite/?[^:]*:" nil t)
        (goto-char (point-max))
        (insert "\n* References\n"
                ":PROPERTIES:\n"
                ":HTML_CONTAINER_CLASS: link-section\n"
                ":END:\n"
                "#+print_bibliography:\n")))))


(defun e/org-roam-auto-links-section ( &optional backend)
  (when (or (not (buffer-file-name))
            (org-roam-file-p))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\s+Links" nil t)
        (unless (org-entry-get (point) "HTML_CONTAINER_CLASS")
          (org-set-property "HTML_CONTAINER_CLASS" "link-section"))))))


(defun e/org-roam-filename-update ()
  (when (org-roam-file-p)
    (let* (( pos (point))
           ( title (save-excursion
                     (goto-char (point-min))
                     (string-join
                      (split-string
                       (cadar (org-collect-keywords '("TITLE")))
                       "[ .,;:/!?%&$]") "_")))
           ( basename (car (split-string (file-name-base (buffer-file-name)) "-")))
           ( directory (file-name-directory (buffer-file-name)))
           ( new (concat directory
                         basename "-" (downcase title) ".org")))
      (if (file-exists-p new)
          (message "Roam file already exists!")
        (rename-file (buffer-file-name) new)
        (set-visited-file-name new)
        (org-roam-db-sync)))))
      

(remove-hook 'before-save-hook 'e/org-roam-links-update)
(remove-hook 'before-save-hook 'e/org-roam-filename-update)
;; (add-hook 'before-save-hook 'e/org-roam-links-update)
;; (add-hook 'before-save-hook 'e/org-roam-filename-update)


(defun e/org-roam-links-highlight-toggle ()
  (interactive)
  (let (( overlays (overlays-in (point-min) (point-max)))
        found)
    (while (setq overlay (pop overlays))
      (when (eq (overlay-get overlay 'type) 'links-highlight)
        (setq found t
              overlays nil)))
    (if found
        (remove-overlays (point-min) (point-max) 'type 'links-highlight)
      (e/org-roam-links-highlight))))


(defun e/org-roam-links-highlight-refresh ()
  (remove-hook 'post-command-hook 'e/org-roam-links-highlight-refresh)
  (e/org-roam-links-highlight))


(defun e/org-roam-links-highlight-old ()
  (remove-overlays (point-min) (point-max) 'type 'links-highlight)
  (let (( ids-re ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (setq ids-re (concat ids-re
                             (unless (string-empty-p ids-re) "\\|")
                             (replace-regexp-in-string
                              "^id:" ""
                              (match-string-no-properties 1))))))
    (dolist ( window (window-list))
      (when (and (not (eq (window-buffer window) (current-buffer)))
                 (eq major-mode 'org-mode)
                 (org-roam-file-p))
        (with-current-buffer (window-buffer window)
          (remove-overlays (point-min) (point-max) 'type 'links-highlight)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (concat "id:\\(" ids-re "\\)") nil t)
              (e/org-roam-links-highlight-line-at-point))))))))


(defun e/org-roam-links-highlight ()
  (remove-overlays (point-min) (point-max) 'type 'links-highlight)
  (let (( ids (mapcar (lambda ( link)
                        (string-match "id:\\([a-z0-9-]+\\)" link)
                        (match-string-no-properties 1 link))
                      (string-split (org-entry-get (point-min) "HLL") "\\]\s+\\[")))
        ( i 0))
    (dolist ( id ids)
      (let (( buffer (marker-buffer (org-roam-id-find id 'marker)))
            ( ids-re "")
            ( color "yellow1")
            title)
        (with-current-buffer buffer
          (save-excursion
            (setq color (or (org-entry-get (point-min) "HLL_COLOR") color)
                  title (cadar (org-collect-keywords '("title"))))
            (goto-char (point-min))
            (re-search-forward org-property-end-re nil t)
            (while (re-search-forward org-link-bracket-re nil t)
              (setq ids-re (concat ids-re
                                   (unless (string-empty-p ids-re) "\\|")
                                   (replace-regexp-in-string
                                    "^id:" ""
                                    (match-string-no-properties 1)))))))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (concat "id:\\(" ids-re "\\)") nil t)
            (let (( overlay (make-overlay (+ (line-beginning-position) 0)
                                          (+ (line-beginning-position) 1 6))))
              (overlay-put overlay 'type 'links-highlight)
              (overlay-put overlay 'help-echo
                           (concat title
                                   "\n\nLINK ID: " (match-string-no-properties 1)
                                   "\nPARENT ID: " id))
              (overlay-put overlay 'face `(:background ,color :extend t))))))
      (message "%s" i)
      (setq i (+ i 5)))))


(defun e/org-roam-links-highlight-persistent ()
  (let (( overlays (overlays-at (point)))
        found)
    (while (setq overlay (pop overlays))
      (when (eq (overlay-get overlay 'type) 'links-highlight)
        (setq found t
              overlays nil)))
    (when found
      (add-hook 'post-command-hook 'e/org-roam-links-highlight-refresh)))
  nil)


(add-hook 'org-metadown-hook 'e/org-roam-links-highlight-persistent)
(add-hook 'org-metaup-hook 'e/org-roam-links-highlight-persistent)


(define-minor-mode org-roam-links-highlight-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter " hll" :keymap nil
  (unless (eq major-mode 'org-mode)
    (user-error "Not in a ORG buffer."))
  (cond (org-roam-links-highlight-mode
         (e/org-roam-links-highlight)
         (add-hook 'org-metadown-hook 'e/org-roam-links-highlight-persistent)
         (add-hook 'org-metaup-hook 'e/org-roam-links-highlight-persistent))
        (t
         (remove-overlays (point-min) (point-max) 'type 'links-highlight)
         (remove-hook 'post-command-hook 'e/org-roam-links-highlight-refresh)
         (remove-hook 'org-metadown-hook 'e/org-roam-links-highlight-persistent)
         (remove-hook 'org-metaup-hook 'e/org-roam-links-highlight-persistent)
         )))


(defun e/org-roam-links-highlight-set-color ()
  (interactive)
  (setq e/org-roam-links-highlight-set-color-buffer (current-buffer))
  (list-colors-display
   nil nil
   (lambda ( color)
     (kill-buffer "*Colors*")
     (pop-to-buffer e/org-roam-links-highlight-set-color-buffer)
     (goto-char (point-min))
     (org-set-property "hll_color" color))))


;;*** screenshot


(defun e/org-image-screenshot-inline ()
  (interactive)
  (shell-command "gnome-screenshot --area")
  (let (( new (expand-file-name (concat (format-time-string "%Y%m%d%H%M%S")
                                        "-screenshot.png")
                                (concat 
                                 (file-name-as-directory org-directory)
                                 "images"))))
    (rename-file (car (last (directory-files "~" t "Bildschirmfoto.*\\.png")))
                 new)
    (insert (org-link-make-string (abbreviate-file-name new)) "\n")
    (org-display-inline-images)))


(defun e/org-image-screenshot-browser ()
  (interactive)
  (e/org-image-screenshot-inline)
  (e/org-link-from-browser))
  

;;*** clocktable with average

(defun org-dblock-write:e/clocktable ( params)
  (let* (( internal-time-tstart (and (plist-get params :tstart)
                                     (org-time-string-to-time (plist-get params :tstart))))
         ( internal-time-start (car (org-clock-special-range 'thisweek)))
         ( tstart (org-format-time-string (org-time-stamp-format t t)
                                          internal-time-start))
         ( internal-time-current (current-time))
         ( internal-time-end (encode-time
                              (append '( 0 0 0) (last (decode-time (current-time)) 6))))
         ( tend (org-format-time-string (org-time-stamp-format t t)
                                          internal-time-end))
         ( internal-time-diff (time-subtract internal-time-end
                                             internal-time-start))
         ( time-diff (min 5 (truncate (/ (float-time internal-time-diff)
                                         (* 60 60 24)))))
         ( days-diff (- (nth 6 (decode-time)) 1))
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
        (org-show-children)
      (when show-entry
        (org-show-entry))))
  (goto-char (point-min))
  (message "%s" show-entry))


(defun e/org-show-entries-at-level ( level)
  (interactive "p")
  (e/org-show-up-to-level level 'show-entry))
    

;;*** Match Sparse Tree with Mouse


(setq e/org-match-string "")


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


(setq e/org-sparse-tree-last-match-id nil
      e/org-sparse-tree-tag-inheritance-toggle nil)


(defun e/org-sparse-tree-match ( &optional only-match-string)
  (interactive)
  (let (( match (or (org-entry-get (point) "match")
                    (when (car (last (org-heading-components)))
                      (substring
                       (replace-regexp-in-string
                        ":" "+" (car (last (org-heading-components))))
                       nil -1))
                    (user-error "Match String Undefined.")))
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
          (org-show-children))))
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
    
    (setq-local reftex-cite-format "[[cite:%l][%a]] [cite/na:@%l]")
    (define-key org-mode-map "\C-ct" 'org-toggle-timestamp-type)
    (define-key org-mode-map (kbd "C-M->") 'org-dan-subtree-template)
    ;; (define-key org-mode-map "\C-ci" 'org-roam-file-header-hide-mode)
    ;; (define-key org-mode-map "\C-cm" 'e-org-choose-macro)
    (define-key org-mode-map "\C-c[" 'reftex-citation)
    (define-key org-mode-map (kbd "M-;") 'e/org-comment-dwim)
    (define-key org-mouse-map (kbd "<mouse-3>") 'e/org-open-at-point-other-window)
    (define-key org-mode-map (kbd "<S-mouse-2>") 'e/org-mouse-cycle-subtree)
    (define-key org-mode-map (kbd "<C-M-mouse-3>") 'e/org-mouse-cycle-global)
    (define-key org-mode-map (kbd "<C-S-mouse-1>") 'e/org-sparse-tree-match-at-mouse)
    (define-key org-mode-map (kbd "<C-S-mouse-2>") 'org-download-clipboard)
    (define-key org-mode-map (kbd "<C-S-mouse-3>") 'e/org-roam-directed-edge-show-entries-mouse)
    ;; (define-key org-mode-map (kbd "<C-M-S-mouse-3>") 'e/org-roam-directed-edge-show-children-mouse)
    (define-key org-mode-map (kbd "C-c x") 'e-org-cut-link)
    (define-key org-mode-map (kbd "C-c w") 'e-org-copy-link)
    (define-key org-mode-map (kbd "C-c C-n C-n") 'e/org-roam-note-list-plain)
    (define-key org-mode-map (kbd "C-c C-n C-e") 'e/org-roam-note-list-plain-exclude)
    ;; org-roam specific
    (define-key org-mode-map (kbd "C-S-l") 'org-roam-links-highlight-mode)
    (define-key org-mode-map (kbd "C-S-k") 'e/org-roam-link-remove)
    (define-key org-mode-map (kbd "C-S-n") 'e/org-roam-note-next)
    (define-key org-mode-map (kbd "C-S-p") 'e/org-roam-note-prev)
    (define-key org-mode-map (kbd "C-S-M-n") 'e/org-roam-note-next-ow)
    (define-key org-mode-map (kbd "C-S-M-p") 'e/org-roam-note-prev-ow)
    (define-key org-mode-map (kbd "M-a")
      'e/org-roam-link-section-buffers-update)
    (define-key org-mode-map (kbd "M-e") 'e/org-publish-current-project)
    (define-key org-mode-map (kbd "C-c l e") 'e/org-latex-environment)
    (define-key org-mode-map (kbd "C-c l l") 'insert-tex-label-random)
    (define-key org-mode-map (kbd "C-c l b") 'insert-tex-brackets)
    (define-key org-mode-map (kbd "C-c l r") 'e/org-latex-insert-ref)
    (define-key org-mode-map (kbd "C-c l c") 'insert-tex-command)
    (define-key org-mode-map (kbd "C-c l <up>") 'LaTeX-insert-superscript)
    (define-key org-mode-map (kbd "C-c l <down>") 'LaTeX-insert-subscript)
    (define-key org-mode-map (kbd "C-c c") 'e/org-roam-link-copy-id)
    (define-key org-mode-map (kbd "C-c y") 'e/org-roam-link-yank)
    (define-key org-mode-map (kbd "C-c r") 'e/org-remove-link)
    (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
    (define-key org-mode-map (kbd "C-c v") 'e/org-tags-visible-headings-add)
    (define-key org-mode-map (kbd "C-c m") 'e/org-roam-node-create-copy)
    (define-key org-mode-map (kbd "C-c e") 'e/org-export-buffer)
    (define-key org-mode-map (kbd "C-c p") 'e/org-set-property)
    (define-key org-mode-map (kbd "C-c h") 'e/org-roam-links-highlight-set-color)
    (define-key org-mode-map (kbd "C-c b b") 'e/org-link-from-browser)
    (define-key org-mode-map (kbd "C-c b t") 'e/org-link-from-browser-title)
    (define-key org-mode-map (kbd "C-c b i") 'e/org-link-from-bibtex)
    (define-key org-mode-map (kbd "C-c i i") 'e/org-image-screenshot-inline)
    (define-key org-mode-map (kbd "C-c i b") 'e/org-image-screenshot-browser)
    (define-key org-mode-map (kbd "C-c q") 'e/org-insert-block-quote)
    (define-key org-mode-map (kbd "C-c s") 'e/org-refile-source-set)
    (define-key org-mode-map (kbd "<C-dead-circumflex>") 'e/org-show-up-to-level)
    (define-key org-mode-map (kbd "<C-escape>") 'e/org-show-entries-at-level)
    (define-key org-mode-map (kbd "C-c C-a") 'org-agenda)
    (define-key org-mode-map (kbd "C-c d a") 'e/org-roam-directed-edge-add)
    (define-key org-mode-map (kbd "C-c d r") 'e/org-roam-directed-edge-remove)
    (define-key org-mode-map (kbd "C-c o")
      'e/org-roam-note-new-from-current-link)
    (define-key org-mode-map (kbd "C-c C-x C-o") 'e/org-clock-out)
    (define-key org-columns-map (kbd "M-<down>") #'e/org-columns-move-subtree-down)
    (define-key org-columns-map (kbd "M-<up>") #'e/org-columns-move-subtree-up)
    ;; left-fringe left-margin body right-margin left-fringe
    
    ;; (setq left-margin-width 10
    ;;       ;; right-margin-width 10
    ;;       fringes-outside-margins t
    ;;       left-fringe-width 40
    ;;       right-fringe-width 40)

    (visual-fill-column-mode 1)
    (setq visual-fill-column-center-text t)
    
    ;; (set-window-buffer nil (current-buffer))
    ;; (add-hook 'after-make-frame-functions 'e/org-set-face-attributes)
    ;; (e/org-set-face-attributes (selected-frame))
    (when (org-roam-file-p)
      ;; (add-hook 'after-save-hook 'e/org-roam-fontify-buffers nil 'local)
      ;; (add-hook 'after-save-hook 'e/org-roam-frame-thumbnail nil 'local)
      (add-hook 'after-save-hook #'e/org-roam-publish-after-save nil 'local)
      (add-hook 'after-save-hook #'e/org-roam-gatsby-sites nil 'local)
      (add-hook 'org-export-before-processing-functions #'e/org-roam-export-preamble nil 'local)
      (add-hook 'org-export-before-processing-functions #'e/org-roam-auto-bibliography nil 'local)
      (add-hook 'org-export-before-processing-functions #'e/org-roam-auto-links-section nil 'local)

      ;; (org-roam-hierarchy-buffer-mode 0) ;; second
      ;; (org-roam-file-header-hide-mode 1) ;; first
      (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
      (define-key org-mode-map (kbd "C-c n i") 'e/ivy-org-roam-node-insert)
      (define-key org-mode-map (kbd "C-c n h") 'e/org-roam-tree-note-new)
      (define-key org-mode-map (kbd "C-c n a") 'e/org-roam-note-alias-add)
      (define-key org-mode-map (kbd "<C-M-S-mouse-1>") 'e/org-roam-browser-open-current)
      (define-key org-mode-map (kbd "<C-M-S-mouse-5>") 'e/org-roam-browser-open-link-next)
      (define-key org-mode-map (kbd "<C-M-S-mouse-4>") 'e/org-roam-browser-open-link-previous)
      (define-key org-mode-map (kbd "<C-M-S-mouse-3>") 'e/org-roam-browser-open-link-at-mouse)
      (e/org-enable-minor-modes)
      (wcount-mode 1)
      )
    (setq org-download-heading-lvl nil
          org-download-image-dir "./images")
    ))


(add-hook 'org-agenda-mode-hook
          (lambda ()
            (setq org-agenda-files (append e/org-agenda-files-orig
                                           (e/org-publish-base-files)))
            (define-key org-agenda-mode-map (kbd "C-c C-a") 'org-agenda)
            (define-key org-agenda-mode-map (kbd "C-c C-x C-x") 'org-clock-in-last)
            (define-key org-agenda-mode-map "\C-co" 'org-agenda-open-file)
            (define-key org-agenda-mode-map (kbd "<C-M-mouse-3>")
              'org-agenda-mouse-open-link)))


(add-hook 'org-occur-hook
          (lambda ()
            (remove-overlays nil nil 'org-type 'org-occur)))


(add-hook 'org-clock-in-hook #'save-buffer)


(add-hook 'org-clock-out-hook #'save-buffer)


;;*** org-journal


(setq org-journal-dir "~/.emacs.d/journal/")


(add-hook 'org-journal-after-entry-create-hook
  (lambda ()
    (save-excursion
      (beginning-of-line 0)
      (unless (looking-at "^\s*$")
        (beginning-of-line 2)
        (insert "\n")))))


;;*** org-cite with reftex/bibtex


(org-link-set-parameters
 "cite"
 :follow 'e/org-cite-open-file
 :export 'e/org-cite-export
 :face 'org-link)


;; (org-link-set-parameters
;;  "cite"
;;  :follow 'e/org-cite-open-file
;;  :face (lambda (path)
;;          (if (e/org-cite-file-from-key path "/home/dan/library/archive/archive.db")
;;              'org-link
;;            'compilation-mode-line-fail)))


(defun e/org-cite-file-from-key ( bibtex-key db-file)
  (shell-command (concat "updatedb -l 0 -o " db-file " -U " (file-name-directory db-file)))
  (car (split-string
        (shell-command-to-string (concat "locate -d " db-file " " bibtex-key))
        "\n" 'omit-nulls)))


(defun e/org-cite-update-db ()
  (interactive)
  (concat "updatedb -l 0 -o " e/org-cite-library-database 
          " -U " (file-name-directory e/org-cite-library-database )))


(setq e/org-cite-library-database "/home/dan/library/archive/archive.db")


(defun e/org-cite-open-file ( path arg)
  (let (( file-path (e/org-cite-file-from-key
                     path
                     e/org-cite-library-database)))
    (if file-path
        (org-link-open-as-file file-path arg)
      (let* (( values (e/org-cite-key-get-fields path "doi" "url" "title"))
             ( doi (nth 0 values))
             ( url (nth 1 values))
             ( title (nth 2 values)))
        (cond (doi (org-link--open-doi doi arg))
              (url (browse-url url arg))
              (title (browse-url (concat "https://google.com/search?q=" title) arg))
              (t (user-error "No target to follow.")))))))


(defun e/org-cite-export ( path description back-end export-channel)
  (pcase back-end
    ('html (concat "<a href=\"emacs:cite:" path "\">"
                   description
                   "</a>"))
    (_ path)))


(defun e/org-cite-key-get-fields ( key &rest fields)
  (save-match-data
    (with-temp-buffer
      (insert-file-contents (car reftex-default-bibliography))
      (when (bibtex-search-entry key)
        (let (( values nil))
          (dolist ( field fields)
            (setq values (cons (ebibtex-entry-get-field field)
                               values)))
          (nreverse values))))))


;;*** export to markdown


(org-link-set-parameters "rel")


(defun e/org-publish-after-message ( in-file out-file)
  (when-let (( dir (org-entry-get (point-min) "EXPORT_DIR")))
    (rename-file out-file
                 (concat (file-name-as-directory
                          (expand-file-name dir))
                         (file-name-nondirectory out-file))
                 'OK-IF-ALREADY-EXISTS)))


(add-hook 'org-publish-after-publishing-hook
          'e/org-publish-after-message)


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


(defun e/org-roam-id-point-open ( path arg)
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
;; (delete '(keyword "transclude") org-roam-db-extra-links-exclude-keys)




(provide 'org-custom)

;;; org-custom.el ends here
