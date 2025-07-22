;;; bibtex-custom --- BibTeX customization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'bibtex-custom)

;;; Code:

(require 'bibtex)
(require 'latex-custom)
(require 'misc-custom)

(setq bibtex-comma-after-last-field t)

(defvar ebibtex-default-bibliography '("/home/dan/library/database/reference.bib"))

(defvar ebibtex-extra-fields
  '("abstract" "keywords" "chapter" "url" "isbn" "isbn_ebook"
    "isbn_soft" "isbn_hard" "issn" "issn_print" "issn_online"
    "issn_cdrom" "eprint" "doi" "year" "publisher" "fullname"
    "oldbibtexkey")
  "Extra fields to add to `bibtex-BibTeX-entry-alist' for the
function `ebibtex-insert-field'.")

(defun ebibtex-entry-get-field ( tag)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (( bounds (bibtex-search-forward-field tag t)))
      (when bounds
        (let* (( beg (bibtex-start-of-text-in-field bounds))
               ( end (bibtex-end-of-text-in-field bounds))
               ( text (buffer-substring-no-properties beg end)))
          (when (or (string-prefix-p "{" text)
                    (string-prefix-p "\"" text))
                (setq text (substring text 1 nil)))
          (when (or (string-suffix-p "}" text)
                    (string-suffix-p "\"" text))
                (setq text (substring text 0 -1)))
          (text-clear-layout text))))))

(defun ebibtex-key-get-fields ( key &rest fields)
  (save-match-data
    (with-temp-buffer
      (insert-file-contents (car ebibtex-default-bibliography))
      (when (bibtex-search-entry key)
        (let (( values nil))
          (dolist ( field fields)
            (setq values (cons (ebibtex-entry-get-field field)
                               values)))
          (nreverse values))))))

(defun ebibtex-field-end-comma-entry ()
  (bibtex-end-of-entry)
  (re-search-forward "\\([^ \n{}]\\)" nil t -1)
  (unless (string= (match-string 1) ",")
    (end-of-line)
    (insert ",")))

(defun ebibtex-field-end-comma ()
  (interactive)
  (goto-char (point-min))
  (let (( lastPos (- (point-max) 6))
        ( pos (point-min))
        ( count 0))
       (while (< (point) lastPos)
              (bibtex-next-field nil t)
              (unless (looking-at ",\\|.,")
                      (setq pos (point))
                      (re-search-forward "[^ \n]*")
                      (insert ",")
                      (setq count (1+ count))
                      (goto-char pos)))
       (message "Added %s missing end-of-field commas" count)))

(defun ebibtex-author-fullname ()
  (save-excursion
    (bibtex-beginning-of-entry)
    (unless (bibtex-search-forward-field "fullname" t)
      (let (( bounds (bibtex-search-forward-field "author" t)))
        (when bounds
          (let* (( beg (1+ (bibtex-start-of-text-in-field bounds)))
                 ( end (1- (bibtex-end-of-text-in-field bounds)))
                 ( fullField (buffer-substring-no-properties beg end)))
            (goto-char beg)
            (bibtex-make-field "fullname" t)
            (goto-char (1- (point)))
            (insert fullField)))))))

(defun ebibtex-provide-fullname ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@.*{\\(.*\\),.*$" (point-max) t)
           (ebibtex-author-fullname))))

(defun ebibtex-duplicate-entries ()
  (save-excursion
    (goto-char (point-min))
    (let (( title "")
          ( titles nil)
          ( duplicates nil))
      (while (re-search-forward "^@\\([a-zA-z]*\\){\\(.*\\),.*$" (point-max) t)
             (setq title (concat (match-string-no-properties 1) " "
                                 (ebibtex-entry-get-field "title"))
                   title (replace-regexp-in-string "-" " " title)
                   title (replace-regexp-in-string "\s\s+" " " title)
                   title (downcase title))
             (setq titles (cons title titles)))
      (while (setq title (pop titles))
             (when (member title titles)
                   (setq duplicates (cons title duplicates))))
      (message "%S" duplicates)
      duplicates)))

(defun ebibtex-show-duplicate-entry ()
  (interactive)
  (let (( dup (car (ebibtex-duplicate-entries))))
    (if dup
      (let (( title "")
            ( i 0)
            ( pos1 (point-min))
            ( pos2 (point-min)))
        (goto-char pos1)
        (while (and (< i 2)
                    (re-search-forward "^@\\([a-zA-z]*\\){\\(.*\\),.*$"
                                       (point-max) t))
          (setq title (concat (match-string-no-properties 1) " "
                                 (ebibtex-entry-get-field "title"))
                title (replace-regexp-in-string "-" " " title)
                title (replace-regexp-in-string "\s\s+" " " title)
                title (downcase title))
          (when (string= title dup)
            (setq i (1+ i))
            (if (= i 1)
              (setq pos1 (line-beginning-position))
              (setq pos2 (line-beginning-position)))))
        (goto-char pos1)
        (recenter 1)
        (switch-to-buffer-other-window (buffer-name))
        (goto-char pos2)
        (recenter 1))
      (message "No duplicate entries found."))))

(defun ebibtex-format-keys ()
  (interactive)
  (let (( randomStr "aaaa")
        ( start 0)
        ( year "0000")
        ( oldBibtexKey "")
        ( countAll 0) ( countMod 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@.*{\\(.*\\),.*$" (point-max) t)
        (setq countAll (1+ countAll))
        (beginning-of-line)
        (unless (re-search-forward "@.*{[0-9]\\{4\\}[a-z]\\{4\\},"
                                   (line-end-position) t)
                (setq countMod (1+ countMod))
                (setq randomStr (random-string 4 t))
                (save-excursion
                  (setq start (point))
                  (bibtex-end-of-entry)
                  (if (re-search-forward "^.*year.*=.*\\([0-9]\\{4\\}\\)"
                                         start t -1)
                      (setq year (match-string 1))
                      (setq year "0000")))
                (re-search-forward "{\\(.*\\)," (line-end-position) t)
                (setq oldBibtexKey (match-string 1))
                (replace-match (concat year randomStr) t nil nil 1)
                (bibtex-beginning-of-entry)
                (end-of-line)
                (insert (concat "\noldbibtexkey = \"" oldBibtexKey "\",")))))
    (ebibtex-provide-fullname)
    (bibtex-reformat)
    (ebibtex-field-end-comma)
    (message "Changed BibTeX keys: %s/%s" countMod countAll)))

(defun ebibtex-fix-key ()
  (bibtex-beginning-of-entry)
  (unless (looking-at "@.*{[0-9]\\{4\\}[a-z]\\{4\\},")
    (let (( beg (point))
          ( year "0000")
          ( oldBibtexKey "")
          ( randomStr (random-string 4 t)))
      (bibtex-end-of-entry)
      (when (re-search-forward "^.*year.*=.*\\([0-9]\\{4\\}\\)"
                                  beg t -1)
        (setq year (match-string 1)))
      (bibtex-beginning-of-entry)
      (re-search-forward "{\\(.*\\)," (line-end-position) t)
      (setq oldBibtexKey (match-string 1))
      (replace-match (concat year randomStr) t nil nil 1)
      (end-of-line)
      (insert (concat "\noldbibtexkey = \"" oldBibtexKey "\",")))))

(defun ebibtex-reformat-key ()
  (bibtex-beginning-of-entry)
  (set-mark-command nil)
  (bibtex-end-of-entry)
  (setq deactivate-mark nil)
  (bibtex-reformat))

(defun e/bibtex-entry-beginning-position ()
  (save-excursion
    (bibtex-beginning-of-entry)))

(defun e/bibtex-entry-end-position ()
  (save-excursion
    (bibtex-end-of-entry)))

(defun e/bibtex-replace-comment-symbol ()
  (replace-regexp-in-region "\\([^\\]\\)%" "\\1\\\\%"
                            (e/bibtex-entry-beginning-position)
                            (e/bibtex-entry-end-position)))

(defun ebibtex-format-key ()
  "Assume point somewhere inside the entry (at or after the
at-symbol)."
  (interactive)
  (unless (find-nonascii-char)
    (ebibtex-fix-key)
    (e/bibtex-replace-comment-symbol)
    (bibtex-clean-entry)
    (ebibtex-author-fullname)
    (bibtex-fill-entry)
    (ebibtex-field-end-comma-entry)))

(defvar ebibtex-library-root "/home/dan/library/archive/")

(defvar ebibtex-entry-beginning-re "^\s*@\s*\\([a-zA-Z]+\\)\s*{\s*\\(.+?\\)\s*,"
  "Regexp for finding next entry beginning with entry type (book,
  article, etc.) and entry key.")

(defun ebibtex-clean-value ( value)
  "Remove control characters from bibtex field value.

The string VALUE is an unfiltered bibtex field text, i.e. text
with delimiters (\"{\") etc. and newlines.  Returns a clean
string with control characters removed."
  (setq value (replace-regexp-in-string "\n" " " value)
        value (replace-regexp-in-string "\s\s+" " " value)
        value (replace-regexp-in-string "^\s*{+\\|^\s*\"" "" value)
        value (replace-regexp-in-string "}+\s*$\\|\"\s*$" "" value)))

(defun ebibtex-export-entry ()
  "Export current bibtex entry to text file.

A temporary file is created or overwritten with the content of
the current bibtex entry, i.e. the entry where point is located.
Each line of the file contains a pair, field-key field-value,
separted by a space."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (( entry (bibtex-parse-entry))
          ( key ""))
      (with-temp-file "/tmp/bibtex-entry.txt"
        (dolist ( key-value entry)
          (when (string= (car key-value) "=key=")
            (setq key (cdr key-value)))
          (insert (format "%s %s\n"
                          (car key-value)
                          (ebibtex-clean-value (cdr key-value))))))
      key)))

(defun ebibtex-clean-tags ( tags)
  "Clean up string suitable for composing file names."
  (when tags
    (let (( case-fold-search nil)
          ( old-tags (concat tags "dfdfd")))
      (while (not (string= old-tags tags))
        (setq old-tags tags
              tags (ebibtex-clean-value tags)
              tags (replace-regexp-in-string "[A-Z][A-Z]?\\." "" tags)
              tags (replace-regexp-in-string "\s*[,:.;!\"]\s*" " " tags)
              tags (replace-regexp-in-string "['{}\\]" "" tags)
              tags (replace-regexp-in-string "\\(\s+\\|^\s*\\)\\(and\\|a\\|at\\|by\\|its\\|on\\|from\\|to\\|with\\|in\\|of\\|for\\|the\\|und\\|der\\|die\\|das\\|bei\\|zur\\|uber\\)\s+" " " tags))))
    tags))

(defun ebibtex-sort-tags ( tags)
  "Sort words in a string.

A string of TAGS is sorted alphabetically.  Returns the modified
string."
  (when tags
    (string-join (sort (split-string tags) 'string-lessp) " ")))

(defun ebibtex-entry-file-name ()
  "Compose a file name according to current bibtex entry.

With point in a bibtex entry, returns a file name composed of
key[year authors title].pdf.  The file name is compatible with
jabref and tagspaces."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* (( key-value (bibtex-parse-entry))
           ( key (cdr (assoc "=key=" key-value)))
           ( year (cdr (assoc "year" key-value)))
           ( author (cdr (assoc "author" key-value)))
           ( title (cdr (assoc "title" key-value))))
      `( ,key . ,(concat key
                         "["
                         (when year
                           (format "%s "
                                   (ebibtex-clean-value year)))
                         (when author
                           (format "%s "
                                   (ebibtex-sort-tags
                                    (ebibtex-clean-tags author))))
                         (when title
                           (ebibtex-sort-tags
                            (ebibtex-clean-tags
                             (downcase title))))
                         "].pdf")))))

(defun ebibtex-locate ( pattern)
  "Provide list of files with names matching pattern.

PATTERN may be the bibtex key when using the naming convention
according to jabref, which expects the bibtex key to be the first
part of the filename. Return relative paths to
`ebibtex-library-root'."
  (shell-command (concat "updatedb -l 0 "
                         "-o /home/dan/.locate/library.db "
                         "-U " ebibtex-library-root))
  (let (( paths (split-string (shell-command-to-string
                               (concat "locate -d /home/dan/.locate/library.db "
                                       pattern))
                              "\n" t))
        files)
    (dolist ( path paths)
      (when (file-regular-p path)
        (setq files (cons (replace-regexp-in-string ebibtex-library-root "" path) files))))
    files))

(defun ebibtex-set-exif ( targetpath)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (( bibtexpath (expand-file-name "~/ebibtex.txt")))
      (write-region (bibtex-beginning-of-entry) (bibtex-end-of-entry) bibtexpath)
      (async-shell-command (concat "bibtex2exif "
                                   bibtexpath " "
                                   (shell-quote-argument targetpath))))))

(defun ebibtex-check-xmpmeta ( filename)
  (let (( output (shell-command-to-string
                  (concat "pdfinfo -meta "
                          (shell-quote-argument filename)))))
    (and (string-match "<x:xmpmeta" output)
         (string-match "</x:xmpmeta>" output))))

(defun ebibtex-gs ( filename)
  (let* (( trash-dir (expand-file-name
                      (file-name-as-directory "~/.ebibtex-trash")))
         ( trash-file (concat trash-dir
                              (file-name-nondirectory filename))))
    (make-directory trash-dir 'PARENTS)
    (rename-file filename trash-file 'OK-IF-ALREADY-EXISTS)
    (shell-command (concat "gs -o " (shell-quote-argument filename)
                           " -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress "
                           (shell-quote-argument trash-file)
                           "> /dev/null"))))

(defun ebibtex-gs-ask ( filename)
  (when (y-or-n-p (concat (unless (ebibtex-check-xmpmeta filename)
                            "WARNING: No <x:xmpmeta *>-tag found.\n")
                          "Rewrite " filename " by ghostcript?"))
    (ebibtex-gs filename)))

(defun ebibtex-rename-file ()
  (interactive)
  (let* (( key-filename (ebibtex-entry-file-name))
         ( files (ebibtex-locate (car key-filename)))
         ( note (when files
                  (concat "Existing files:\n"
                          (string-join files "\n")
                          "\n")))
         ( orig (read-file-name (concat note
                                        "Current file name? ")
                                ebibtex-library-root
                                nil t
                                (when (= (length files) 1)
                                  (car files))))
         ( orig-absolute (expand-file-name orig))
         ( new orig)
         ( new-absolute orig-absolute))
    (when (y-or-n-p (concat "Rename " orig "?"))
      (setq new (read-file-name (concat "Replace " orig
                                           "\nwith    ")
                                   (file-name-directory orig)
                                   nil nil
                                   (cdr key-filename))
            new-absolute (expand-file-name new))
      (rename-file orig-absolute new-absolute 'OK-IF-ALREADY-EXISTS))
    (when (y-or-n-p (concat (unless (ebibtex-check-xmpmeta new-absolute)
                              "WARNING: No <x:xmpmeta *>-tag found.\n")
                            "Rewrite " new " by ghostcript?"))
      (ebibtex-gs new-absolute))
    (when (y-or-n-p (concat "Set exif data for " new "?"))
      (ebibtex-set-exif new-absolute))))


(defun ebibtex-next-missing-file ()
  (interactive)
  (let (( valid-file 'true)
        ( info "No missing file."))
    (while (and valid-file
                (re-search-forward ebibtex-entry-beginning-re (point-max) t))
      (let* (( files (ebibtex-locate (match-string 2)))
             ( filecount (length files)))
        (if (= filecount 1)
            (let (( filename (concat ebibtex-library-root
                                     (car files))))
              (unless (ebibtex-check-xmpmeta filename)
                (setq valid-file nil
                      info "Tag xmpmeta missing.")))
          (setq valid-file nil)
          (if (> filecount 1)
              (setq info "Duplicate files found.")
            (setq info "No files found.")))))
    (message "%s" info)))

(defun ebibtex-gather-field-names ()
  (let ( all-fields)
    (dolist ( type bibtex-BibTeX-entry-alist)
      (dolist ( fields type)
        (when (listp fields)
          (dolist ( field fields)
            (setq all-fields (cons (car field) all-fields))))))
    (reverse (delete-dups all-fields))))


(defun ebibtex-insert-field ()
  "Insert new field in current BibTeX entry.

This is a variant of function `bibtex-make-field' providing all
possible fields regardless of the entry type. Custom field names
can be added with the variable `ebibtex-extra-fields'."
  (interactive)
  (let (( fieldTag nil)
        ( valueColumn 17)
        ( all-fields (delete-dups
                      (append (ebibtex-gather-field-names)
                              ebibtex-extra-fields))))
    (setq fieldTag (completing-read "Field tag name: "
                                    all-fields
                                    nil nil ""))
    (beginning-of-line)
    (unless (looking-at "^\s*$")
            (bibtex-end-of-entry)
            (beginning-of-line)
            (insert "\n")
            (beginning-of-line 0))
    (insert (format "  %s = " fieldTag))
    (when (< (current-column) valueColumn)
          (move-to-column valueColumn t))
    (insert "{},")
    (goto-char (- (point) 2))))

(defun ebibtex-entry-from-isbn ()
  (interactive)
  (let* (( isbn (read-string "ISBN: "))
         ( url (format "https://openlibrary.org/isbn/%s.json" isbn))
	     ( json (with-temp-buffer
                 (url-insert-file-contents url)
                 (json-parse-buffer :object-type 'alist
                                    :array-type 'list)))
         ( title (cdr (assoc 'title json)))
         ( date (parse-time-string (cdr (assoc 'publish_date json))))
         ;; ( day (when (nth 3 date) (number-to-string (nth 3 date))))
         ( month (when (nth 4 date) (number-to-string (nth 4 date))))
         ( year (when (nth 5 date) (number-to-string (nth 5 date))))
         ( publisher (string-join (cdr (assoc 'publishers json)) ", "))
         ( authors (string-join
                    (mapcar
                     (lambda ( author-url)
                       (setq author-url (cdr (assoc 'key author-url))
                             author-url (format "https://openlibrary.org%s.json"
                                                author-url))
                       (let (( author (with-temp-buffer
                                        (url-insert-file-contents author-url)
                                        (json-parse-buffer :object-type 'alist
                                                           :array-type 'list))))
                         (cdr (assoc 'personal_name author))))
                     (cdr (assoc 'authors json)))
                    " and "))
         ( isbn-10 (car (cdr (assoc 'isbn_10 json))))
         ( isbn-13 (car (cdr (assoc 'isbn_13 json))))
         ( lccn (car (cdr (assoc 'lccn json))))
         ( physical-format (cdr (assoc 'physical_format json)))
         ( pages (when (cdr (assoc 'number_of_pages json))
                   (number-to-string (cdr (assoc 'number_of_pages json)))))
         ( revision (when (cdr (assoc 'latest_revision json))
                      (number-to-string (cdr (assoc 'latest_revision json)))))
         ( entry (concat "@book{" isbn ",\n"
                         "title = {" title "},\n"
                         "author = {" authors "},\n"
                         "publisher = {" publisher "},\n"
                         (when pages (concat "pages = {" pages "},\n"))
                         (when month (concat "month = {" month "},\n"))
                         "year = {" year "},\n"
                         "isbn = {" (or isbn-10 isbn-13) "},\n"
                         "isbn10 = {" isbn-10 "},\n"
                         "isbn13 = {" isbn-13 "},\n"
                         "lccn = {" lccn "},\n"
                         "format = {" physical-format "},\n"
                         (when revision (concat "revision = {" revision "},\n"))
                         "}\n")))
    (insert entry)))

(defun ebibtex-entry-from-doi ()
  (interactive)
  (let* (( url-mime-accept-string "text/bibliography;style=bibtex")
         ( doi (read-string "DOI: "))
         ( url (format "http://dx.doi.org/%s" doi))
	     ( entry (with-temp-buffer
                   (url-insert-file-contents url)
                   (buffer-substring (point-min) (point-max)))))
    (insert entry)
    (bibtex-fill-entry)))

(add-hook 'bibtex-mode-hook
  (lambda ()
    (define-key bibtex-mode-map "\C-ce" 'ebibtex-format-key)
    (define-key bibtex-mode-map "\C-ck" 'ebibtex-format-keys)
    (define-key bibtex-mode-map "\C-ci" 'ebibtex-insert-field)))

(provide 'bibtex-custom)

;;; bibtex-custom.el ends here
