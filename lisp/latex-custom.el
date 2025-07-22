;;; latex-custom --- LaTeX and TeX customization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'latex-custom)

;;; Code:

(require 'org)
(require 'font-latex)
(require 'misc-custom)

(defface LaTeX-layer-begin-bg
  '((((type x w32 mac))
     (:background "#E2E1D5" :underline "#A7A6AA" :height 90))
    (((type tty))
     (:foreground "blue")))
    "Face used to display Doxygen comments (light gray)."
    :group 'basic-faces)
(defface LaTeX-layer-end-bg
  '((((type x w32 mac))
     (:background "#E2E1D5" :overline "#A7A6AA" :height 90))
    (((type tty))
     (:foreground "blue")))
    "Face used to display Doxygen comments (light gray)."
    :group 'basic-faces)


(defface latex-block-yellow
  '((((type x w32 mac) (class color) (min-colors 88) (background light))
     :background "#ffffa0")
    (((type tty))
     (:foreground "blue")))
    "Face used to display inline math inside multiline blocks."
    :group 'basic-faces)
(defface latex-block-green
  '((((type x w32 mac) (class color) (min-colors 88) (background light))
     :background "#e0ffe0")
    (((type tty))
     (:foreground "blue")))
    "Face used to display inline math inside multiline blocks."
    :group 'basic-faces)
(defface latex-block-red
  '((((type x w32 mac) (class color) (min-colors 88) (background light))
     :background "#fff3f3")
    (((type tty))
     (:foreground "blue")))
    "Face used to display inline math inside multiline blocks."
    :group 'basic-faces)
(defface latex-block-blue
  '((((type x w32 mac) (class color) (min-colors 88) (background light))
     :background "#f2f2ff")
    (((type tty))
     (:foreground "blue")))
    "Face used to display inline math inside multiline blocks."
    :group 'basic-faces)
(defface latex-block-purple
  '((((type x w32 mac) (class color) (min-colors 88) (background light))
     :background "#ffebff")
    (((type tty))
     (:foreground "blue")))
    "Face used to display inline math inside multiline blocks."
    :group 'basic-faces)


(defvar LaTeX-map-block-colors '(("yellow" latex-block-yellow)
                               ("green" latex-block-green)
                               ("red" latex-block-red)
                               ("blue" latex-block-blue)
                               ("purple" latex-block-purple)))


;;*** Define variable for layer handling

(defvar-local LaTeX-environment-layers nil
  "This variable holds a list of currently defined layers and
their associated colors. In normal usage, this variable is
automatically set by parsing new layer commands in the preamble
of the source file.

Example:
\\newlayer{x}{layername1}{layercolor1})
\\newlayer{x}{layername2}{layercolor2})
...

The value of `LaTeX-environment-layers is then set to
((\"layername1\" \"layercolor1\") (\"layername2\" \"layercolor2\") ...)
")


;;*** Function for inserting commands


(defun insert-tex-label-random ( cread)
  (interactive (list
    (completing-read "Insert label type (default equation): "
        '("equation" "table" "figure" "section" "chapter"
          "layer" "list" "equation (no command)" "table (no command)"
          "figure (no command)" "section (no command)"
          "chapter (no command)" "layer (no command)")
        nil t "")))
  (unless (string-suffix-p "(no command)" cread)
    (insert "\\label{"))
  (let* (( prefix (cond ((string-prefix-p "equation" cread) "eq:")
                        ((string-prefix-p "table" cread) "tab:")
                        ((string-prefix-p "figure" cread) "fig:")
                        ((string-prefix-p "section" cread) "sec:")
                        ((string-prefix-p "chapter" cread) "chp:")
                        ((string-prefix-p "layer" cread) "lay:")
                        ((string-prefix-p "list" cread) "lst:")
                        ( t "eq:")))
         ( id (concat prefix (random-string))))
    (insert id)
    (kill-new id))
  (unless (string-suffix-p "(no command)" cread)
          (insert "}")
          (newline)))

(defun insert-tex-sparse-tree ()
  "Create section label, environment and hiding switch."
  (interactive)
  (let (( label (concat "sec:" (random-string)))
        ( beg (point))
        ( end (point)))
    (when (use-region-p)
      (setq beg (region-beginning)
            end (region-end)))
    (goto-char end)
    (insert "\\end{" label "}")
    (goto-char beg)
    (insert "\\label{" label "}\n\\begin{" label "}\n")
    (when (re-search-backward "% end section visibility switches" nil t)
      (beginning-of-line 1)
      (insert "\\includecomment{" label "}\n"
              "%\\excludecomment{" label "}\n"))))

(defun latex-section-occur ()
  (re-search-forward "\\\\hyperref\\[\\([A-Za-z0-9]+\\):\\([^]]*\\)\\]{\\([^}]*\\)}" nil t)
  (save-excursion
    (re-search-backward "\\\\begin{\\(sec:[^}]+\\)}\\|\\\\section" nil t)
    (when (match-string 1)
      (re-search-backward (concat "\\(%+\\) *\\\\excludecomment{"
                                  (match-string 1) "}")
                          nil t)
      (replace-match "" t t nil 1))))


(defun insert-tex-ref ()
  (interactive)
  (insert "(\\ref{})")
  (goto-char (- (point) 2)))

(defun region-tex-insert ()
  (let (( pos1 (point))
        ( pos2 (point)))
       (if (use-region-p)
           (setq pos1 (region-beginning)
                 pos2 (region-end))
         (when (re-search-forward "[\s\n]\\([^\s\n]*\\)" (point-min) t -1)
           (setq pos1 (match-beginning 1)
                 pos2 (match-end 1))
           (forward-char)))
       (unless (= pos1 pos2)
         (delete-and-extract-region pos1 pos2))))


(defun insert-tex-text ()
  (interactive)
  (insert (concat (completing-read "Text type: "
                    '(("\\textrm" 1) ("\\textbf" 2) ("\\textit" 3))
                    nil nil "\\text")
                  "{" (region-tex-insert) "}"))
  (goto-char (1- (point))))


(defun insert-tex-texteq()
  (interactive)
  (insert (concat "$ " (region-tex-insert) "$"))
  (forward-char -1))


(defun insert-tex-brackets ()
  (interactive)
  (let (( region (region-tex-insert)))
    (insert (concat "\\left( " region " \\right)"))
    (unless region
      (goto-char (- (point) 8)))))


(defun insert-tex-norm ()
  (interactive)
  (let (( region (region-tex-insert)))
    (insert (concat "\\left\\lVert " region " \\right\\rVert"))
    (unless region
      (goto-char (- (point) 13)))))


(defun LaTeX-cycle-bracket-size ()
  (interactive)
  (let (( left (list "left" "big" "Big" "bigg" "Bigg"))
        ( right (list "right" "big" "Big" "bigg" "Bigg"))
        ( type (string (char-before)))
        new)
    (re-search-forward "\\\\\\([a-zA-Z]+\\)"
                       (line-beginning-position) t -1)
    (if (setq new (cadr (member (match-string-no-properties 1) right)))
        (replace-match new t nil nil 1)
        (replace-match (car right) t nil nil 1))
    (re-search-forward type (line-end-position) t)
    (if (string= type "}")
        (LaTeX-curly-backward)
        (backward-sexp))
    (re-search-forward "\\\\\\([a-zA-Z]+\\)"
                       (line-beginning-position) t -1)
    (if (setq new (cadr (member (match-string-no-properties 1) left)))
        (replace-match new t nil nil 1)
        (replace-match (car left) t nil nil 1))
    (if (string= type "}")
        (LaTeX-curly-forward)
        (forward-sexp))))


(defun LaTeX-curly-backward ()
  (re-search-forward "\\\\[a-zA-Z]*\\\\}"
                     (line-beginning-position) t -1)
  (let (( nested 0))
    (while (>= nested 0)
      (re-search-forward "\\\\[a-zA-Z]*\\\\\\({\\|}\\)"
                         (point-min) t -1)
      (if (string= (match-string-no-properties 1) "}")
          (setq nested (1+ nested))
          (setq nested (1- nested)))))
  (re-search-forward "{" (line-end-position) t)
  (backward-char))


(defun LaTeX-curly-forward ()
  (let (( nested 0))
    (while (>= nested 0)
      (re-search-forward "\\\\[a-zA-Z]*\\\\\\({\\|}\\)"
                                 (point-max) t)
      (if (string= (match-string-no-properties 1) "{")
          (setq nested (1+ nested))
          (setq nested (1- nested))))))


(defun insert-tex-curlies ()
  (interactive)
  (let ( extracted)
    (insert (concat "{" (setq extracted (region-tex-insert)) "}"))
    (when (string= extracted "")
      (goto-char (- (point) 1)))))


(defun insert-tex-command ()
  (interactive)
  (let ( extracted macro)
    (setq macro (concat (completing-read "Command: "
'("\\boldsymbol" "\\dot" "\\ddot" "\\textrm" "\\textbf" "\\textit"
"\\mathrm" "\\mathbf" "\\mathit" "\\mathds" "\\mathcal" "\\ref" "\\cite" "\\chapter"
"\\section" "\\subsection" "\\subsubsection" "\\hsection"
"\\hsubsection" "\\hsubsubsection" "\\begin" "\\end" "\\mathcal"
"\\documentclass" "\\usepackage" "\\input" "\\include"
"\\includecomment" "\\excludecomment" "\\bar" "\\tilde" "\\hat"
"\\eqref" "\\secref" "\\figref")
                    nil nil "\\")
                  "{" (setq extracted (region-tex-insert)) "}")
          macro (replace-regexp-in-string  "\\\\mr{" "\\\\mathrm{" macro)
          macro (replace-regexp-in-string  "\\\\mb{" "\\\\mathbf{" macro)
          macro (replace-regexp-in-string  "\\\\mi{" "\\\\mathit{" macro)
          macro (replace-regexp-in-string  "\\\\mc{" "\\\\mathcal{" macro)
          macro (replace-regexp-in-string  "\\\\md{" "\\\\mathds{" macro)
          macro (replace-regexp-in-string  "\\\\tr{" "\\\\textrm{" macro)
          macro (replace-regexp-in-string  "\\\\tb{" "\\\\textbf{" macro)
          macro (replace-regexp-in-string  "\\\\ti{" "\\\\textit{" macro))
    (insert macro)
    (when (string= extracted "")
          (goto-char (1- (point))))))


(defun insert-tex-partial-derivative ()
  (interactive)
  (insert "\\frac{\\partial  }{\\partial }")
  (goto-char (- (point) 13)))


(defun insert-tex-derivative ()
  (interactive)
  (insert "\\frac{\\mathrm{d}  }{\\mathrm{d}  }")
  (goto-char (- (point) 16)))


(defun insert-tex-fraction ()
  (interactive)
  (insert "\\frac{  }{  }")
  (goto-char (- (point) 6)))


(defun insert-tex-biblink ()
  (interactive)
  (let* (( masterFile (buffer-file-name))
         ( biblinkFile (concat biblink-path-commands-dir
                               (file-name-base masterFile)
                               "-biblink.tex"))
         ( key (car (reftex-citation t)))
         ( path (latex-locate-citation key))
         ( fields (ebibtex-key-get-fields key "author" "year"))
         ( command (replace-regexp-in-string "[0-9]" "" key))
         ( commands (latex-locate-citation-read biblinkFile))
           author year)
    (when fields (setq author (pop fields)
                       year (pop fields)))
    (insert (format "\\biblink[%s]{\\%s}{%s}" author command key))
    (with-temp-file biblinkFile
      (insert-file-contents biblinkFile)
      (goto-char (point-min))
      (when (re-search-forward command (point-max) t)
            (beginning-of-line)
            (delete-region (line-beginning-position)
                           (line-beginning-position 2)))
      (goto-char (point-max))
      (unless (looking-at "^$")
              (insert "\n"))
      (unless path
        (setq path (if author
                       (concat "NO FILE FOUND FOR: "
                                author " (" year ")")
                       "BibTeX key not found.")))
      (insert (concat "\\newcommand{\\" command
                      "}{" path "}\n")))))


(defun LaTeX-macro-hsection ( optional)
  (insert (concat "{" (read-string "Title: ")
                  "}\n{sec:" (random-string) "}")))


(defun LaTeX-env-layer ( environment)
  (let (( layers (mapcar 'car LaTeX-environment-layers))
        ( layer "")
        ( header "")
        ( footer "")
          pos region)
    (setq layer (completing-read "Layer name: "
                                 layers nil nil "")
          header (read-string "Layer header: ")
          region (region-tex-insert))
    (insert (format "\\begin{%s}{x}{%s}{%s}\n{%s}{lay:%s}\n"
              environment layer header footer (random-string)))
    (insert (concat "\\begin{layercontent}\n" region))
    (setq pos (point))
    (insert (format "\n\\end{layercontent}\n\\end{%s}\n%%\n"
              environment))
    (goto-char pos)))


(defun LaTeX-env-layer-split ( environment)
  (when (LaTeX-inside-environment-p)
    (let (( type (car (LaTeX-inside-environment-p)))
          ( header (cadr (LaTeX-inside-environment-p)))
          ( footer ""))
      (insert "\\end{layercontent}\n\\end{layer}\n%\n")
      (insert (format "\\begin{layer}{x}{%s}{%s}\n{%s}{lay:%s}\n"
              type header footer (random-string)))
      (insert "\\begin{layercontent}\n"))
    (re-search-forward "\\\\begin{layer}" (point-min) t -1)))


(defun LaTeX-insert-subscript ()
  (interactive)
  (insert "_{ }")
  (backward-char))


(defun LaTeX-insert-superscript ()
  (interactive)
  (insert "^{ }")
  (backward-char))


;;*** Function for viewing output in out folder

(defun TeX-view-other-directory ()
  "Modification of the TeX-view command from auctex.

Append absolute path to output and out folder to output file
name.  i3 window manager: close other windows when emacs occupies
whole display width.

Setting `TeX-output-extension' to \"pdf\" will cause
`TeX-view-predicate-list-builtin' to return the symbol
\"output-pdf\" and that in turn leads to the viewer command via
`TeX-view-program-selection'. This is needed for `TeX-command' to
choose the right viewing programm when it is run with the command
name \"View\".
"
  (interactive)
  (setq TeX-output-extension "pdf")
  (let (( output-file (concat (expand-file-name default-directory)
                              (file-name-as-directory "out")
                              (TeX-active-master (TeX-output-extension) nil))))
    (if (file-exists-p output-file)
        (progn
          (crowded-close-others)
          (TeX-command "View"
            '(lambda ( &optional extension nondirectory)
               output-file)
             -1))
        (message "Output file %S does not exist." output-file))))


(defun TeX-view-manage-windows ()
  (interactive)
  (crowded-close-others)
  (TeX-view))


;;*** Function for handling log files in out folder

(defun TeX-handle-log-file-other-directory ()
  (let* (( log-file (with-current-buffer TeX-command-buffer
                           (TeX-active-master "log")))
         ( tex-path (file-name-directory
                    (with-current-buffer TeX-command-buffer
                           (buffer-file-name))))
         ( tex-out-dir (file-name-as-directory "out"))
         ( default-log-path (concat tex-path log-file))
         ( proper-log-path (concat tex-path tex-out-dir log-file)))
    (unless (string= default-log-path proper-log-path)
      (when (get-file-buffer default-log-path)
        (message "Killing buffer of non-existent file %s"
                 (buffer-file-name (get-buffer log-file)))
        (kill-buffer (get-file-buffer default-log-path)))
      (unless (get-file-buffer proper-log-path)
        (message "Visiting file %s" proper-log-path)
        (with-current-buffer (find-file-noselect proper-log-path)
          (unless (string= major-mode "latex-mode")
            (latex-mode))
          (read-only-mode 1))))))


(defun TeX-next-error-other-directory ()
  (interactive)
  (outline-show-all)
  (TeX-handle-log-file-other-directory)
  (TeX-next-error)
  (TeX-handle-log-file-other-directory))


(defun TeX-previous-error-other-directory ()
  (interactive)
  (outline-show-all)
  (TeX-handle-log-file-other-directory)
  (TeX-previous-error 1)
  (TeX-handle-log-file-other-directory))


;; ! Missing $ inserted.
;; <inserted text> 
;;                 $
;; l.153   multi-phased]_p3-p40-20190201233410.pdf}
;;                                                 .FILE
;; I've inserted a begin-math/end-math symbol since I think
;; you left one out. Proceed, with fingers crossed.


(defun TeX-log-closing-parenthesis ()
  (let (( level 0)
        ( tree (list '(0 "log-header")))
          errors)
    (while (re-search-forward "\\([()]\\)\\|^!\s+\\([a-zA-Z].*\\)\n" nil t)
      (cond ((match-string 2)
             (setq errors (cons (list (cadar tree)
                                      (TeX-parse-error-fields))
                                errors)))
            ((match-string 1)
             (if (string= (match-string 1) ")")
                 (progn
                   (setq level (1- level))
                   (when (< level (caar tree))
                     (pop tree)))
               (setq level (1+ level))
               (when (looking-at "\\(.*\\.\\(tex\\|bbl\\)\\)[\s\n]")
                 (setq tree (cons (list level (match-string 1)) tree)))))))
    (reverse errors)))


(defun TeX-parse-error-fields ()
  (let (( brief (match-string 2))
        ( pos (point)))
    (re-search-forward "^l\\.\\([0-9]+\\)\s*" nil t)
    (end-of-line 2)
    (let (( description (buffer-substring pos (1- (match-beginning 0))))
          ( line-num (match-string 1))
          ( line-error (buffer-substring (match-end 0) (point)))
          ( remark-beg (line-beginning-position 2)))
      (re-search-forward "^\s*$" nil t)
      (list brief description line-num line-error
            (buffer-substring remark-beg (line-end-position 0))))))
  

(defun TeX-assemble-error-help ()
  (interactive)
  (let* (( master-file (buffer-file-name))
         ( basename (file-name-base master-file))
         ( master-dir (file-name-directory master-file))
         ( log-file (concat master-dir
                           (file-name-as-directory "out")
                           basename ".log"))
          errors)
    (with-temp-buffer
      (insert-file-contents log-file)
      (goto-char (point-min))
      (setq errors (TeX-log-closing-parenthesis)))
    (switch-to-buffer-other-window "*TeX Error Help*")
    (let (( window (selected-window))
          ( inhibit-read-only t))
      (erase-buffer)
      (dolist ( error errors)
        (let (( file (pop error)))
          (unless (file-name-absolute-p file)
            (setq file (concat master-dir file)))
          (let* (( fields (pop error))
                 ( brief (pop fields))
                 ( excerpt (pop fields))               
                 ( line-relative (string-to-number (pop fields)))
                 ( line-brief (pop fields))
                 ( remark (pop fields))
                 ( re (TeX-error-compose-re (if (string= excerpt "\n")
                                                line-brief
                                              excerpt)))
                 ( pos-line (find-regexp-in-file file re))
                 ( error-line (number-to-string
                                  (if pos-line
                                      (cadr pos-line)
                                    line-relative))))
            (insert (propertize "Error in " 'face 'font-lock-warning-face))
            (insert-button (concat file ": l." error-line) 'action
                  `(lambda ( file)
                     (find-file-other-window ,file)
                     (goto-char (point-min))
                     (if (re-search-forward ,re nil t)
                         (goto-char (match-end 1))
                       (goto-line ,line-relative))
                     (delete-overlays-at-point)
                     (select-window ,window)))
            (insert (propertize (format "\n%s\n" brief) 'face 'italic))
            (insert (format "%s\nl.%s %s\n%s\n\n\n"
                            excerpt
                            line-relative line-brief
                            remark)))))))
  (goto-char (point-min)))


(defun TeX-error-compose-re ( raw)
  (setq raw (replace-regexp-in-string "<argument>\\|\\.\\.\\." "" raw)
        raw (regexp-quote (string-trim raw))
        raw (replace-regexp-in-string "\s\s+" "\s" raw)
        raw (concat "\\(" raw "\\)")
        raw (replace-regexp-in-string "\s*\n" "\\\\)\\\\(\n" raw)
        raw (replace-regexp-in-string "\s\\|\n" "[\s\n]*" raw)))


(defun find-regexp-in-file ( file regexp)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (list (match-end 1) (line-number-at-pos (match-end 1))))))


;;*** Function for abstract and keyword formatting

(defun TeX-abstract-extract-format ()
  (goto-char (point-min))
  (let* (( begin (re-search-forward "\\\\begin{abstract}" (point-max) t))
         ( end (progn
                 (re-search-forward "\\\\end{abstract}" (point-max) t)
                 (match-beginning 0)))
         ( text (buffer-substring-no-properties begin end)))
    (while (string-match "[^{]\\\\[^\s{]+{\\([^}]*\\)}" text)
           (setq text (replace-match
                        (concat " " (match-string 1 text)) nil nil text)))
    (while (string-match "{\\\\[^\s{]+\s\\([^}]*\\)}" text)
           (setq text (replace-match (match-string 1 text) nil nil text)))
    (while (string-match "[^{]\\(\\\\[^\s\n]+\\)" text)
           (setq text (replace-match " " nil nil text 1)))
    (setq text (replace-regexp-in-string "\n" " " text)
          text (replace-regexp-in-string "\s\s+" " " text)
          text (replace-regexp-in-string "\s+\\." "." text)
          text (replace-regexp-in-string "\s+\\," "," text)
          text (replace-regexp-in-string "\s+\\;" ";" text)
          text (string-trim text))))

(defun TeX-keywords-extract-format ()
  (goto-char (point-min))
  (let* (( begin (re-search-forward "\\\\begin{keyword}" (point-max) t))
         ( end (progn
                 (re-search-forward "\\\\end{keyword}" (point-max) t)
                 (match-beginning 0)))
         ( text (buffer-substring-no-properties begin end))
         ( start 0))
    (setq text (replace-regexp-in-string "\\\\sep" "; " text)
          text (replace-regexp-in-string "\n" " " text)
          text (replace-regexp-in-string "\s\s+" " " text)
          text (replace-regexp-in-string "\s+\\." "." text)
          text (replace-regexp-in-string "\s+\\," "," text)
          text (replace-regexp-in-string "\s+\\;" ";" text)
          text (string-trim text))
    (while (string-match "\\(^\\|;\s\\)\\([^\s;]+\\)" text start)
           (setq start (match-end 0))
           (setq text (replace-match
                 (capitalize (match-string 2 text)) nil nil text 2)))
    text))

(defun text-clear-layout ( text)
  (setq text (replace-regexp-in-string "\n" " " text)
        text (replace-regexp-in-string "\s\s+" " " text)
        text (replace-regexp-in-string "\s+\\." "." text)))

(defun TeX-title-extract-format ()
  (goto-char (point-min))
  (let* (( begin (re-search-forward "\\\\title{" (point-max) t))
         ( end (progn
                 (backward-char)
                 (forward-sexp)
                 (1- (point))))
         ( text (buffer-substring-no-properties begin end)))
        (text-clear-layout text)))

(defun TeX-to-ascii-extraction ( path dir)
  (let* (( master (file-name-base path))
         ( abstractFile (concat (file-name-as-directory dir)
                                master "-ABSTRACT.txt"))
         ( keywordsFile (concat (file-name-as-directory dir)
                                master "-KEYWORDS.txt"))
         ( titleFile (concat (file-name-as-directory dir)
                                master "-TITLE.txt"))
         abstract keywords title)
    (with-temp-buffer
      (insert-file-contents path)
      (setq abstract (TeX-abstract-extract-format)
            keywords (TeX-keywords-extract-format)
            title (TeX-title-extract-format)))
    (with-temp-file abstractFile (insert abstract))
    (with-temp-file keywordsFile (insert keywords))
    (with-temp-file titleFile (insert title))))


;;*** Function for archiving tex file trees

(defface path-exists
  '((t :foreground "green"))
  "Face for existing file paths."
  :group 'file-path-operations)
(defface path-exists-not
  '((t :foreground "red"))
  "Face for existing file paths."
  :group 'file-path-operations)
(defface unique-base-name
  '((t :foreground "blue"))
  "Face for existing file paths."
  :group 'file-path-operations)
(defface archive-file-path
  '((t :foreground "SeaGreen4"))
  "Face for existing file paths."
  :group 'file-path-operations)

(defun get-filename-at-pos ( start dir)
  (re-search-forward "}" (point-max) t)
  (let (( s (buffer-substring-no-properties start (1- (point)))))
    (setq s (replace-regexp-in-string "\\\\string~/"
              (file-name-as-directory (getenv "HOME")) s))
    (setq s (replace-regexp-in-string "\\\\HOME/"
              (file-name-as-directory (getenv "HOME")) s t))
    (expand-file-name s dir)))

(defun basename-add-extension ( base baseExt ext str)
  (when (string= base baseExt)
        (setq baseExt (file-name-sans-extension base)))
  (setq base (file-name-sans-extension base))
  (concat baseExt
      (propertize str 'face
          (if (file-exists-p (concat base ext))
            'path-exists
            'path-exists-not))))

(defun count-existing-files ( base ext count)
  (if (file-exists-p (concat base ext))
      (1+ count)
      count))

(defun TeX-file-include-list ( dir path)
  (let (( re (concat "^[^%\n]*\\\\\\("
                     "input\\|include[^{[]*"
                  "\\|movie.*{.*\n*"
                  "\\|bibliography[^{[]*"
                     "\\)[^{]*{"))
        ( countGra 0) ( countPdf 0) ( countEps 0)
        start procede base baseExt files remainFiles)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (re-search-forward re (point-max) t)
        (setq start (point))
        (setq procede t)
        (save-match-data
          (when (re-search-forward "providecommand\\|renewcommand"
                                   (line-beginning-position) t -1)
                (setq procede nil)))
        (goto-char start)
        (when procede
          (setq baseExt nil)
          (message "%s" (match-string 1))
          (cond ((or (string= "input" (match-string 1))
                     (string= "include" (match-string 1)))
                 (setq base (get-filename-at-pos start dir))
                 (setq baseExt (basename-add-extension
                                 base base ".tex" ".tex"))
                 (when (file-exists-p baseExt)
                       (push baseExt remainFiles)))
                ((string= "bibliography" (match-string 1))
                 (setq base (get-filename-at-pos start dir))
                 (setq baseExt (basename-add-extension
                                 base base ".bib" ".bib"))
                 (when (file-exists-p baseExt)
                       (add-to-list baseExt remainFiles)))
                ((string= "includegraphics" (match-string 1))
                 (setq base (get-filename-at-pos start dir))
                 (setq baseExt (basename-add-extension
                                 base base ".pdf" " pdf"))
                 (setq baseExt (basename-add-extension
                                 base baseExt ".eps" " eps"))
                 (setq baseExt (basename-add-extension
                                 base baseExt
                                 "-eps-converted-to.pdf" " eps->pdf"))
                 (setq countGra (1+ countGra))
                 (setq countPdf
                       (count-existing-files base ".pdf" countPdf))
                 (setq countPdf
                       (count-existing-files base
                              "-eps-converted-to.pdf" countPdf))
                 (setq countEps
                       (count-existing-files base ".eps" countEps)))
                ((string-match "movie" (match-string 1))
                 (setq base (get-filename-at-pos start dir))
                 (setq baseExt (basename-add-extension
                                 base base ".avi" ".avi"))))
          (when baseExt
                (push baseExt files)))))
    (list remainFiles files countGra countPdf countEps)))


(defun TeX-all-file-include-list ( path)
  (let (( dir (file-name-directory path))
        ( remainFiles (list path))
        ( out (concat (file-name-directory path)
                      (file-name-as-directory "out")
                      (file-name-base path)))
        ( countGra 0)
        ( countPdf 0)
        ( countEps 0)
        files usePdf useEps outExt)
    (while remainFiles
      (setq infoList (TeX-file-include-list dir (pop remainFiles)))
      (setq remainFiles (append (pop infoList) remainFiles))
      (setq files (append (pop infoList) files))
      (setq countGra (+ (pop infoList) countGra))
      (setq countPdf (+ (pop infoList) countPdf))
      (setq countEps (+ (pop infoList) countEps)))
    (when (= countPdf countGra) (setq usePdf t))
    (when (= countEps countGra) (setq useEps t))
    (setq outExt (basename-add-extension out out ".pdf" " pdf"))
    (setq outExt (basename-add-extension out outExt ".dvi" " dvi"))
    (list (reverse files) outExt usePdf useEps)))


(defun extend-base-name ( path str sep)
  (concat (propertize (file-name-base path) 'face 'archive-file-path)
          (propertize sep 'face 'archive-file-path)
          (propertize str 'face 'unique-base-name)
          (propertize (file-name-extension path t) 'face 'archive-file-path)))


(defun unique-base-names ( pathList)
  (let ( dirs duplicates unique uniqueDirs)
    (dolist ( path pathList)
      (setq dirs (append (split-string path "/") dirs)))
    (setq duplicates (delete-dups (dupes dirs)))
    (dolist ( path pathList)
      (dolist ( dir (split-string path "/"))
        (unless (member dir duplicates)
                (setq unique dir)))
      (setq uniqueDirs (cons unique uniqueDirs)))
    (reverse uniqueDirs)))


(defun unique-file-names ( files)
  (let ( names dupliPaths uniques oldPaths)
    (dolist ( path files)
      (setq names (cons (file-name-nondirectory path) names)))
    (dolist ( name (delete-dups (dupes names)))
      (dolist ( path files)
        (when (string= (file-name-nondirectory path) name)
              (setq dupliPaths (cons path dupliPaths))))
      (setq uniques (append (unique-base-names (reverse dupliPaths)) uniques))
      (setq oldPaths (append (reverse dupliPaths) oldPaths))
      (setq dupliPaths nil))
    (zip-lists oldPaths uniques)))


(defun dupes ( lst)
  (cond ((null lst) '())
        ((member (car lst) (cdr lst))
         (cons (car lst) (dupes (cdr lst))))
        (t (dupes (cdr lst)))))


(defun zip-lists ( list1 list2)
  (let (( list1 (reverse list1))
        ( list2 (reverse list2))
          zipped)
    (dolist ( item list1)
      (setq zipped (cons (pop list2) zipped))
      (setq zipped (cons item zipped)))
    zipped))


(defun directory-files-with-basename ( path)
  (let ( files)
    (dolist ( file (directory-files (file-name-directory path)))
            (when (string= (file-name-base file)
                           (file-name-base path))
                  (setq files (cons
                    (concat (file-name-directory path) file)
                              files))))
    files))


(defun delete-files-with-basename ( path)
  (dolist ( file (directory-files-with-basename path))
          (delete-kill-file file)))


(defun insert-copy-file-tree ( path files out &optional newDir)
  (let (( uniques (unique-file-names files)) new)
    (when newDir
      (setq newDir (propertize (contract-file-name newDir)
                     'face 'archive-file-path)))
    (insert "Copy files to archive:\n\n")
    (insert "----Master file:\n\n")
    (insert (propertize (format "%s\n" (contract-file-name path))
                        'face 'bold))
    (when newDir
      (insert (propertize "-> " 'face 'archive-file-path))
      (insert (format "%s" newDir))
      (insert (propertize (file-name-nondirectory path)
                'face 'archive-file-path)))
    (insert (format "\n\n----Include file(s) %s:\n\n" (length files)))
    (dolist ( path files)
      (insert (format "%s\n" (contract-file-name path)))
      (when newDir
        (if (member path uniques)
            (setq new (extend-base-name path
                        (cadr (member path uniques)) "-"))
            (setq new (propertize (file-name-nondirectory path)
                        'face 'archive-file-path)))
        (insert (propertize "-> " 'face 'archive-file-path))
        (insert (format "%s\n" (concat newDir new)))))
    (insert "\n----Output file(s):\n\n")
    (insert (format "%s\n" (contract-file-name out)))
    (when newDir
      (insert (propertize "-> " 'face 'archive-file-path))
      (insert newDir)
      (insert (propertize (file-name-nondirectory out)
                'face 'archive-file-path)))))


(defun TeX-archive-copy-files ( path newDir files out extension &optional del)
  (unless (file-directory-p newDir)
          (dired-create-directory newDir))
  (when (string= (car (split-string (buffer-name) "<"))
                 "README-archive.txt")
        (write-file (concat newDir "README-archive.txt"))
        (kill-buffer))
  (let* (( pdfOut (concat (substring out 0 -8) ".pdf"))
         ( dviOut (concat (substring out 0 -8) ".dvi"))
         ( uniques (unique-file-names files))
         ( masterPath (file-name-directory path))
         ( masterBase (file-name-base path))
         ( auxFile (concat masterPath
                           (file-name-as-directory "out")
                           masterBase ".aux"))
         ( bibFile (concat masterPath
                           (file-name-as-directory "include")))
         ( bibNew (concat masterPath
                          (file-name-as-directory "out")
                          masterBase ".bib"))
         ( extractBib ))
    (dired-copy-file path
      (concat newDir (file-name-nondirectory path)) t)
    (when (file-exists-p pdfOut)
          (dired-copy-file pdfOut
            (concat newDir (file-name-nondirectory pdfOut)) t))
    (when (file-exists-p dviOut)
          (dired-copy-file dviOut
            (concat newDir (file-name-nondirectory dviOut)) t))
    (when del (delete-kill-file path)
              (delete-kill-file pdfOut)
              (delete-kill-file dviOut))
    (dolist ( file files)
      (when (string-match " pdf" file)
            (setq file (substring file 0 -17))
            (cond ((string= extension "PDF")
                   (setq file (concat file ".pdf")))
                  ((string= extension "EPS")
                   (setq file (concat file ".eps")))))
      (when (string-match ".bib" file)
            (setq bibFile (concat bibFile (file-name-nondirectory file)))
            (message "%s %s %s" bibFile auxFile bibNew)
            (shell-command (concat "bibtool -q"
                                   " -i " bibFile
                                   " -x " auxFile
                                   " -o " bibNew))
            (setq file bibNew
                  new (file-name-nondirectory bibNew)))
      (if (member file uniques)
          (setq new (extend-base-name file
                      (cadr (member file uniques)) "-"))
          (setq new (file-name-nondirectory file)))
      (setq new (concat newDir new))
      (dired-copy-file file new t))
    (TeX-command-no-directory path newDir)
    (TeX-to-ascii-extraction path newDir)
    (message "Copying file(s) into archive successful!")
    (setq cleanup (switch-to-buffer "*Clean Up*"))
    (erase-buffer)
    (insert "Delete files:\n\n")
    (insert (format "%s\n" path))
    (dolist ( file (directory-files-with-basename pdfOut))
      (insert (format "%s\n" file)))
    (when (y-or-n-p "Delete files? ")
          (delete-kill-file path)
          (delete-files-with-basename pdfOut))
    (kill-buffer cleanup)))


(defun TeX-archive-files ( path newDir)
  (setq dired-copy-preserve-time t)
  (when (get-buffer "*Choices*") (kill-buffer "*Choices*"))
  (let* (( fileInfo (TeX-all-file-include-list path))
         ( files (pop fileInfo))
         ( out (pop fileInfo))
         ( usePdf (pop fileInfo))
         ( useEps (pop fileInfo))
         ( extensions nil))
    (switch-to-buffer "README-archive.txt")
    (erase-buffer)
    (when (file-directory-p newDir)
          (insert "Archive directory already exists!\n"))
    (insert-copy-file-tree path files out newDir)
    (when usePdf (setq extensions (cons "PDF" extensions )))
    (when useEps (setq extensions (cons "EPS" extensions )))
    (insert "\n\n")
    (if files
      (progn
        (insert "Choose graphics file type: ")
        (dolist ( extension extensions)
          (insert-button (concat "[" extension "]")
            'path path 'newDir newDir 'files files 'out out
            'extension extension
            'action (lambda (b)
                (TeX-archive-copy-files
            (button-get b 'path) (button-get b 'newDir)
            (button-get b 'files) (button-get b 'out)
            (button-get b 'extension))))
          (insert " ")))
      (insert-button (concat "[Apply]")
        'path path 'newDir newDir 'files nil 'out out
        'extension nil
        'action (lambda (b)
            (TeX-archive-copy-files
        (button-get b 'path) (button-get b 'newDir)
        (button-get b 'files) (button-get b 'out)
        (button-get b 'extension)))))))


(defun TeX-command-no-directory ( master dir)
  (let (( re (concat "^\\([^%\n]*\\)\\\\\\("
                     "input\\|include[^{[]*"
                  "\\|movie.*{.*\n*"
                  "\\|bibliography[^{[]*"
                     "\\)[^{]*{\\([^}]+\\)}")))
    (dolist ( file (directory-files dir t ".*\\.tex" t))
      (with-temp-file file
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward re (point-max) t)
          (if (string= "bibliography" (match-string 2))
              (replace-match (file-name-base master) nil nil nil 3)
              (unless (string-match "command" (match-string 1))
                      (replace-match (file-name-nondirectory (match-string 3))
                                 nil nil nil 3))))))))


(defun TeX-archive ()
  (interactive)
  (let* (( master (buffer-file-name))
         ( newDir (concat (file-name-as-directory (getenv "HOME"))
                          (file-name-as-directory "archive")
                          (file-name-as-directory
                            (file-name-base master)))))
    (setq newDir (file-name-as-directory
                   (expand-file-name
                     (read-directory-name "Archive files into directory: "
                       (file-name-add-time newDir t) nil nil nil))))
    (TeX-archive-files master newDir)))


;;*** Function for walking file hierachy tree

(defun TeX-include-files ( masterFile &optional path)
  (unless path (setq path masterFile))
  (let (( masterDir (file-name-directory masterFile))
        ( paths nil)
        ( filename ""))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (re-search-forward
               "^[^%\n]*\\\\\\(input\\|include\\)[^gc\n]*{\\([^{}\n]*\\)}"
               (point-max) t)
             (setq filename (match-string 2))
             (unless (string= (file-name-extension filename) "tex")
                     (setq filename (concat filename ".tex")))
             (if (string-prefix-p "/" filename)
               (setq paths (cons filename paths))
               (setq paths (cons (concat masterDir filename)
                                 paths)))))
    (nreverse paths)))


(defun TeX-include-files-all ( masterFile)
  (let* (( masterDir (file-name-directory masterFile))
         ( remainFiles (list masterFile))
         ( allFiles nil))
    (while remainFiles
      (setq addFiles (TeX-include-files masterFile (pop remainFiles)))
      (setq remainFiles (append addFiles remainFiles))
      (setq allFiles (append addFiles allFiles)))
    (cons masterFile allFiles)))


;;*** Biblink group


(defgroup biblink nil
  "Provide links to local files in PDF documents."
  :tag "Latex Biblink"
  :group 'LaTeX-macro)


;;*** Biblink customization

(defcustom biblink-path-commands-dir "/home/dan/tex/include/"
  "Directory of biblink commands input file.

The biblink database file comes with extension \".tex\". It holds
the latex commands which define aliases for absolute path of the
cited publication."
  :group 'biblink
  :type 'file)


;;*** Biblink functions

(defun latex-locate-citation ( bibtexkey)
  "Find file with a name starting with a given BIBTEXKEY.

The list of possible files is retrieved by the linux command
\"locate\". There are cases when files are new or recently renamed,
then it is necessary to update the database by executing
\"updatedb\".

BIBTEXKEY is the citation key of the entry in the BibTeX-database
file. The return value is the found absolute path."
  (save-match-data
    (let* (( out (shell-command-to-string
                   (concat "locate " bibtexkey)))
           ( paths (delete "" (split-string out "\n")))
           ( found nil))
      (while (and (setq path (pop paths))
                  (not found))
        (when (and (string-prefix-p bibtexkey (file-name-base path))
                   (file-exists-p path))
              (setq found path)))
      found)))

(defun latex-locate-citation-list ()
  "Gather BibTeX keys, define a valid latex command name and
associate corresponding file information.

Loop over all biblink commands in current buffer and return a
dictionary with one valid command name and file path
information. File path information may be a local file path, a
file not found or a BibTeX key not found information.

Example return entry of dictionary:
(\"\\kdjd\" \"/path/to/pdf/file\").

The path is determined by `latex-locate-citation'.

On the way, according to the BibTeX key, the author and command
arguments of the biblink command are updated directly in the
buffer."
  (save-excursion
    (goto-char (point-min))
    (let (( re (concat "^[^%\n]*"
                       "\\\\biblink"
                       "\\(\\[[^][]*\\]\\|\s*\\)"
                       "{\\([^{}\s\n]*\\)}"
                       "{\\([^{}\s\n]+\\)}"))
          ( dict nil)
          key command fields author year path)
      (while (re-search-forward re (point-max) t)
        (setq key (match-string-no-properties 3)
              command (concat
                         "\\" (replace-regexp-in-string "[0-9]" "" key))
              fields (ebibtex-key-get-fields key "author" "year")
              path (latex-locate-citation key)
              author nil
              year nil)
        (when fields
          (setq author (pop fields)
                year (pop fields)))
        (when (string-match-p "\\[\s*\\]" (match-string 1))
          (replace-match (concat "[" author "]") nil nil nil 1))
        (replace-match (concat "\\" command) nil nil nil 2)
        (unless path
          (setq path (if author
                         (concat "NO FILE FOUND FOR: "
                                 author " (" year ")")
                         "BibTeX key not found.")))
        (setq dict (cons (list command path) dict)))
      dict)))

(defun latex-locate-citation-all ( masterFile)
  "Get biblink information from all project files.

MASTERFILE is the root (main) tex-file of the project.

Loop over all included external source files, the master file
inclusive. Perform search for biblink commands, fetch command
names (non-numeric part of the BibTeX keys) and associated paths
inside every processed file. Change biblink arguments according
to current state of the locate database.

See also `latex-locate-citation-list'."
  (let (( citations nil)
        ( buffer nil))
    (dolist ( path (TeX-include-files-all masterFile))
      (when (setq buffer (get-file-buffer path))
        (with-current-buffer buffer (save-buffer)))
      (with-temp-file path
        (insert-file-contents path)
        (setq citations (append (latex-locate-citation-list)
                                citations))))
    (delete-dups citations)))


(defun latex-locate-citation-insert ()
  (interactive)
  (let* (( masterFile (buffer-file-name))
         ( biblinkFile (concat biblink-path-commands-dir
                               (file-name-base masterFile)
                               "-biblink.tex")))
    (unless (file-exists-p biblinkFile)
      (with-temp-buffer (write-file biblinkFile)))
    (with-temp-file biblinkFile
      (dolist ( pair (latex-locate-citation-all masterFile))
        (insert (format "\\newcommand{%s}{%s}\n"
                        (car pair) (cadr pair))))))
    (message "Citation link commands finished."))


(defun latex-locate-citation-read ( biblinkFile)
  (let* (( commands nil))
    (with-temp-buffer
      (insert-file-contents biblinkFile)
      (goto-char (point-min))
      (while (re-search-forward "\\\\newcommand{\\\\\\([a-zA-Z]+\\)}"
                                (point-max) t)
        (setq commands (cons (match-string-no-properties 1) commands))))
    commands))

(defun layers-get-footer ()
  (save-excursion
    (goto-char (point-min))
    (let (( footers nil))
      (while (re-search-forward "\\\\layerfooter" nil t)
        (save-match-data (forward-sexp))
        (setq footers (cons (buffer-substring-no-properties
                             (1+ (match-end 0)) (1- (point)))
                            footers)))
      (delete-dups (remove "" footers)))))

(defun layers-insert-footer ()
  (interactive)
  (insert (minibuffer-with-setup-hook #'minibuffer-completion-help
            (completing-read "Insert footer: "
              (layers-get-footer) nil t ""))))

(defun latex-locate-citation-view ()
  (interactive)
  (save-excursion
    (unless (re-search-forward "[ }.,]" (line-beginning-position) t -1)
            (beginning-of-line))
    (re-search-forward "\\\\cite[^{}\n]*{\\([^}\n]+\\)")
    (let* (( word (match-string-no-properties 1))
           ( paths (delete ""
                     (split-string
                       (shell-command-to-string (concat "locate " word))
                       "\n")))
           ( view nil)
           ( path nil))
      (while (and (not view) (setq path (pop paths)))
             (when (string-prefix-p word (file-name-base path) t)
                   (setq view path)))
      (if path
          (let* (( external (file-external-app-command path))
                 ( binCmd (concat external " " (shell-quote-argument path))))
                (crowded-close-others)
                (async-shell-command binCmd nil))
          (message "No file name beginning with '%s' found." word)))))

(defun LaTeX-preamble-end ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\\\begin{document}" (point-max) t)
          (line-end-position 0))))

(defun LaTeX-parse-newlayers ()
  (save-excursion
    (goto-char (point-min))
    (let (( layers nil)
          ( limit (LaTeX-preamble-end)))
      (while (re-search-forward
               "\\\\newlayer{x?}{\\([a-z]+\\)}{\\([a-z]+\\)}"
               limit t)
        (setq layers (cons (list (match-string-no-properties 1)
                                 (match-string-no-properties 2))
                           layers)))
      (setq-local LaTeX-environment-layers layers))))


(defun LaTeX-goto-matching-end ( limit)
  (let (( count 0))
    (while (>= count 0)
           (re-search-forward "\\\\\\(begin\\|end\\)" limit t)
           (if (string= (match-string 1) "begin")
               (setq count (1+ count))
               (setq count (1- count))))))

(defvar LaTeX-layer-begin-map
  (let (( map (make-sparse-keymap)))
    (define-key map (kbd "<S-mouse-2>") 'LaTeX-mouse-layer-folding)
    map))

(defun LaTeX-fontify-environments ( limit)
  (when (re-search-forward "^\s*\\\\begin{layer}{\\(x?\\)}{\\([a-z]+\\)}{\\([^{}%]*\\)}"
                           limit t)
    (let (( beg (match-beginning 0))
          ( beg1 (line-beginning-position 2))
          ( visible-beg (match-beginning 1))
          ( type (match-string 2))
          ( type-beg (match-beginning 2))
          ( type-end (match-end 2))
          ( title-beg (match-beginning 3))
          ( title-end (match-end 3)))
      (LaTeX-goto-matching-end nil) ;; (LaTeX-find-matching-end)
      (let* (( end (line-end-position))
             ( block-end (line-beginning-position))
             ( faces (LaTeX-environment-faces
                       type LaTeX-environment-layers))
             ( layer-bg (car faces)))
        ;; Clear properties from entire layer block
        (remove-text-properties beg end
          '( font-lock-face nil face nil keymap nil
             syntax-table nil rear-nonsticky nil line-prefix nil))
        (add-text-properties beg beg1
          `( keymap ,LaTeX-layer-begin-map))
        (if (invisible-p beg1)
            (progn
              (add-text-properties visible-beg (1+ visible-beg)
                '( face font-lock-warning-face))
              (add-text-properties title-beg title-end
                '( face font-lock-string-face)))
          (add-text-properties beg end
                               '( font-lock-fontified t
                                  font-lock-multiline t))
          (add-text-properties beg1 block-end `( face ,layer-bg))
          (add-text-properties beg beg1 '( face LaTeX-layer-begin-bg))
          (add-text-properties block-end (1+ end)
                               '( face LaTeX-layer-end-bg))
          (add-text-properties visible-beg (1+ visible-beg)
            `( face ,(parent-override-face
                       font-lock-warning-face 'LaTeX-layer-begin-bg)))
          (add-text-properties title-beg title-end
            `( face ,(parent-override-face
                       font-lock-string-face 'LaTeX-layer-begin-bg))))
        (add-text-properties type-beg type-end `( face ,layer-bg))))))


(defun specified-face-attributes ( face)
  (let ( attribute-list)
  (dolist ( attr-value (face-all-attributes face (selected-frame)))
    (let (( value (cdr attr-value)))
      (unless (eq value 'unspecified)
        (setq attribute-list (cons value attribute-list))
        (setq attribute-list (cons (car attr-value) attribute-list)))))
  attribute-list))

(defun parent-override-face ( parent-face override-face)
  (cons :inherit
        (cons parent-face
              (specified-face-attributes override-face))))

(defun LaTeX-inside-environment-p ()
  (save-excursion
    (save-match-data
      (end-of-line)
      (when (re-search-forward
              "\\\\\\(begin\\|end\\){layer}\\(.*\\)"
                               (point-min) t -1)
            (when (string= (match-string 1) "begin")
                  (let (( name (match-string 2)))
                       (string-match "{x?}{\\([a-z]+\\)}{\\([^{}]*\\)}" name)
                       (list (match-string-no-properties 1 name)
                             (match-string-no-properties 2 name))))))))

(defun LaTeX-environment-faces ( environment layers)
  (let (( hue (cadr (assoc environment layers))))
       (cdr (assoc hue LaTeX-map-block-colors))))

(defun LaTeX-fontify-keywords-inside ( limit)
  (when (re-search-forward (concat "\\$[^$]*\\$"
                                "\\|&"
                                "\\|\\\\\\\\"
                                "\\|\\\\!" "\\|\\\\," "\\|\\\\:" "\\|\\\\;"
                                "\\|\\\\quad" "\\|\\\\qquad"
                                "\\|%.*"
                                "\\|\\\\notag")
                           limit t)
    (let* (( beg (match-beginning 0))
           ( end (match-end 0))
           ( match (match-string 0))
           ( type (car (LaTeX-inside-environment-p)))
           ( background (car (LaTeX-environment-faces
                               type
                               LaTeX-environment-layers))))
          (when background
            (cond ((string-prefix-p "$" match)
                   (put-text-property beg end
                                      'face `(:foreground "SaddleBrown"
                                              :inherit ,background)))
                  ((string-prefix-p "%" match)
                   (put-text-property beg end
                                      'face `(:foreground "Firebrick"
                                              :inherit ,background)))
                  ((string-prefix-p "\\" match)
                   (put-text-property beg end
                                      'face `(:foreground "red"
                                              :inherit ,background)))
                  (t
                   (put-text-property beg end
                                      'face `(:foreground "red"
                                              :inherit ,background
                                              :weight bold))))))
    t))


(defun LaTeX-fontify-level-indentation ( limit)
  "Fontify latex headline definitions (e.g. \\section{}).

Two properties are set for the sectioning commands in latex
buffers. On the one hand increasing indentations for decreasing
levels of headings produce a table of contents like layout when
the sections are folded. On the other hand, sectioning lines are
highlighted by different colors according to their levels.

Remark: E.g. keyword based hightlighting of lines can also be
done by `font-lock-add-keywords'. Since the same region is only
visited once by jit-lock, we need to add all text properties
together in one search. LIMIT is the search limit provided by
jit-lock."
  (let (( prefix ""))
    (when (re-search-forward "\\\\h?\\(\\(sub\\)*\\)section" limit t)
      (cond ((string= (match-string 1) "sub")
             (setq prefix "  "))
            ((string= (match-string 1) "subsub")
             (setq prefix "    ")))
      (remove-text-properties (line-beginning-position)
                              (line-end-position)
                              '( line-prefix nil))
      (add-text-properties (line-beginning-position)
                           (line-end-position)
                           `( line-prefix ,prefix))
      t)))

(defun LaTeX-hide-layer ()
  (let (( beg (line-end-position)))
    (beginning-of-line)
    (when (and (not (begin-end-block-folded-p))
               (re-search-forward "\s*\\\\begin{layer}"
                                  (line-end-position) t)
               (re-search-forward "\\\\\\(begin\\|end\\){layer}"
                                  nil t))
      (when (string= (match-string 1) "end")
        (let (( overlay (make-overlay beg (line-end-position))))
          (overlay-put overlay 'invisible t)
          (overlay-put overlay 'isearch-open-invisible 'delete-overlay)
          (overlay-put overlay 'before-string "..."))))
    (goto-char beg)))


(defun LaTeX-show-layer ( &optional overlay)
  (unless overlay
    (setq overlay (begin-end-block-folded-p)))
  (when overlay
    (delete-overlay overlay)))


(defun LaTeX-mouse-layer-folding ( event)
  (interactive "e")
  (with-mouse-click-position event
    (toggle-block-folding 'LaTeX-show-layer 'LaTeX-hide-layer))
  (LaTeX-layer-font-lock-flush))

(defun LaTeX-layer-font-lock-flush ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (LaTeX-next-layer)
        (let (( beg (line-beginning-position)))
          (end-of-line)
          (re-search-forward "\\\\\\(begin\\|end\\){layer}" nil t)
          (if (string= (match-string 1) "end")
              (font-lock-flush beg (line-end-position))
            (beginning-of-line)))))))

(defun LaTeX-next-layer ()
  (end-of-line)
  (when (re-search-forward "^\s*\\\\begin{layer}" nil t)
    (goto-char (match-beginning 0))))

(defun LaTeX-previous-layer ()
  (beginning-of-line)
  (when (re-search-forward "^\s*\\\\begin{layer}" nil t -1)
    (goto-char (match-beginning 0))))

(defun LaTeX-layer-cloaked-p ()
  (let* (( pos (point))
         ( overlays (overlays-at pos))
         ( overlay (pop overlays)))
    (while (and overlay
                (or (< pos (overlay-start overlay) )
                    (< (overlay-end overlay) pos)
                    (not (overlay-get overlay 'invisible))))
      (setq overlay (pop overlays)))
    overlay))

(defun LaTeX-fold-cloaked-layers ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (LaTeX-next-layer)
        (when (begin-end-block-cloaked-p)
          (LaTeX-hide-layer))))))

(defun LaTeX-show-all-layers ()
  (save-excursion
    (goto-char (point-max))
    (while (LaTeX-previous-layer)
      (let (( overlay (begin-end-block-folded-p)))
        (when overlay
          (LaTeX-show-layer)))))
  (font-lock-flush))

(defun LaTeX-change-before-compilation ()
  (interactive)
  (LaTeX-parse-newlayers)
  (save-excursion
    (goto-char (point-min))
    (let* (( baseLayer "base")
           ( layers (delete baseLayer
                      (mapcar 'car LaTeX-environment-layers)))
           ( reLayers (concat "\\\\\\(begin\\|end\\){\\("
                              (string-join layers "\\|")
                              "\\)}"))
           ( reExclude (concat "^[^%\n]*"
                               "\\\\excludecomment{" baseLayer "}"
                            "\\|^[^%\n]*[^\\]%.*"
                               "\\\\includecomment{" baseLayer "}"))
           ( baseBegin (concat "\\begin{" baseLayer "}"))
           ( baseEnd (concat "\\end{" baseLayer "}")))
      (when (re-search-forward reExclude (point-max) t)
        (re-search-forward "\\\\begin{document}" (point-max) t)
        (re-search-forward "^\s*$\\|\\\\begin")
        (beginning-of-line)
        (insert (format "%s\n" baseBegin))
        (while (re-search-forward reLayers
                                  (point-max) t)
               (if (string= (match-string 1) "begin")
                   (progn
                     (beginning-of-line)
                     (insert (format "%s\n" baseEnd))
                     (end-of-line))
                   (progn
                     (end-of-line)
                     (insert (format "\n%s" baseBegin)))))
        (re-search-forward "\\\\end{document}" (point-max) t)
        (beginning-of-line)
        (insert (format "%s\n" baseEnd)))))
    (TeX-command-master))


(defun LaTeX-change-after-compilation ( output)
  (with-current-buffer (window-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(begin\\|end\\){submit}\n" (point-max) t)
             (delete-region (match-beginning 0) (match-end 0))))))

(defun TeX-evince-sync-view-2 ( de app)
  "Modified version of `TeX-evince-sync-view-1', which take
different output directory into account."
  (require 'url-util)
  (let* (( uri (concat "file://" (url-encode-url
                                  (concat (expand-file-name default-directory)
                                          (file-name-as-directory "out")
                                          (TeX-active-master (TeX-output-extension) nil)))))
         ( owner (dbus-call-method
                  :session (format "org.%s.%s.Daemon" de app)
                  (format "/org/%s/%s/Daemon" de app)
                  (format "org.%s.%s.Daemon" de app)
                  "FindDocument"
                  uri
                  t)))
    (if owner
	(with-current-buffer (or (when TeX-current-process-region-p
				   (get-file-buffer (TeX-region-file t)))
				 (current-buffer))
	  (dbus-call-method
	   :session owner
	   (format "/org/%s/%s/Window/0" de app)
	   (format "org.%s.%s.Window" de app)
	   "SyncView"
	   (buffer-file-name)
	   (list :struct :int32 (1+ (TeX-current-offset))
		 :int32 (1+ (current-column)))
	   :uint32 0))
      (error "Couldn't find the %s instance for %s" (capitalize app) uri))))

(defun TeX-evince-sync-view-3 ( de app)
  "Modified version of `TeX-evince-sync-view-1'.

Fix of unspecified file variable."
  (require 'url-util)
  (let* (( file (expand-file-name
                 (TeX-active-master (TeX-output-extension) t)))
         ( uri (concat "file://" (url-encode-url file)))
         ( owner (dbus-call-method
                  :session (format "org.%s.%s.Daemon" de app)
                  (format "/org/%s/%s/Daemon" de app)
                  (format "org.%s.%s.Daemon" de app)
                  "FindDocument"
                  uri
                  t)))
    (if owner
        (with-current-buffer (or (when TeX-current-process-region-p
                                   (get-file-buffer (TeX-region-file t)))
                                 (current-buffer))
          (dbus-call-method
           :session owner
           (format "/org/%s/%s/Window/0" de app)
           (format "org.%s.%s.Window" de app)
           "SyncView"
           (buffer-file-name)
           (list :struct
                 :int32 (1+ (TeX-current-offset))
                 :int32 (1+ (current-column)))
           :uint32 0))
      (error "Couldn't find the %s instance for %s"
             (capitalize app) uri))))

(defun TeX-evince-sync-view-mod ( event)
  "Run `TeX-evince-sync-view-1', which focuses the current emacs
position in evince viewer, emacs -> evince."
  (interactive "e")
  (with-mouse-click-position event
   (TeX-evince-sync-view-3 "gnome" "evince")))

(advice-add #'TeX-source-correlate-sync-source :after
            #'TeX-source-correlate-after-hook)

(defun TeX-source-correlate-after-hook ( file linecol &rest ignored)
  "Provide a hook for postprocessing of
`TeX-source-correlate-sync-source' function.  This will treat the
sync direction evince -> emacs."
  (delete-overlays-at-point)
  (message "Run TeX-source-correlate-after-hook finished."))

(defun LaTeX-pdf-find-position ()
  "Synchronizing direction: LaTeX source in emacs -> PDF-viewer.

From the current position of point, an external script runs the
PDF-viewer with a position name (string). The PDF-viewer should
update or reload the corresponding file and display the named
destination."
  (interactive)
  (let (( output-file (concat default-directory
                              (file-name-as-directory "out")
                              (TeX-active-master
                                (TeX-output-extension) nil)))
        ( reKey "{\\(\\(lay\\|sec\\):[a-z0-9]+\\)}")
        ( reStop "\\\\begin{layer}\\|\\\\end{layer}\\|\\\\h[sub]*section")
        ( pos (point)))
    (when (file-exists-p output-file)
      (when (re-search-forward (concat reKey "\\|" reStop)
                               (point-max) t)
        (unless (match-string 1)
          (goto-char (match-beginning 0))
          (re-search-forward (concat reKey "\\|" reStop)
                             (point-min)  t -1))
        (let (( dest (match-string 1)))
          (with-temp-buffer
            (shell-command (concat "evince --named-dest="
                                   dest " " output-file)
                         t)))))
    (shell-command "/home/dan/bin/i3-focus-window 5 emacs")
    (goto-char pos)))

(defun LaTeX-pdf-find-position-mouse ( event)
  (interactive "e")
  (point-set-to-mouse event)
  (LaTeX-pdf-find-position))

(defun pdf-find-LaTeX-source-show ( file linecol &rest _)
  (find-file (url-filename (url-generic-parse-url file)))
  (forward-line (car linecol))
  (beginning-of-line)
  (let (( end (point)))
    (cond ((looking-at "\s*\\\\end{\\([a-zA-Z*]+\\)}")
           (setq end (match-beginning 0))
           (re-search-forward (concat "\s*\\\\begin{" (match-string 1) "}")
                            (point-min) t -1)
           (set-mark (line-beginning-position 2))
           (goto-char end)
           (activate-mark)))))

(defun pdf-find-LaTeX-source ()
  (dbus-register-signal
       :session nil "/org/gnome/evince/Window/0"
       "org.gnome.evince.Window"
       "SyncSource"
       'pdf-find-LaTeX-source-show))

(defcustom AUCTeX-dnd-format "\\includegraphics[width=\\textwidth]{%s}"
  "What to insert, when a file is dropped on Emacs window.  %s is
replaced by the actual file name.  If the filename is located
under the directory of .tex document, only the part of the name
relative to that directory in used."
  :type 'string
  :group 'AUCTeX)

(defun AUCTeX-dnd-includegraphics (uri _)
  "Insert the text defined by `AUCTeX-dnd-format' when a file is
dropped on Emacs window."
  (let ((file (dnd-get-local-file-name uri t)))
    (when (and file (file-regular-p file))
      (if (string-match "/my/texinputs/path/to/images" file)
      (insert (format AUCTeX-dnd-format (file-name-nondirectory file)))
    (insert (format AUCTeX-dnd-format file))))))


(defcustom AUCTeX-dnd-protocol-alist
  '(("^file:///" . AUCTeX-dnd-includegraphics)
    ("^file://"  . dnd-open-file)
    ("^file:"    . AUCTeX-dnd-includegraphics))
  "The functions to call when a drop in `mml-mode' is made.
See `dnd-protocol-alist' for more information.  When nil, behave
as in other buffers."
  :type '(choice (repeat (cons (regexp) (function)))
                 (const :tag "Behave as in other buffers" nil))
  :version "22.1" ;; Gnus 5.10.9
  :group 'AUCTeX)


(define-minor-mode AUCTeX-dnd-mode
  "Minor mode to inser some text (\\includegraphics by default)
when a file is dopped on Emacs window."
  :lighter " DND"
  (when (boundp 'dnd-protocol-alist)
    (if AUCTeX-dnd-mode
        (set (make-local-variable 'dnd-protocol-alist)
             (append AUCTeX-dnd-protocol-alist dnd-protocol-alist))
      (kill-local-variable 'dnd-protocol-alist))))


;;*** Set variables

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      font-latex-script-display (quote ((raise -0.0) raise 0.0))
      TeX-outline-regexp (concat
                          "%[%*]\\(\\*\\**\\)\s[^\n]\\|"
                          "\s*\\\\\\(chapter\\|appendix\\|section\\|subsection\\)")
      TeX-outline-heading-alist '(("chapter" . 9)
                                  ("appendix" . 9)
                                  ("section" . 10)
                                  ("subsection" . 11)
                                  ("subsubsection" . 12))
      LaTeX-indent-level 0
      LaTeX-amsmath-label "eq:"
      LaTeX-label-alist (quote (("figure" . LaTeX-figure-label)
                                ("table" . LaTeX-table-label)
                                ("figure*" . LaTeX-figure-label)
                                ("table*" . LaTeX-table-label)
                                ("equation" . LaTeX-equation-label)
                                ("eqnarray" . LaTeX-eqnarray-label)
                                ("align" . LaTeX-equation-label)))
      TeX-PDF-from-DVI "Dvipdfmx")


;;*** Set variables for PDFLaTeX

;; Set compile command and flags for latexmk command.

;; Variable *TeX-expand-list-builtin* provides these expansion strings:

;; 1. %s: expand to input file
;; 2. %t: expand to input file with extension
;; 3. %f: expand to input file with "ps" (postscript) extension
;; 4. %d: expand to input file with "dvi" extension

;; Expansion strings for *latexmk* command:

;; 1. %O: expand to options
;; 2. %S: expand to file given at the end of the command line

;; If you want to make sure to get a .pdf file as output, just mention "-pdf"


(setq pdflatexCmd (concat "pdflatex"
                          " -synctex=1"
                          " -interaction=nonstopmode"
                          ;; " -output-directory=out"
                          " %t"))
(setq pdflatexmkCmd
  (concat "latexmk -bibtex -pdf -interaction=nonstopmode"
          ;; " -outdir=out"
          " -pdflatex=\"pdflatex -shell-escape -synctex=1\""
          " %t"))


;;*** Set variables for plain LaTeX

;; Variable *TeX-expand-list-builtin* provides these expansion strings:

;; 1. %`: Set variable *TeX-command-pos* to t (non-nil)\\
;;        and *TeX-command-text* to "" (empty string)
;; 2. %(mode): if *TeX-interactive-mode* is empty string (nil),\\
;;             append to latex command " -interaction=nonstopmode"
;; 3. %l: list with TeX-style-check LaTeX-command-style
;; 4. %': choose between *TeX-command-pos* and *TeX-command-text*
;; 5. %t: expand to input file

;; *TeX-interactive-mode* is nil, meaning use flag
;; "-interaction=nonstopmode" and thus go through latex compile process
;; with minimal user interaction, e.g., asking user for help input. This
;; option is usefull for automation of latex compilations.

;; Example:
;; %`     %l                 %(mode)                  %'       %t
;; latex  -file-line-error   -interaction=nonstopmode "\input" Dissertation.tex


(setq LaTeX-command "latex" ; gets inserted -> latexCmd -> TeX-command-list
      latexCmd "%`%l -output-directory=out %(mode)%' %t")


(setq latexmkCmd (concat "latexmk"
                         " -interaction=nonstopmode"
                         " -outdir=out"
                         " %t"))


;;*** Set variables for plain TeX

(setq texCmd (concat "%(PDF)%(tex)"
                     " %(file-line-error)"
                     " %(extraopts)"
                     " %`%S%(PDFout)%(mode)%' %t"))


;;*** Set variables TeX-command-list

;; 1. element: command for user, string to shell
;; 2. element: function to start the process
;; 3. element: modify expanded command string
;; 4. element: command only present in these modes
;; 5. element: transfer to respective menu entries


(setq TeX-command-list (list
  (list "PdfLatexmk" pdflatexmkCmd
                     'TeX-run-TeX
                     nil
                     '( latex-mode doctex-mode)
                     :help "Run PdfLatexmk")
  (list "Latexmk" latexmkCmd
                  'TeX-run-TeX
                  nil
                  '( latex-mode doctex-mode)
                  :help "Run Latexmk")
  (list "TeX" texCmd
              'TeX-run-TeX
              nil
              '( plain-tex-mode texinfo-mode ams-tex-mode)
              :help "Run plain TeX")
  (list "LaTeX" latexCmd
                'TeX-run-TeX
                nil
                '( latex-mode doctex-mode)
                :help "Run LaTeX")
  (list "PdfLaTeX" pdflatexCmd
                   'TeX-run-TeX
                   nil
                   '( latex-mode doctex-mode)
                   :help "Run LaTeX")
  (list "Makeinfo" "makeinfo %(extraopts) %t"
                   'TeX-run-compile
                   nil
                   '( texinfo-mode)
                   :help "Run Makeinfo with Info output")
  (list "Makeinfo HTML" "makeinfo %(extraopts) --html %t"
                        'TeX-run-compile
                        nil
                        '(texinfo-mode)
                        :help "Run Makeinfo with HTML output")
  (list "AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t"
                 'TeX-run-TeX
                 nil
                 '( ams-tex-mode)
                 :help "Run AMSTeX")
  (list "ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t"
                  'TeX-run-TeX
                  nil
                  '( context-mode)
                  :help "Run ConTeXt once")
  (list "ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t"
                       'TeX-run-TeX
                       nil
                       '( context-mode)
                       :help "Run ConTeXt until completion")
  (list "BibTeX" "bibtex %s"
                 'TeX-run-BibTeX
                 nil
                 t
                 :help "Run BibTeX")
  (list "Biber" "biber %s"
                'TeX-run-Biber
                nil
                t
                :help "Run Biber")
  (list "View" "%V"
               'TeX-run-discard-or-function
               t
               t
               :help "Run Viewer")
  (list "Print" "%p"
                'TeX-run-command
                t
                t
                :help "Print the file")
  (list "Queue" "%q"
                'TeX-run-background
                nil
                t
                :help "View the printer queue"
                :visible 'TeX-queue-command)
  (list "File" "%(o?)dvips %d -o %f "
               'TeX-run-dvips
               t
               t
               :help "Generate PostScript file")
  (list "Dvips" "%(o?)dvips %d -o %f "
                'TeX-run-dvips
                nil
                t
                :help "Convert DVI file to PostScript")
  (list "Dvipdfmx" "dvipdfmx out/%d -o out/%s.pdf"
                   'TeX-run-dvipdfmx
                   nil
                   t
                   :help "Convert DVI file to PDF with dvipdfmx")
  (list "Ps2pdf" "ps2pdf %f"
                 'TeX-run-ps2pdf
                 nil
                 t
                 :help "Convert PostScript file to PDF")
  (list "Index" "makeindex %s"
                'TeX-run-index
                nil
                t
                :help "Run makeindex to create index file")
  (list "Xindy" "texindy %s"
                'TeX-run-command
                nil
                t
                :help "Run xindy to create index file")
  (list "Check" "lacheck %s"
                'TeX-run-compile
                nil
                '(latex-mode)
                :help "Check LaTeX file for correctness")
  (list "ChkTeX" "chktex -v6 %s"
                 'TeX-run-compile
                 nil
                 '(latex-mode)
                 :help "Check LaTeX file for common mistakes")
  (list "Spell" "(TeX-ispell-document \"\")"
                'TeX-run-function
                nil
                t
                :help "Spell-check the document")
  (list "Clean" "TeX-clean"
                'TeX-run-function
                nil
                t
                :help "Delete generated intermediate files")
  (list "Clean All" "(TeX-clean t)"
                    'TeX-run-function
                    nil
                    t
                    :help "Delete generated intermediate and output files")
  (list "Other" ""
                'TeX-run-command
                t
                t
                :help "Run an arbitrary command")))


;;*** Set variables for reftex

(setq reftex-default-bibliography '("/home/dan/library/database/reference.bib")
      reftex-plug-into-AUCTeX t)


(setq-local reftex-cite-format (concat "[" "[cite:%l]" "]"))


;;*** Set variables for viewing


(setq TeX-expand-list
      '(("%(absOUT)"
         (lambda nil
           (prin1-to-string
            (expand-file-name
             (concat (file-name-sans-extension (buffer-file-name))
                     ".pdf"))))))
      TeX-view-program-list
      '(("Evince" "evince --page-index=%(outpage) %o" "evince")
        ("FoxitReader" "foxitreader %(absOUT)" "foxitreader")
        ("XDvi" "xdvi %o" "xdvi")
        ("qpdfview" "qpdfview %o")
        ("Zathura" "zathura %o"))
      TeX-view-program-selection
      '(((output-dvi style-pstricks) "dvips and gv")
        (output-dvi "XDvi")
        (output-pdf "Evince")
        (output-html "xdg-open")))


;;*** Hook

;; A peculiarity of having latex buffer managed by auctex is that
;; font-lock-add-keywords won't work from within the LaTeX-mode-hook. The
;; variable font-lock-keywords is overwritten after running
;; LaTeX-mode-hook. So we need to put font-lock-add-keywords commands in
;; a higher run level. One hook which is run after font-lock-keywords is
;; rewritten happens to be TeX-update-style-hook.

;; Remark: Make sure that in search based fontification the same region
;; is not searched for twice. The first search marks any found keywords
;; as fontified and therefore no second search is done in this region.


(add-hook 'LaTeX-mode-hook
  (lambda ()
    (setq-local TeX-command-default "PdfLatexmk")
    (LaTeX-parse-newlayers)
    (TeX-source-correlate-mode 1)
    (setq-local outline-regexp TeX-outline-regexp)
    (setq-local outline-heading-alist TeX-outline-heading-alist)
    (outline-minor-mode 1)
    (LaTeX-math-mode 1)
    (AUCTeX-dnd-mode 1)
    (outline-hide-body)
    ;; (define-key LaTeX-mode-map "\C-c\C-c" 'LaTeX-change-before-compilation)
    ;; (define-key LaTeX-mode-map "\C-c\C-v" 'TeX-view-other-directory)
    (define-key LaTeX-mode-map "\C-c\C-v" 'TeX-view-manage-windows)
    (define-key LaTeX-mode-map "\C-co" 'buffer-open-file)
    (define-key LaTeX-mode-map "\C-cr" 'insert-tex-label-random)
    (define-key LaTeX-mode-map "\C-cm" 'insert-tex-ref)
    (define-key LaTeX-mode-map "\C-ct" 'insert-tex-curlies)
    (define-key LaTeX-mode-map "\C-cb" 'insert-tex-brackets)
    (define-key LaTeX-mode-map "\C-cd" 'insert-tex-derivative)
    (define-key LaTeX-mode-map "\C-cp"
      'insert-tex-partial-derivative)
    (define-key LaTeX-mode-map "\C-cc" 'insert-tex-command)
    (define-key LaTeX-mode-map "\C-ce" 'insert-tex-texteq)
    (define-key LaTeX-mode-map "\C-cf" 'insert-tex-fraction)
    (define-key LaTeX-mode-map (kbd "C-c <up>")
      'LaTeX-insert-superscript)
    (define-key LaTeX-mode-map (kbd "C-c <down>")
      'LaTeX-insert-subscript)
    (define-key LaTeX-mode-map "\C-cn" 'TeX-assemble-error-help)
    (define-key LaTeX-mode-map "\C-c[" 'reftex-citation)
    (define-key LaTeX-mode-map "\C-c\M-8" 'insert-tex-biblink)
    (define-key LaTeX-mode-map "\C-c\M-9" 'layers-insert-footer)
    (define-key LaTeX-mode-map "\C-ck" 'latex-locate-citation-insert)
    (define-key LaTeX-mode-map "\C-c\M-o"
      'latex-locate-citation-view)
    (define-key LaTeX-mode-map (kbd "<C-mouse-1>") 'TeX-evince-sync-view-mod)
    (define-key LaTeX-mode-map (kbd "<C-S-mouse-4>") 'LaTeX-cycle-bracket-size)
    (font-latex-add-keywords '(("newlayer" "{{{{")) 'function)
    (font-latex-add-keywords '(("layerfooter" "{")) 'function)
    (font-latex-add-keywords '(("layerlabel" "{")) 'function)
    (font-latex-add-keywords '(("hidealllayers" "{")) 'function)
    (font-latex-add-keywords '(("printlayers" "{")) 'function)
    (font-latex-add-keywords '(("biblink" "[{{")) 'function)
    (font-latex-add-keywords '(("hlabel" "{")) 'function)
    (font-latex-add-keywords '(("comment" "{{")) 'function)
    (font-latex-add-keywords '(("hsection" "{")) 'sectioning-1)
    (font-latex-add-keywords '(("hsubsection" "{")) 'sectioning-2)
    (font-latex-add-keywords '(("hsubsubsection" "{"))
                             'sectioning-3)))


(add-hook 'TeX-update-style-hook
  (lambda ()
    (TeX-add-symbols '("hsection" LaTeX-macro-hsection)
                     '("hsubsection" LaTeX-macro-hsection)
                     '("hsubsubsection" LaTeX-macro-hsection))
    (font-lock-add-keywords nil
      '(;; (hyperlink-fontify-button 0 nil append t)
        ("\\(\\\\,\\)" 1 font-lock-keyword-face t)
        ( LaTeX-fontify-environments ( 0 nil append t))
        ;; ( LaTeX-fontify-keywords-inside ( 0 nil append t))
        ( LaTeX-fontify-level-indentation ( 0 nil append t))))))


(add-hook 'TeX-after-compilation-finished-functions
          'LaTeX-change-after-compilation)



(provide 'latex-custom)

;;; latex-custom.el ends here
