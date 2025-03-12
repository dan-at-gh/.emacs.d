;;; doxy --- Literate programming with doxy

;;; Commentary:

;;; Code:



;;* Define variables


(defvar doxy-file ""
  "Set this variable to the doxygen project name.
The configuration file for doxygen is then \\'doxy-file.doxy\\' (the
extension \\'.doxy\\' is added to the project name by default).")
(make-variable-buffer-local 'doxy-file)
(put 'doxy-file 'safe-local-variable #'stringp)


(defvar doxy-page-file-patterns '("md" "html" "dox")
  "Valid file patterns for pure documentation files (no source code files).")


(require 'tex)
(require 'latex)
(setq TeX-parse-self t)
(defun doxy-latex-environment-list ()
  (with-current-buffer
      (get-buffer-create " *doxy-latex-environment-list*")
    (erase-buffer)
    (insert "\\documentclass{article}
\\usepackage{amsmath}
\\begin{document}
\\end{document}")
    (latex-mode)
    (TeX-auto-parse)
    (LaTeX-environment-list-filtered)))


(defvar doxy-common-mode-map
  (let (( map (make-sparse-keymap))
        ( menu-entry-compile '(menu-item "Compile Doxygen"
                                         doxy-compile-buffer
                                         :help
"Compile project.
Configuration file should be specified as local variable,
doxy-file: \"~/path/to/project-configuration.doxy\"."))
        ( menu-entry-doxyfile '(menu-item "Set Doxyfile Local Variable"
                                          doxy-set-doxyfile
                                          :help
"Store name of doxygen config file in buffer local variable.")))
    (define-key map "\C-cd" 'doxy-compile-buffer)
    (define-key map "\C-cv" 'doxy-view)
    (define-key map "\C-cs" 'doxy-section)
    (define-key map "\C-c[" 'reftex-citation)
    ;; (define-key map (kbd "C-c <right>") 'doxy-block-indent-more)
    ;; (define-key map (kbd "C-c <left>") 'doxy-block-indent-less)
    (define-key map "\C-c\C-e" 'doxy-insert-environment)
    (define-key map (kbd "<mouse-2>") 'doxy-paste-primary-selection)
    (define-key map (kbd "C-y") 'doxy-yank)
;; Bottom of Menu
    (define-key map [menu-bar doxy doxy-compile-buffer]
                    menu-entry-compile)
    (define-key map [menu-bar doxy doxy-set-doxyfile]
                    menu-entry-doxyfile)
    (define-key map [menu-bar doxy sep-compile] '(menu-item "--"))
    (define-key map [menu-bar doxy-minor doxy-minor-compile-buffer]
                    menu-entry-compile)
    (define-key map [menu-bar doxy-minor doxy-minor-set-doxyfile]
                    menu-entry-doxyfile)
    (define-key map [menu-bar doxy-minor sep-minor-compile]
                    '(menu-item "--"))
;; Top of Menu
     map))


(require 'sgml-mode)
(defvar doxy-mode-map
  (let (( map (make-sparse-keymap)))
    (require 'sgml-mode)
    (require 'markdown-mode)
    (set-keymap-parent map (make-composed-keymap
              (list doxy-common-mode-map
                    markdown-mode-map
                    html-mode-map)))
    (define-key map [menu-bar doxy]
      (cons "Doxy" (make-sparse-keymap)))
    (define-key map [menu-bar doxy doxy-section]
        '(menu-item "Insert section command" doxy-section
           :help "Insert sectioning command."))
    (define-key map [menu-bar doxy doxy-formula-submenu]
      (cons "Formula" (make-sparse-keymap)))
        (define-key map [menu-bar doxy doxy-formula-submenu equation]
          '(menu-item "equation" doxy-equation-formula
            :help "LaTeX equation"))
        (define-key map [menu-bar doxy doxy-formula-submenu align]
          '(menu-item "align" doxy-align-formula
            :help "LaTeX aligned equations"))
        (define-key map [menu-bar doxy doxy-formula-submenu align-star]
          '(menu-item "align*" doxy-align-starred-formula
            :help "LaTeX unnumbered aligned equations"))
        (define-key map [menu-bar doxy doxy-formula-submenu unnumbered]
          '(menu-item "unnumbered" doxy-unnumbered-formula
            :help "LaTeX unnumbered formula"))
        (define-key map [menu-bar doxy doxy-formula-submenu in-text]
          '(menu-item "In-Text" doxy-in-text-formula
            :help "LaTeX in-text formula"))
    (define-key map [menu-bar doxy doxy-page-submenu]
      (cons "Page and subpage" (make-sparse-keymap)))
        (define-key map [menu-bar doxy doxy-page-submenu doxy-subpage]
          '(menu-item "Subpage" doxy-in-text-formula
            :help "Insert LaTeX in-text formula"))
        (define-key map [menu-bar doxy doxy-page-submenu doxy-page]
          '(menu-item "Page" doxy-in-text-formula
            :help "Insert LaTeX in-text formula"))
        (define-key map [menu-bar doxy doxy-page-submenu doxy-mainpage]
          '(menu-item "Mainpage" doxy-in-text-formula
            :help "Insert LaTeX in-text formula"))
    (define-key map [menu-bar doxy doxy-latex-submenu]
      (cons "LaTeX environment" (make-sparse-keymap)))
      (dolist ( env (reverse
                      (mapcar 'car (doxy-latex-environment-list))))
        (define-key map
          (vector 'menu-bar 'doxy 'doxy-latex-submenu
           (make-symbol (concat "doxy-latex-environment-" env)))
          `(menu-item ,env (lambda nil (interactive) (doxy-menu-filter ,env))
            :help "Insert LaTeX environment.")))
    (define-key map [menu-bar doxy insert-example-template]
      '(menu-item "Verbatim" insert-example-template
        :help "Insert example box at point."))
    ; Remove all remaps inherited from markdown-mode-map
    (define-key map [remap narrow-to-page] nil)
    (define-key map [remap mark-page] nil)
    (define-key map [remap forward-page] nil)
    (define-key map [remap backward-page] nil)
    (define-key map [remap mark-paragraph] nil)
    (define-key map [remap forward-paragraph] nil)
    (define-key map [remap backward-paragraph] nil)
    map))


(defun doxy-menu-filter ( real-binding)
  "Menu filter function.

REAL-BINDING argument."
  (doxy-insert-latex-environment real-binding))


(defvar doxy-minor-mode-map
  (let (( map (make-sparse-keymap)))
    (set-keymap-parent map doxy-common-mode-map)
    (define-key map [menu-bar doxy-minor]
      (cons "Doxy-Minor" (make-sparse-keymap)))
     map))


(defvar doxy-paragraph-start
  (concat "\s*\\(?:-+\\|\\*+\\|(?[0-9]+[).]\\|\\\\"
          "\\(?:arg\\|argvar\\|param\\(?:\\[[a-z,]+\\]\\)?\\|"
               "retval\\|exception\\)"
          "\\)\\(?:\s\\|$\\)"))


(defvar doxy-paragraph-separate
  (concat "\s*$\\|"
          ".\\+BEGIN_DOXY\\|.\\+END_DOXY\\|"
          "\\\\\\(?:end[a-z]*\\|code\\|verbatim\\|par\\|"
                    "parameters\\|funcresult\\|section\\|subsection\\|"
                    "f{[a-z*]+}{\\|f}\\|f\\[\\|f\\]\\|snippet\\)"
          "\\(?:\s\\|$\\)"))


;;*** Font definitions

(defface font-lock-doxy-comment-face
  '((((type x pc w32) (background light))
     :background "#EEE2FF")
    (((type x pc w32) (background dark))
     (:background "#5e5e5e" :inherit font-lock-comment-face))
    (((type tty))
     :foreground "blue"))
  "Face used to display Doxygen comments (light gray).
       :foreground gray25 "
    :group 'basic-faces)
(defface font-lock-doxy-begin-face
  '((((type x w32 mac) (background light))
     (:foreground "#555555" :background "#E2E1D5" :underline "#A7A6AA"
      :height 90))
     (((type x pc w32) (background dark))
      (:background "#27535b" :inherit font-lock-comment-face))
    (((type tty))
     (:foreground "blue")))
    "Face used to display Doxygen comments (light gray)."
    :group 'basic-faces)


(defvar font-lock-doxy-comment-face 'font-lock-doxy-comment-face)
(defvar font-lock-doxy-begin-face 'font-lock-doxy-begin-face)


;;*** Function for fontification

(defun doxy-fontify-command-paragraph ( limit)
  (when (re-search-forward (concat
          "\\(\\\\\\(brief\\|details\\|param\\|par\\|author\\)\\)"
          "\\(\\[[inout,]+\\]\\)?\s+\\([a-z0-9]+\\)?")
          limit t)
    (let (( beg-0 (match-beginning 0))
          ( end-0 (match-end 0)))
      (put-text-property beg-0 end-0
                         'face 'font-lock-function-name-face)
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'font-lock-keyword-face)
      (cond ((string= (match-string 2) "param")
             (when (match-string 3)
               (put-text-property (match-beginning 3) (match-end 3)
                                  'face 'font-lock-variable-name-face))
             (when (match-string 4)
               (put-text-property (match-beginning 4) (match-end 4)
                                  'face 'font-lock-constant-face)))
            ((string= (match-string 2) "par")
             (when (match-string 4)
               (put-text-property (match-beginning 4) (match-end 4)
                                  'face 'font-lock-constant-face))
             (when (match-string 5)
               (put-text-property (match-beginning 5) (match-end 5)
                                  'face 'font-lock-constant-face)))
            ((string= (match-string 2) "file")
             ))
      (when (re-search-forward "^\s*$\\|\\\\par\\|\\'" nil t)
        (let (( separate-beg (match-beginning 0)))
          (add-text-properties beg-0 separate-beg
                               '( font-lock-multiline t))
          (put-text-property end-0 separate-beg
                             'face 'font-lock-function-name-face)
          (goto-char end-0))))
    t))


(defun doxy-fontify-file-keyword ( limit)
  (when (re-search-forward (concat
          "\\(\\\\\\(file\\)\\)\s*"
          "\\([^\s\n]+\\)?\s*\\(\n\s*[^\n\s].*\\)?")
          limit t)
    (let (( beg-0 (match-beginning 0))
          ( end-0 (match-end 0)))
      (put-text-property beg-0 end-0
                         'face 'font-lock-function-name-face)
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'font-lock-keyword-face)
      (when (match-string 3)
        (add-text-properties (match-beginning 3) (match-end 3)
                                  '( face font-lock-variable-name-face
                                     help-echo "File name")))
      (when (match-string 4)
        (add-text-properties (match-beginning 4) (match-end 4)
                           '( face font-lock-constant-face
                              help-echo "Brief file description line")))
      (when (re-search-forward "^\s*$\\|\\\\par\\|\\'" nil t)
        (let (( separate-beg (match-beginning 0)))
          (add-text-properties beg-0 separate-beg
                               '( font-lock-multiline t))
          (add-text-properties end-0 separate-beg
                             '( face font-lock-function-name-face
                                help-echo "Detailed file description paragraph")))))
    t))


(defun doxy-fontify-environment ( limit)
  (when (re-search-forward
               "\\\\\\(verbatim\\|code\\)"
            limit t)
    (let* (( beg (match-beginning 0))
           ( end (match-end 0))
           ( match (match-string 0))
           ( cmd (match-string 1))
           ( cmd-end-re (concat "\\\\end" cmd))
           ( doxy-block (get-text-property (match-beginning 0)
                                           'doxy-block))
           ( file-ext (file-name-extension (buffer-name))))
      (when (or doxy-block
                (member file-ext doxy-page-file-patterns))
        (let* (( bg (if doxy-block
                        (face-attribute 'font-lock-doxy-comment-face
                                      :background)
                        "white"))
               ( fgFunction (face-attribute
                             'font-lock-function-name-face
                             :foreground))
               ( fgKeyword (face-attribute
                             'font-lock-keyword-face
                             :foreground))
               ( pos 0) ( bufBeg 0) ( found 0))
             (put-text-property beg end
                'face `(:foreground ,fgKeyword :background ,bg))
             (save-match-data
               (re-search-forward cmd-end-re nil t)
               (put-text-property (match-beginning 0) (match-end 0)
                'face `(:foreground ,fgKeyword :background ,bg))
               (put-text-property end (match-beginning 0)
                'face `(:foreground "SaddleBrown" :background ,bg))))))
    t))


(defun doxy-fontify-latex-environment ( limit)
  (when (re-search-forward "\\(\\\\f\\)\\({\\([a-z]+\\*?\\)}{\s*\n\\|\\$\\)"
                           limit t)
    (let (( beg (match-beginning 0))
          ( end (match-end 0))
          ( env (match-string 3))
          ( footer-re "\\\\f}"))
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'font-lock-keyword-face)
      (if env
          (put-text-property (match-beginning 3) (match-end 3)
                             'face 'font-lock-function-name-face)
        (put-text-property (1- end) end
                             'face 'font-lock-keyword-face)
        (setq footer-re "\\\\f\\$"))
      (when (re-search-forward footer-re nil t)
        (let (( footer-beg (match-beginning 0))
              ( footer-end (match-end 0))
              ( doxy-latex-map (copy-keymap doxy-common-mode-map)))
          (set-keymap-parent doxy-latex-map LaTeX-mode-map)
          (put-text-property footer-beg
                             (if env (1- footer-end) footer-end)
                             'face 'font-lock-keyword-face)
          (put-text-property beg footer-end 'font-lock-multiline t)
          (doxy-highlight-derived end footer-beg "LATEX" env)
          (remove-text-properties end footer-beg
                                  '( keymap nil))
          (put-text-property end footer-beg
                             'keymap doxy-latex-map))))
    t))


(defun doxy-include-formula ( string &optional env)
  "Insert the associated LaTeX-environment to the given doxygen
keyword sequence.

In order to highlight doxygen comment parts by major-mode
fontification properties, it is necessary to place the source
text in the appropriate context or environment. For this purpose,
the text and coresponding context or environment is inserted into
a temporary buffer."
  (let (( header (concat "\\usepackage{amsmath}"
                           "\\usepackage{bm}"))
        ( footer ""))
    (if env
        (setq header (concat header
                             "\\begin{" env "}")
              footer (concat "\\end{" env "} "))
      (setq header (concat header " $")
            footer "$dfd"))
    (insert header string footer)
    (length header)))


(defun doxy-highlight-derived ( start end &optional key env)
  "Inspired by `org-src-font-lock-fontify-block' from org-src.el."
  (save-match-data ; we need this to prevent jit-lock errors
    (let (( string (buffer-substring-no-properties start end))
          ( target (current-buffer))
          ( parent-mode major-mode)
          ( len (- end start))
          ( pos0 1)
          ( message-log-max nil)
          ( inhibit-message t))
      (with-current-buffer
        (get-buffer-create " *doxy-highlight-substring*")
        (erase-buffer)
        (cond ((string= key "LATEX")
               (setq pos0 (doxy-include-formula string env))
               (latex-mode)
               (TeX-update-style t))
              ((string= key "HTML")
               (insert string " ")
               (html-mode))
              (t
               (insert string " ")
               (funcall parent-mode)))
        (font-lock-ensure)
        (let (( target-beg (- start pos0 1))
              ( pos pos0)
              ( pos1 (point-max))
                next target-next prop value)
          (while (and (setq next (next-property-change pos))
                      (<= next pos1))
            (if (> (- next pos0) len)
                (setq target-next (+ pos0 len 1))
              (setq target-next next))
            (dolist ( prop (cons 'face font-lock-extra-managed-props))
              (setq value (get-text-property pos prop))
              (when (< pos target-next)
                (put-text-property (+ target-beg pos)
                                   (+ target-beg target-next)
                                   prop value target)))
            (setq pos next)))))))


;;*** Function for manipulating and pasting text

(defun doxy-paste-primary-selection ( event)
  (interactive "e")
  (let (( beg (posn-point (event-end event))))
    (mouse-yank-primary event)
    (let (( indent-str (get-text-property (point) 'line-prefix)))
      (when indent-str
        (add-text-properties beg (point)
                             `( line-prefix ,indent-str))))))

(defun doxy-yank ( &optional arg)
  (interactive)
  (let (( beg (point)))
    (yank arg)
    (let (( indent-str (get-text-property (point) 'line-prefix)))
      (when indent-str
        (add-text-properties beg (point)
                           `( line-prefix ,indent-str)))))
  (message "custom yanked"))


;;*** Compiling doxygen


(defun doxy-new-doxyfile ()
  (interactive)
  (let* (( default-dir (concat (file-name-as-directory (getenv "HOME"))
                              (file-name-as-directory "doxygen")))
         ( template (concat default-dir ".Doxyfile-no-comment"))
         ( new (expand-file-name (read-file-name
                  "New Doxyfile Name: "
                 default-dir nil nil nil))))
    (if (file-exists-p new)
      (message "File already exists.")
      (copy-file template new)
      (find-file-other-window new))))


(defun doxy-set-doxyfile ()
  (interactive)
  (let* (( default (file-name-as-directory (getenv "HOME")))
         ( new (expand-file-name (read-file-name
                 (format "Set Doxyfile: "
                   (file-name-nondirectory default))
                 default nil nil nil))))
  (add-file-local-variable 'doxy-file new)
  (set-buffer-modified-p t)
  (save-buffer)
  (revert-buffer t t nil)))


(defun doxy-config-file ()
  (let* (( default-dir (file-name-as-directory (getenv "HOME")))
         ( conf-file (if (local-variable-p 'doxy-file)
                         (expand-file-name doxy-file)
                         (expand-file-name (read-file-name
                            (format "Doxyfile: "
                              (file-name-nondirectory default-dir))
                            default-dir nil nil nil)))))
    (when (file-exists-p conf-file)
          conf-file)))


(defun doxy-compile-buffer ()
  (interactive)
  (save-buffer)
  (let (( conf-file (doxy-config-file)))
    (if conf-file
        (async-shell-command (concat "doxygen-latex2image " conf-file))
      (message "Doxygen configuration file \"%s\" not found"
               conf-file))))


;;*** Function viewing html and pdf

;; In analogy to latex view command provided by auctex (C-cC-v) doxy-mode
;; ships with two viewing commands for html and pdf output.

(defun doxy-parse-config-file ( field)
  (let (( conf-file (doxy-config-file)))
    (with-temp-buffer
      (insert-file-contents conf-file)
      (goto-char (point-min))
      (re-search-forward
        (concat "^" field
         "\s*=\s*\\("
                  "\\(?:"
                    "\\(?:.\\|\\\\\n\\)*?"
                     "[^\\\\]\\)$"
                      "\\|$<\\)")
        (point-max) t)
      (replace-regexp-in-string "\s*\\\\\s*\n\s*\\|\s+" " "
        (string-trim (match-string 1))))))


(defun doxy-view-html ()
  (interactive)
  (let (( conf-file (doxy-config-file)))
    (if conf-file
      (let* (( conf-dir (file-name-directory conf-file)) ; with trailing /
             ( out-dir (doxy-parse-config-file "OUTPUT_DIRECTORY"))
             ( base-name (file-name-nondirectory out-dir))
             ( user (file-name-as-directory (getenv "USER")))
             ( home (file-name-as-directory (getenv "HOME")))
             ( url (concat "http://localhost/~" user base-name))
             ( path (concat home
                            (file-name-as-directory "public_html")
                            base-name))
             ( out (concat (file-name-as-directory
                            (if (file-name-absolute-p out-dir)
                                out-dir
                              (expand-file-name out-dir conf-dir)))
                           "html")))
        (when (and (not (file-exists-p path)) (file-exists-p out))
          (make-symbolic-link out path))
        (shell-command (concat "firefox " url)))
      (message "Doxygen configuration file \"%s\" not found"
               conf-file))))


(defun doxy-view-pdf ()
  "Modification of the TeX-view command from auctex.
Append absolute path to output and out folder to output file name.
i3 window manager: close other windows when emacs occupies whole display width.
Quick View menu item still is not working."
  (interactive)
  ; choose output-pdf from TeX-view-program-selection
  (setq TeX-output-extension "pdf")
  (let (( conf-file (doxy-config-file)))
    (if conf-file
        (let* (( conf-dir (file-name-directory conf-file))
               ( out-dir (doxy-parse-config-file "OUTPUT_DIRECTORY"))
               ( out-file (concat (file-name-as-directory
                                   (if (file-name-absolute-p out-dir)
                                       out-dir
                                     (expand-file-name out-dir conf-dir)))
                                  (file-name-as-directory "latex")
                                  "refman." TeX-output-extension)))
          (if (file-exists-p out-file)
              (progn
                (crowded-close-others)
                (TeX-command "View"
                             '(lambda ( &optional extension nondirectory) out-file)
                             -1))
            (message "Output file %S does not exist." out-file)))
      (message "Doxygen configuration file \"%s\" not found"
               conf-file))))


(defun doxy-view ()
  (interactive)
  (doxy-view-html)
  (doxy-view-pdf))


;;*** Function inserting blocks and templates


(defun region-min-indent ( beg end)
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let (( min-indent 10000) indent)
      (while (<= (line-number-at-pos) (line-number-at-pos end))
         (looking-at "\s*")
         (setq indent (length (match-string 0)))
         (when (< indent min-indent)
           (setq min-indent indent))
         (beginning-of-line 2))
      min-indent)))


(defun insert-google-template ()
  (interactive)
  (let (( text
"\\brief 

\\param
\\return
\\exception"))
    (insert text)
    (re-search-forward "brief" nil t -1)
    (end-of-line)))


(defun insert-example-template ()
  (interactive)
  (let (( text
"Example:
\\verbatim
in: 
out:
\\endverbatim"))
    (insert text)
    (re-search-forward "in:" nil t -1)
    (end-of-line)))


(defun line-beginning-at-pos ( pos &optional dwim)
  (save-excursion
    (goto-char pos)
    (if (not dwim)
      (line-beginning-position)
      (beginning-of-line)
      (if (re-search-forward "[^\s]" pos t)
        (line-beginning-position)
        (line-beginning-position 0)))))


(defun doxy-insert-environment ( beg end)
  (interactive "r")
  (let* (( cmd (completing-read "Command: "
                    '("code" "verbatim")
                    nil nil ""))
         ( beg0 (line-beginning-at-pos beg))
         ( end0 (line-beginning-at-pos end t)))
     (goto-char end0)
     (beginning-of-line 2)
     (insert (format "\\end%s\n" cmd))
     (goto-char beg0)
     (insert (format "\\%s\n" cmd))))


(defun doxy-align-starred-formula ()
  (interactive)
  (insert
"\\f{align*}{

\\f}")
  (end-of-line 0))


(defun doxy-align-formula ()
  (interactive)
  (insert
"\\f{align}{

\\f}")
  (end-of-line 0))


(defun doxy-equation-formula ()
  (interactive)
  (insert
"\\f{equation}{

\\f}")
  (end-of-line 0))


(defun doxy-unnumbered-formula ()
  (interactive)
  (insert
"\\f[

\\f]")
  (end-of-line 0))


(defun doxy-in-text-formula ()
  (interactive)
  (insert
"\\f$\\f$")
  (forward-char -3))


(defun doxy-point-inside-block-p ()
  (let* (( beginInfo (save-excursion (doxy-begin)))
         ( end (1- (cadr (doxy-end))))
         ( begin (if (string= (car beginInfo) "")
                     (caddr beginInfo)
                     (1+ end))))
    (if (and (<= begin (point)) (<= (point) end ))
        (list begin end)
        (list nil nil))))


(defun doxy-insert-latex-environment ( env)
  (insert (format "\\f{%s}{\n\n\\f}" env))
  (beginning-of-line 0))


(defun doxy-latex-environment ()
  (interactive)
  (let (( env (completing-read "Insert Environment: "
                               (doxy-latex-environment-list) nil nil "")))
    (doxy-insert-latex-environment env)))


(defun doxy-section ()
  (interactive)
  (let (( name (random-string))
        ( section (completing-read "Section command: "
                   '( "page" "section"
                      "subsection" "subsubsection") nil nil "")))
    (when (string= section "page")
      (setq name (read-string "(HTML file) Name: ")))
    (insert (format "\\%s %s " section name))))


;;*** Choose modes

(add-to-list 'auto-mode-alist '("Doxyfile" . conf-unix-mode))
(add-to-list 'magic-mode-alist '("# Doxyfile" . conf-unix-mode))

(add-to-list 'auto-mode-alist '("\\.dox\\'" . doxy-mode))
(add-to-list 'magic-mode-alist '("<!-- DOXY" . doxy-mode))


;;*** Minor mode definition
 
(define-minor-mode doxy-minor-mode
  "This is a minor mode for Doxygen documentation text included
   in the source code."
  :lighter " dxy"
  :keymap doxy-minor-mode-map
  (if doxy-minor-mode
      (progn
        ;; Enable doxy-minor-mode
        (message "doxy enable")
        (font-lock-add-keywords nil
          `((,(concat "^\\(" (string-trim comment-start) "[#>]\\)\\(.*\n\\)")
             (1 font-lock-doxy-begin-face t) (2 srcdoc-block t))))
        (setq-local cmt-block-paragraph-start nil)
        (setq-local cmt-block-paragraph-separate "\s*\\\\file"))
      ;; Disabling doxy-minor-mode
      (font-lock-remove-keywords nil
        `((,(concat "^\\(" (string-trim comment-start) "[#>]\\)\\(.*\n\\)")
           (1 font-lock-doxy-begin-face t) (2 srcdoc-block t)))))
    (font-lock-flush))


;;*** Major mode definition

(define-derived-mode doxy-mode html-mode "Doxy"
  "Major mode for doxygen page source files."
  (setq-local case-fold-search nil)
  (setq-local font-lock-extra-managed-props
              '(font-lock-multiline keymap latex-env))
  (font-lock-add-keywords nil
                          `((,(concat
;; command name line
"\\(\\\\\\(snippet\\|page\\|section\\|subsection\\|defgroup\\)\\)"
"\s+\\([a-z0-9/]+\\)\s+\\(.*\\)")
                             (1 'font-lock-keyword-face)
                             (3 'font-lock-constant-face)
                             (4 'font-lock-function-name-face))
                            (,(concat
;; command
"\\\\\\(tableofcontents\\|parameters\\|argvar\\|arg\\|return"
"\\|funcresult\\)\s\\|\\[TOC\\]")
                             0 'font-lock-keyword-face)
                            (,(concat
;; command word
"\\(\\\\\\(retval\\|c\\|e\\|em\\|cite\\|b\\|exception\\)\\)"
"\s+\\([^\s\n]*\\)")
                             (1 'font-lock-keyword-face)
                             (3 'font-lock-constant-face))
                            (,(concat
;; command line
"\\(\\\\\\(ingroup\\|mainpage\\)\\)"
"\s+\\([^\n]*\\)")
                             (1 'font-lock-keyword-face)
                             (3 'font-lock-constant-face))
                            ;; Multiline fontifications
                            (doxy-fontify-file-keyword 0 nil append t)
                            (doxy-fontify-command-paragraph 0 nil append t)
                            (doxy-fontify-latex-environment 0 nil append t)))
  (kill-local-variable 'paragraph-start)
  (kill-local-variable 'paragraph-separate)
  (setq-local paragraph-start doxy-paragraph-start)
  (setq-local paragraph-separate doxy-paragraph-separate))


(provide 'doxy)

;;; doxy.el ends here
