;;; emacs --- Summary

;;; Commentary:


;;* Introduction for buffer "emacs.org"


;;** General information

;; This is a user configuration file for the Emacs editor.  The
;; default file would be "~/.emacs" on linux systems.  In spite of
;; using elisp (emacs-lisp dialect) for the entire configuration file,
;; org-mode provides a convenient way to organize extensive
;; configurations and Emacs programming.  In order to replace the
;; default elisp file "~/.emacs" by a *.org file, first it is
;; necessary to create a file for initialization, in this case called
;; init.el.  #+begin_example (require 'org) (org-babel-load-file
;; (expand-file-name "emacs.org" user-emacs-directory)) #+end_example
;; But before diving into emacs.org, it is recommended to keep
;; customize settings in their own file #+begin_example (setq
;; custom-file "~/.emacs.d/custom.el") (when (file-exists-p
;; custom-file) (load custom-file)) #+end_example


;;** Mime types and applications

;; - System defaults: /usr/share/applications/defaults.list
;; - User customization: ~/.config/mimeapps.list


;;* Settings main section

;; This is the main settings section where mode independent customization
;; resides.


;;; Code:


;;** Package, require, server start, pixel scrolling


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
 ;; (add-to-list 'package-archives
 ;;                 '("melpa" . "http://melpa.milkbox.net/packages/"))
 ;; (setq package-archives
 ;;    '(("gnu" . "http://elpa.gnu.org/packages/")
 ;;     ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(require 'desktop)

;; Bug string-trim is undefined
(require 'subr-x)

;; Bug dead-key is undefined
(require 'iso-transl)

;; Put here custom packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

(and window-system (server-start))


;; (pixel-scroll-precision-mode 1)


;;** Put enable/disable commands


(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;;** Function general


(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (buffer-modified-p)))
        (normal-mode)
        (revert-buffer t t nil))))
  (message "Refreshed open files."))


(defun update-buffer-file-names ()
  "Check for changed filenames outside of Emacs."
  (interactive)
  (let (( home "~/"))
    (dolist ( buffer (buffer-list))
      (let (( file (buffer-file-name buffer)))
        (when (and file
                   (not (file-exists-p file)))
          (unless
              (y-or-n-p
               (format "Kill buffer visiting nonexistent file %s? "
                       file))
            (find-file-noselect
             (expand-file-name
              (read-file-name "Replace with "
                              (file-name-directory file)
                              nil 'file-must-exists
                              (file-name-nondirectory file)))))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))


(defun insert-at ( pos text)
  "Insert TEXT at POS."
  (goto-char pos)
  (insert text))


(defun get-string-from-file ( file-path)
  "Return FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))


(defun read-lines ( filepath &optional omit-nulls)
  "Return a list of lines of a file at FILEPATH.

OMIT-NULLS is passed to 'split-string`."
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string (buffer-string) "\n" omit-nulls)))


(defun global-disable-mode (mode-fn)
  "Disable `MODE-FN' in ALL buffers."
  (interactive "a")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall mode-fn -1))))


(defun minibuffer-msg (msg-str)
  "Display MSG-STR only in minibuffer, not in *Message* buffer."
  (let ((message-log-max nil))
       (message msg-str)))


(defun execute-without-undo ()
  "Execute without undo."
  (let (save (copy-tree buffer-undo-list))
    (setq buffer-undo-list t)
    (setq buffer-undo-list (copy-tree save))))


(defun current-line-empty-p ()
  "Return non-nil, if line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))


(defun insert-random-string ()
  "Insert a random alphanumerics string of length 10."
  (interactive)
  (let ((mycharset "1234567890abcdefghijklmnopqrstyvwxyz"))
    (dotimes (i 10)
      (insert (elt mycharset (random (length mycharset)))))))


(defun random-string ( &optional len alpha)
  "Insert a random alphanumerics string of specified length.

Args:
  LEN (optional): Length of random string, default: 10
  ALPHA (optional): Random string only contains alphabetic characters.
Returns:
  Random string"
  (let (( mycharset (if alpha
                        "abcdefghijklmnopqrstyvwxyz"
                        "1234567890abcdefghijklmnopqrstyvwxyz"))
        ( str ""))
    (unless len (setq len 10))
    (dotimes ( i len)
      (setq str
        (concat str (string
                (elt mycharset (random (length mycharset)))))))
    str))


(defun remove-regexp-chars ( str)
  "Take STR and remove all special characters."
  (replace-regexp-in-string (concat "\\^" "\\|"
                                    "\\\\" "\\|"
                                    "(" "\\|"
                                    ")")
                            "" str))


(defun ramp ( x)
  "Ramp function with argument X."
  (if (< x 0) 0 x))


(defun mouse-wheel-time-up ()
  "Control timestamp by mouse wheel."
  (interactive)
  (when (org-at-timestamp-p t)
        (org-timestamp-up-day)))


(defun mouse-wheel-time-down ()
  "Control timestamp by mouse wheel."
  (interactive)
  (when (org-at-timestamp-p t)
        (org-timestamp-down-day)))


(defun e/mwheel-scroll-left ()
  (interactive)
  (scroll-left 5))


(defun e/mwheel-scroll-right ()
  (interactive)
  (scroll-right 5))


(defun message-time-difference ( comment start)
  "Print a message about elapsed time.

Provide a COMMENT string and START time."
  (let (( elapsed (float-time
                    (time-subtract (current-time) start))))
       (message "%s (%.6fs)" comment elapsed))
  (current-time))


(defun find-nonascii-char ()
  "Scan whole buffer for nonascii characters."
  (interactive)
  (let (( pos (point)))
    (goto-char (point-min))
    (if (re-search-forward "[[:nonascii:]]" (point-max) t)
      (point)
      (progn
        (goto-char pos)
        nil))))


(defun fill-to-width ()
  (interactive)
  (delete-region (point) (line-end-position))
  (insert (make-string (ramp (- 70 (current-column)))
    (char-before))))


(defun shorten-file-name ( path)
  (let (( rel (file-relative-name path))
        ( abb (abbreviate-file-name path))
        ( home (file-name-as-directory
                (expand-file-name (getenv "HOME")))))
    (string-match "[/.]*" rel)
    (if (string=
         home
         (file-name-as-directory (expand-file-name
                                  (concat default-directory
                                          (match-string 0 rel)))))
        abb
      rel)))


(defun paste-word-or-region ()
  "Prepare the text at point for enclosing operations.

Two cases are considered:
With active region: Cut the region.
With point after word: Cut the word.

In both cases the removed text is returned in order to process
and paste it."
  (let (( pos1 (point))
        ( pos2 (point)))
       (if (use-region-p)
           (setq pos1 (region-beginning)
                 pos2 (region-end))
           (when (re-search-forward "[\s\n]\\([^\s\n]*\\)" (point-min) t -1)
             (setq pos1 (match-beginning 1)
                   pos2 (match-end 1))
             (forward-char)))
       (delete-and-extract-region pos1 pos2)))


(defun select-region ()
  (interactive)
  (let* (( beg-end-str (read-string "Input-Format BEGINNING END: "))
         ( beg-end-list (delete "" (split-string beg-end-str))))
    (set-mark (string-to-number (pop beg-end-list)))
    (goto-char (string-to-number (pop beg-end-list)))
    (activate-mark)))


(defun fill-paragraph-left ()
  (interactive)
  (forward-paragraph)
  (let (( end (line-end-position 0))
        ( indent (get-text-property (point) 'line-prefix)))
    (backward-paragraph)
    (let (( beg (point)))
      (while (re-search-forward "[^\s].*\\(\n\\)" end t)
        (replace-match " " t nil nil 1))
      (fill-paragraph)
      (forward-paragraph)
      (add-text-properties beg (point) `( line-prefix ,indent)))))


(defun delete-overlays-in-region ( beg end)
  (interactive "r")
  (dolist ( overlay (overlays-in beg end))
    (when (overlay-get overlay 'invisible)
      (delete-overlay overlay))))


(defun delete-overlays-at-point ()
  (dolist ( overlay (overlays-at (point)))
    (when (overlay-get overlay 'invisible)
      (delete-overlay overlay))))


(defun buffer-path-to-clipboard ()
  "Place the path of the current buffer into the primary
selection.

The primary selection is the system wide selection for mouse
middle button pasting."
  (interactive)
  (let ((select-enable-primary t))
    (kill-new (buffer-file-name) t)))


(defun library-directory ( library)
  "Find the directory of the library source file.

The LIBRARY is the string version of the symbol given to the
`require' function. That means, the library directory should be
added to the `load-path' variable. Internally `find-library-name'
is used to find the full path of the library."
  (file-name-directory (find-library-name library)))


(defun set-original-value (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))


(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))


(defun e/region-remove-spaces ()
  (interactive)
  (when (region-active-p)
    (replace-regexp "\s" "" nil
                    (region-beginning)
                    (region-end))))


;;** Function for mouse click position


(defmacro with-mouse-click-position ( event &rest body)
  "Execute body at mouse click position without moving point."
  `(let (( window (posn-window (event-end ,event)))
         ( pos (posn-point (event-end ,event))))
     (with-current-buffer (window-buffer window)
           (goto-char pos)
           ,@body)))


(defun point-set-to-mouse ( event)
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (mouse-set-point event))


(defun goto-mouse-click-position ( event)
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event))))



(defun mouse-goto-bol (click)
  "Move to beginning of line for mouse-1 click in left fringe."
  (interactive "e")
  (mouse-goto-line click 'left))


(defun mouse-goto-eol (click)
  "Move to beginning of line for mouse-1 click in left fringe."
  (interactive "e")
  (mouse-goto-line click 'right))


(defun mouse-goto-line (click left/right)
  "Helper for `mouse-goto-(bol|eol)'."
  (let* ((posn      (event-start click))
         (click-pt  (posn-point posn))
         (window    (posn-window posn))
         (buf       (window-buffer window))
         (clicks    (if (eq mouse-selection-click-count-buffer buf)
                        (event-click-count click)
                      0)))
    (when (= clicks 1)                  ; No-op if not single-click.
      (with-current-buffer buf
        (goto-char click-pt)
        (if (eq 'left left/right)
            (line-beginning-position)
          (line-end-position))))))


;;** Function for hyperlink file buttons

;; Order of fontification:
;;                    (subexp facename                 override laxmatch)
;; regexp or function (0      nil                      append   t)
;; regexp or function (0      (outline-font-lock-face) nil      t)
;;                                                     t override existing
;;                                                     keep keep existing
;;                                                     prepend prefer new (beginning of prop list)
;;                                                     append prefer existing (end of prop list)


(defvar hyperlink-default-paths
  '("~/dev/codeaster/src-micropolar/catalo/cataelem/Commons"
    "~/jekyll"))


(defvar hyperlink-exclude-buffer-names
  '("*Ibuffer*" "*Obuffer*"))


(defun find-closing-position ( open-char)
  (let (( close-char (cond ((string= open-char "(") ")")
                           ((string= open-char "[") "]")))
        ( i 1))
    (while (and (> i 0)
                (re-search-forward
                 ;; for square brackets, use closing-opening
                 (concat "[" close-char open-char "]") nil t))
      (if (and (string= (match-string 0) open-char) close-char)
          (setq i (1+ i))
        (setq i (1- i)))))
  (1- (point)))


(defun hyperlink-check-path ( path)
  (if (file-exists-p (expand-file-name path))
      (setq path (expand-file-name path))
    (let* (( default-paths (copy-tree hyperlink-default-paths))
           ( root (pop default-paths)))
      (while (and root
                  (not (file-exists-p
                        (expand-file-name
                         (concat (file-name-as-directory root)
                                 path)))))
        (setq root (pop default-paths)))
      (setq path (when root
                   (expand-file-name
                    (concat (file-name-as-directory root)
                            path))))))
  path)


(defun hyperlink-fontify-button ( limit)
  (unless (member (buffer-name) hyperlink-exclude-buffer-names)
    (when (re-search-forward "\\(\\([[(\"']\\)\s*\\)?\\([.~]*/[^}{\s]\\)" limit t)
      (let (( beg (match-beginning 3))
            ( end (point)))
        (if (match-string 2)
            (setq end (find-closing-position (match-string 2)))
          (re-search-forward "[.,;:]?\\(\s\\|$\\)" (line-end-position) t)
          (setq end (match-beginning 0)))
        ;; face and font-lock-multiline are managed by font-lock
        (remove-text-properties beg end `( mouse-face nil
                                           keymap nil
                                           help-echo nil
                                           rear-nonsticky nil
                                           front-sticky nil))
        (let (( path (hyperlink-check-path
                      (buffer-substring-no-properties beg end)))
              ( map (make-sparse-keymap)))
          (when path
            (define-key map [mouse-2]
              `(lambda ()
                 (interactive)
                 (find-file-other-window ,path)))
            (add-text-properties beg end
               `( face button
                  font-lock-multiline t
                  mouse-face highlight
                  keymap ,map
                  help-echo "mouse-2, Open location in other window."
                  rear-nonsticky t
                  front-sticky t)))))
      t)))


;;** Function for reading multiple choice


(defun read-choice ( prompt choices)
  "Read a choice in the minibuffer, with completion.

This function is meant as a replacement for `completing-read' in
some special cases. These cases are characterized by multiple
choice selection rather than selection by completion. Partly
inspired by `minibuffer-completion-help'."
  (with-current-buffer-window
   "*Choices*"
   `((display-buffer-at-bottom)
     ,(if temp-buffer-resize-mode
          '(window-height . resize-temp-buffer-window)
        '(window-height . fit-window-to-buffer))
     ,(when temp-buffer-resize-mode
        '(preserve-size . (nil . t))))
   nil
   (princ "Click on a choice to select it.\n")
   (princ "In this buffer, type RET to select the completion near point.\n\n")
   (princ "Possible choices are:\n")
   ;; needed for propertizing the string ?
   (with-current-buffer standard-output
       (dolist ( item choices)
         (insert (propertize (concat "["(string (car item))"] "
                                     (cadr item))
                             'read-choice (car item)
                             'mouse-face 'highlight
                             'help-echo "mouse-1: Select"))
         (insert "\n"))))
  (let (( inhibit-quit t)
          event)
    (while (and (not (car (assoc event choices)))
                (not (eq event 7))) ; Quit event C-g
      (setq event (read-event prompt)
            event (if (and (listp event) (eq (car event) 'mouse-1))
                      (with-current-buffer (window-buffer (car (cadr event)))
                        (get-text-property (cadr (cadr event))
                                           'read-choice))
                      event)))
    (delete-window (get-buffer-window "*Choices*"))
    event))


;;** Function for screen and frame layout

(defun kill-other-apps ()
  (interactive)
  (shell-command "/home/dan/bin/i3-kill-window -e 5 emacs"))


(defun i3-fullscreen-by-killing ()
  (interactive)
  (when (< (frame-pixel-width) 1900)
        (kill-other-apps)
        (sleep-for 0.25)))


(defun fullscreen-two-pan-view ( &optional other)
  (interactive)
  (delete-other-windows)
  (i3-fullscreen-by-killing)
  (split-window-right)
  (sleep-for 0.25)
  (when other (other-window 1)))


;;** Function for external browser actions


(defun launch-browser ()
  (interactive)
  (crowded-close-others)
  (async-shell-command "firefox -P emacs -no-remote"))


(defun browser-google-active-region ( regionStr)
  (let (( url "https://google.com/search?q="))
    (when (region-active-p)
      (browse-url-firefox (concat url regionStr)))))


(defun browser-google-region ( start end)
  (interactive "r")
  (let (( regionStr (buffer-substring start end)))
    (browser-google-active-region regionStr)))


(defun browser-google-region-quoted ( start end)
  (interactive "r")
  (let (( regionStr (concat "\"" (buffer-substring start end) "\"")))
    (browser-google-active-region regionStr)))


(defun network-protocol-emacs:file ( file)
  (let* (( protocol-file (string-split file ":"))
         ( protocol (car protocol-file))
         ( file (cadr protocol-file)))
    (find-file file)))


;;** Function for archiving and backup of files

(defun get-original-file-path ()
  (let (( path (buffer-file-name)))
    (unless path
            (cond ((string= (buffer-name) "*Org Agenda*")
                   (setq path (org-agenda-get-link-path)))
                  ((string= (buffer-name) "*Locate*")
                   (setq path (locate-get-filename)))))
    path))


(defun archive-copy-file ( &optional path)
  "Copy a file into some archive directory.

PATH can be given, if it is not the current buffer file."
  (interactive)
  (let (( dired-copy-preserve-time t))
    (unless path (setq path (get-original-file-path)))
    (let* (( file (file-name-nondirectory path))
           ( base (file-name-base path))
           ( newDir (concat (file-name-as-directory (getenv "HOME"))
                            (file-name-as-directory "archive")
                            (file-name-as-directory base)))
           new)
      (setq newDir (expand-file-name
                    (read-directory-name
                     (format "Archive file %s into directory: " file)
                     (file-name-add-time newDir t) nil nil nil)))
      (setq newDir (file-name-as-directory newDir))
      (setq new (concat newDir file))
      (when (y-or-n-p (format "Archive to %s? " new))
        (unless (file-directory-p newDir)
          (make-directory newDir t))
        (dired-copy-file path new t))
      new)))


(defun archive-move-file ( &optional path)
  (interactive)
  (when (get-buffer "*Choices*") (kill-buffer "*Choices*"))
  (unless path (setq path (get-original-file-path)))
  (let (( new (archive-copy-file path)))
    (if (and path
             (file-exists-p new)
             (not (string-equal path new)))
        (progn
          (delete-file path)
          (unless (string= (buffer-name) "*Org Agenda*")
                  (kill-buffer)))
          (message "File moved to %s" new)
        (message "File copied to %s, but not deleted!" new))))


(defun backup-copy-file ( &optional path)
  (interactive)
  (let* (( dired-copy-preserve-time t)
         ( new "/home/dan/backup/mtime-file/"))
    (unless path (setq path (get-original-file-path)))
    (unless (file-directory-p new) (make-directory new t))
    (setq new (concat new (file-name-add-time path)))
    (when (and path new)
          (dired-copy-file path new t)
          (message "Wrote %s" new))
    new))


;;** Function for file-name changes


(defun contract-file-name ( path)
  "Opposite function to expand-file-name"
  (replace-regexp-in-string (getenv "HOME") "~" path))


(defun file-remove-url-part ( url)
  "Opposite function to expand-file-name"
  (replace-regexp-in-string "file://" "" url))


(defun file-atime ( path)
  (nth 4 (file-attributes path 'string)))
(defun file-mtime ( path)
  (nth 5 (file-attributes path 'string)))
(defun file-stime ( path)
  (nth 6 (file-attributes path 'string)))


(defun directory-path-p ( path)
  (string= path (file-name-as-directory path)))


(defun file-name-add-time ( path &optional abs mtime prepend)
  (let (( name (file-name-nondirectory (directory-file-name path)))
        ( dir (directory-path-p path))
        ( time (format-time-string "%Y%m%d_%H%M%S"
                  (if mtime
                      (file-mtime path)
                      (current-time))))
        ( absPath (when abs (file-name-directory (directory-file-name path)))))
    (setq name (concat absPath
                       (if prepend
                           (concat time "-" name)
                           (concat (file-name-base name) "-" time
                                   (file-name-extension name t)))))
    (if dir (file-name-as-directory name) name)))


(defun mv-buffer ()
  "Move (rename) file visited by the current buffer.

This command is the elisp version of the *mv* command in
unix/linux."
  (interactive)
  (let* (( path (buffer-file-name))
         ( new (expand-file-name
            (read-file-name
              (format "Move file %s to: "
                      (file-name-nondirectory path))
              path nil nil nil)))
         ( pos (point)))
    (if (file-exists-p new)
        (message "File already exists!")
      (save-buffer)
      (make-directory (file-name-directory new) 'parents)
      (rename-file path new)
      (kill-buffer)
      (find-file new)
      (goto-char pos))))


(defun cp-buffer ()
  "Copy current buffer or region content to new file.

Purpose is to take the content of the current buffer or active
region and save it under a new name.  The user is interactively
asked for a new path location and name for the new file. If the
filename already exsists, nothing is done."
  (interactive)
  (let* (( path (buffer-file-name))
         ( new-path (expand-file-name
            (read-file-name
              (format "Copy file %s to: "
                      (file-name-nondirectory path))
                path nil nil nil))))
    (if (file-exists-p new-path)
        (message "File %s already exists!"
                 (file-name-nondirectory new-path))
        (progn (find-file new-path)
               (insert-file-contents path)
               (normal-mode t)
               (save-buffer)))))


(defun ln-s-buffer ()
  (interactive)
  (let* (( path (buffer-file-name))
         ( new-path (expand-file-name
            (read-file-name
              (format "Symbolic link to %s. Link name: "
                      (file-name-nondirectory path))
                path nil nil nil))))
    (if (file-exists-p new-path)
        (message "File %s already exists!"
                 (file-name-nondirectory new-path))
        (make-symbolic-link path new-path 1))))


(defun rm-buffer ( &optional path)
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (when (get-buffer "*Choices*") (kill-buffer "*Choices*"))
  (unless path (setq path (buffer-file-name)))
  (delete-file path 'TRASH)
  (message "File %s deleted!" path)
  (when (get-file-buffer path)
        (kill-buffer (get-file-buffer path))))


(defun trash-put-buffer ( &optional path)
  "Kill the current buffer and move the file it is visiting to trash."
  (interactive)
  
  (when (get-buffer "*Choices*") (kill-buffer "*Choices*"))
  (unless path (setq path (buffer-file-name)))
  (let (( org-roam-build-cache (org-roam-file-p path)))
    (rename-file path (concat (expand-file-name "~/.local/share/Trash/files/")
                              (file-name-nondirectory path)))
    (when org-roam-build-cache (org-roam-db-sync)))
  (message "File %s moved to trash!" path)
  (when (get-file-buffer path)
        (kill-buffer (get-file-buffer path))))


;;** Function frame display


(defun frame-outer-size-width ()
  "Return outer width of frame."
  (let (( size nil)
        ( identifier nil))
    (dolist (g (frame-geometry))
      (setq identifier (pop g))
      (when (string= identifier "outer-size")
        (setq size (car g))))
    size))


(defun frame-outer-size-height ()
  "Return outer height of frame."
  (let (( size nil)
        ( identifier nil))
    (dolist (g (frame-geometry))
      (setq identifier (pop g))
      (when (string= identifier "outer-size")
        (setq size (cdr g))))
    size))


(defun get-column-windows ()
  (save-window-excursion
  (let (( window (selected-window))
          column-windows)
    (while window
      (select-window window)
      (setq window (window-in-direction 'below)))
    (setq window (selected-window))
    (while window
      (select-window window)
      (setq column-windows (cons window column-windows)
            window (window-in-direction 'above)))
    column-windows)))


(defun delete-difference-windows ( remaining-windows)
  (let (( delete-windows (cl-set-difference (window-list)
                                            remaining-windows)))
    (dolist ( window delete-windows)
      (delete-window window))))


(defun crowded-close-others ()
  (when (= (frame-outer-size-width) (x-display-pixel-width))
    ;; (delete-other-windows)
    (delete-difference-windows (get-column-windows))))


(defun collapse-toggle-frame-fullscreen ()
  (interactive)
  (toggle-frame-fullscreen)
  (when (< (frame-outer-size-width) (x-display-pixel-width))
    (delete-other-windows)))


(defun swap-buffer-window ()
  "Put the buffer from the selected window in next window."
  (interactive)
  (let (( other-buffer (window-buffer (next-window))))
    (set-window-buffer (next-window) (current-buffer))
    (set-window-buffer nil other-buffer)
    ;; (previous-buffer)
    ;; (tabbar-close-tab)
    )
  (other-window 1))


(defun e/kill-buffer-delete-frame ()
  (interactive)
  (unless (> (length (get-buffer-window-list nil nil t)) 1)
    (kill-buffer))
  (delete-frame))


;; (setq window-divider-default-right-width 50)
;; (window-divider-mode 1)


;;** Function opening files


(defun extension-find-major-mode ( path)
  (let (( modeList (copy-tree auto-mode-alist))
        pair modeFound)
    (while modeList
           (setq pair (pop modeList))
           (when (string-match (car pair) path)
                 (setq modeFound (cdr pair))
                 (setq modeList nil)))
    (format "%s" modeFound)))


(defun first-line-find-major-mode ( path)
  (let (( firstLine (with-temp-buffer
                      (insert-file-contents path)
                      (goto-char (point-min))
                      (buffer-substring-no-properties
                        (point)
                        (line-end-position))))
        ( modeList (copy-tree magic-mode-alist))
        pair modeFound)
    (while modeList
           (setq pair (pop modeList))
           (when (string-match (car pair) firstLine)
                 (setq modeFound (cdr pair))
                 (setq modeList nil)))
    (format "%s" modeFound)))


(require 'org)
(defun file-external-app-command ( path)
  (let* ( cmd binPath binCmd)
    (dolist ( pair org-file-apps)
            (when (stringp (car pair))
                  (when (string-match-p (car pair)
                                        (file-name-extension path t))
                        (setq cmd (car (split-string (cdr pair)))))))
    (when cmd
          (dolist ( binDir (split-string (getenv "PATH") ":"))
                  (setq binPath
                        (concat (file-name-as-directory binDir)
                                cmd))
                  (when (file-executable-p binPath)
                        (setq binCmd binPath))))
    binCmd))


(defun open-file-in-chosen-mode ( path majorMode binCmd)
  (cond ((and path majorMode (not binCmd))
         (find-file path)
         (unless (string= majorMode "normal-mode")
                 (funcall (intern majorMode)))
         (kill-buffer "*Choices*"))
        ((and path (not majorMode) binCmd)
         (crowded-close-others)
         (async-shell-command binCmd nil))
        ((and path (not majorMode) (not binCmd))
         (backup-copy-file path)
         (open-file-choice path t))
        ((kill-buffer "*Choices*"))))


(defun open-file-dired-goto ( path)
  (dired (file-name-directory path))
  (dired-goto-file path))


(defun open-file-choice ( path &optional finish)
  (let* (( extension (file-name-extension path))
         ( external (file-external-app-command path))
         ( alternative (first-line-find-major-mode path))
         ( majorMode (extension-find-major-mode path))
         ( choice 1)
         ( binCmd (concat external " " (shell-quote-argument path))))
    (if finish
        (switch-to-buffer "*Choices*")
        (switch-to-buffer-other-window "*Choices*"))
    (read-only-mode -1)
    (erase-buffer)
    (hl-line-mode 1)
    (insert (format "Operations for file %s:\n\n"
                    (contract-file-name path)))
    (insert (string-join (butlast (split-string
      (shell-command-to-string
        (concat "ls -lh "
                (shell-quote-argument path))))) " "))
    (insert "\n")
    (insert (car (cdr (split-string
      (shell-command-to-string
           (concat "file " (shell-quote-argument path))) ": "))))
    (insert (format "readable %s, executable %s\n"
            (file-readable-p path)
            (file-executable-p path)))
    (insert "\n")
    (if finish
        (insert-button "Finish"
                   'action (lambda (b)
            (open-file-in-chosen-mode nil nil nil)))
        (insert-button "Backup file"
                   'path path
                   'action (lambda (b)
            (open-file-in-chosen-mode (button-get b 'path)
                                      nil
                                      nil))))
    (insert "\nOpen file in ")
    (insert-button "normal-mode (see find-file)"
                   'path path
                   'majorMode "normal-mode"
                   'action (lambda (b)
            (open-file-in-chosen-mode (button-get b 'path)
                                      (button-get b 'majorMode)
                                      nil)))
    (insert "\nOpen file in major mode (see auto-mode-alist) ")
    (insert-button (format "%s" majorMode)
                   'path path
                   'majorMode majorMode
                   'action (lambda (b)
            (open-file-in-chosen-mode (button-get b 'path)
                                      (button-get b 'majorMode)
                                      nil)))
    (insert "\nOpen file in alternative mode (see magic-mode-alist) ")
    (insert-button (format "%s" alternative)
                   'path path
                   'majorMode alternative
                   'action (lambda (b)
            (open-file-in-chosen-mode (button-get b 'path)
                                      (button-get b 'majorMode)
                                      nil)))
    (insert "\nOpen file in ")
    (insert-button "fundamental-mode"
                   'path path
                   'majorMode "fundamental-mode"
                   'action (lambda (b)
            (open-file-in-chosen-mode (button-get b 'path)
                                      (button-get b 'majorMode)
                                      nil)))
    (insert "\nGoto file in ")
    (insert-button "Dired"
                   'path path
                   'action (lambda (b)
            (open-file-dired-goto (button-get b 'path))))
    (insert "\nOpen file in external app (see org-file-apps) ")
    (insert-button (format "%s" external)
                   'path path
                   'binCmd binCmd
                   'action (lambda (b)
            (open-file-in-chosen-mode (button-get b 'path)
                                      nil
                                      (button-get b 'binCmd))))
    (insert "\n\nArchiving file:\n")
    (insert-button "Copy"
                   'path path
                   'action (lambda (b)
            (archive-copy-file (button-get b 'path))))
    (insert " into archive ~/archive/NEW-DIR\n")
    (insert-button "Move"
                   'path path
                   'action (lambda (b)
            (archive-move-file (button-get b 'path))))
    (insert " into archive ~/archive/NEW-DIR\n")
    (when (string= (file-name-extension path) "tex")
          (let* (( fileInfo (TeX-all-file-include-list path)))
            (insert-button "Archive file tree:"
                   'path path
                   'action (lambda (b)
            (TeX-archive-files (button-get b 'path))))
            (insert " ")
            (insert-copy-file-tree path (car fileInfo)
                                        (cadr fileInfo))))
    (insert "\n------Stat\n")
    (insert (shell-command-to-string
            (concat "stat " (shell-quote-argument path))))
    (insert "\n------\n")
    (insert-button "Remove"
                   'path path
                   'action (lambda (b)
            (delete-kill-file (button-get b 'path))))
    (insert " file and kill buffer\n")
    (read-only-mode 1)))


(defun buffer-open-file ()
  (interactive)
  (fullscreen-two-pan-view)
  (open-file-choice (buffer-file-name)))


;;** Function for begin-end-block folding

(defun begin-end-block-folded-p ()
  (let* (( start (line-end-position))
         ( overlays (overlays-at start))
         ( overlay (pop overlays)))
    (while (and overlay
                (or (/= (overlay-start overlay) start)
                    (not (overlay-get overlay 'invisible))))
      (setq overlay (pop overlays)))
    overlay))


(defun toggle-block-folding ( show-function hide-function)
  (interactive)
  (let (( overlay (begin-end-block-folded-p)))
    (if overlay
      (let (( beg (overlay-start overlay)))
        (funcall show-function overlay)
        (goto-char beg))
      (funcall hide-function))))


(defun begin-end-block-cloaked-p ()
  (let* (( pos (point))
         ( overlays (overlays-at pos))
         ( overlay (pop overlays)))
    (while (and overlay
                (or (< pos (overlay-start overlay) )
                    (< (overlay-end overlay) pos)
                    (not (overlay-get overlay 'invisible))))
      (setq overlay (pop overlays)))
    overlay))


;;** Function for remapping global keys

(defun self-insert-command-dwim ( N)
  "Target function of a remapping of `self-insert-command'.

Allow for insert manipulation when there is an active region. The
manipulation places the pressed character around the active
region."
  (interactive "p")
  (if (use-region-p)
      (let* (( beg (region-beginning))
             ( end (region-end))
             ( new-pos (if (= (point) end)
                           (+ end 2)
                         (+ beg 1))))
        (goto-char end)
        (self-insert-command N)
        (let (( c (string (char-before))))
          (goto-char beg)
          (cond ((string= c ")")
                 (insert "("))
                ((string= c "}")
                 (insert "{"))
                ((string= c "]")
                 (insert "["))
                ((string= c ">")
                 (insert "<"))
                (t
                 (self-insert-command N))))
        (goto-char new-pos))
    (self-insert-command N)))


;;** Function for temp-edit buffer

;;*** Define variables

(defvar temp-edit-reinsert-list nil)


(defvar temp-edit-major-mode-alist
  '(( "> " . doxy-mode)
    ( "# " . doxy-mode)
    ( "m " . markdown-mode)))


(defvar temp-edit-compile-command nil)


;;*** Function for compiling the base buffer

(defun temp-edit-compile-base ()
  (interactive)
  (let (( buffer (car temp-edit-reinsert-list)))
    (temp-edit-reinsert)
    (with-current-buffer buffer
      (funcall temp-edit-compile-command))))
         

;;*** Function for extracting and reinsert

(defun prefix-block-boundaries ()
  (save-excursion
    (let (( prefix (string-trim (line-prefix))))
      (beginning-of-line)
      (while (and (looking-at prefix)
                  (not (bobp)))
        (beginning-of-line 0))
      (unless (bobp)
        (beginning-of-line 2))
      (let (( beg (point)))
        (while (and (looking-at prefix)
                    (not (eobp)))
          (beginning-of-line 2))
        `( ,beg . ,(point))))))
      

(defun remove-prefix ( prefix)
  (goto-char (point-min))
  (setq prefix (string-trim prefix))
  (while (re-search-forward (concat "^" prefix "\s?") nil t)
    (replace-match "")))


(defun temp-edit-extract-region ( beg end)
  (let* (( prefix (line-prefix beg))
         ( prefix-extra (when prefix (substring prefix 1)))
         ( mm (or (cdr (assoc prefix-extra temp-edit-major-mode-alist))
                  major-mode))
         ( str (replace-regexp-in-string "\n\\'" ""
                 (buffer-substring beg end))))
    (setq temp-edit-reinsert-list
          (list (current-buffer) beg (+ beg (length str)) prefix
                (= (length (window-list)) 1)))
    (read-only-mode 1)
    (switch-to-buffer-other-window "*Edit*")
    (erase-buffer)
    (insert str)
    (when prefix
      (remove-prefix prefix))
    (funcall mm)
    (outline-minor-mode 0)
    (temp-edit-mode 1)
    (set-buffer-modified-p nil)))


(defun temp-edit-reinsert ( &optional save-base kill-edit)
  (interactive)
  (let* (( edit-buffer (current-buffer))
         ( buffer (nth 0 temp-edit-reinsert-list))
         ( beg (nth 1 temp-edit-reinsert-list))
         ( end (nth 2 temp-edit-reinsert-list))
         ( prefix (nth 3 temp-edit-reinsert-list))
         ( sole-window-p (nth 4 temp-edit-reinsert-list))
         ( str (replace-regexp-in-string
                "^" (or prefix "")
                (buffer-substring (point-min) (point-max)))))
      (when (buffer-modified-p)
        (setq temp-edit-reinsert-list
              (list buffer beg (+ beg (length str)) prefix
                    sole-window-p))
        (when save-base
          (set-buffer-modified-p nil))
        (with-current-buffer buffer
          (let (( inhibit-read-only t))
            (delete-region beg end)
            (goto-char beg)
            (insert str)
            (when save-base
              (save-buffer)))))
      (when kill-edit
        (when sole-window-p
          (delete-window))
        (pop-to-buffer buffer)
        (read-only-mode 0)
        (kill-buffer edit-buffer))))


(defun temp-edit-toggle ()
  (interactive)
  (if (string= (buffer-name) "*Edit*")
      (temp-edit-reinsert nil 'kill-edit)
    (let (( beg (region-beginning))
          ( end (region-end))
          beg-end)
      (unless (region-active-p)
        (setq beg-end (prefix-block-boundaries)
              beg (car beg-end)
              end (cdr beg-end)))
      (temp-edit-extract-region beg end))))


(defun temp-edit-save ()
  (interactive)
  (temp-edit-reinsert 'save-base nil))


;;*** Minor mode definition

(defvar temp-edit-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

(defun set-temp-edit-mode-map ()
  (define-key temp-edit-mode-map "\C-x\C-s" 'temp-edit-save)
  (define-key temp-edit-mode-map "\C-c\C-c" 'temp-edit-compile-base))
(set-temp-edit-mode-map)

(define-minor-mode temp-edit-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value nil
  :lighter ""
  :keymap temp-edit-mode-map)


;;** Function for emacs process handling


(defun process-command-exists-p ( command)
  (let (( processes (process-list))
          process found)
    (while
      (and processes
           (not (setq found
                      (string-match command
                        (string-join (process-command
                         (setq process (pop processes)))))))))
    (when found
      process)))


(defun delete-process-by-command ( command)
  (let (( process (process-command-exists-p command)))
    (when process
      (delete-process process))))


(defun kill-firefox ()
  (interactive)
  (delete-process-by-command "firefox"))


;;** Stemplate package


(push "~/.emacs.d/lisp/stemplate" load-path)
(require 'stemplate)


;;** Function for journal


(defun journal ()
  "Open HTML file named after today's date, format YYYY-MM-DD-Day.html,
in subdirectory named in variable journal-dir, set in ~/.emacs,
else in $HOME."
  (interactive)
  (let (( system-time-locale "en_US.utf8"))
    (let* (( directory (concat (file-name-as-directory
                                (expand-file-name "~"))
                               (file-name-as-directory ".journal")))
           ( topic (completing-read "Topic: "
                                    (subdirs directory)
                                    nil nil ""))
          ( filename (concat (format-time-string "%Y-%m-%d-%a-%H-%M" nil)
                             ".md")))
      (unless (string= topic "")
        (setq topic (file-name-as-directory topic)))
      (setq directory (concat directory topic))
      (unless (file-directory-p directory)
        (mkdir directory 'create-nonexistant-parents)) 
      (find-file (expand-file-name
                  (read-file-name "Open journal file: "
                                  directory filename nil filename)
                  directory)))))


(defun subdirs (directory &optional dotted)
  (let (dirs)
    (dolist (file (directory-files directory))
      (message "%s" file)
      (when (file-directory-p (expand-file-name file directory))
        (when (or dotted (not (string= (substring file 0 1) ".")))
          (setq dirs (cons file dirs)))))
    dirs))


(defun journal-set-entry ( dir action file)
  (find-file (concat (file-name-as-directory dir) "contents.md"))
  (goto-char (point-max))
  (insert "\n" (file-relative-name dir) file " via " action ":\n")
  (concat "emacsclient: The file " file
          "appeared in directory " dir
          "via " action "."))


;;** Function for timewarrior


(defun tw-tags ()
  (let ( tags)
    (dolist ( data-file (directory-files-recursively
                     "/home/dan/.timewarrior/data/" "[0-9]+-[0-9]+\\.data"))
      (with-temp-buffer
        (insert-file-contents data-file)
        (goto-char (point-min))
        (while (re-search-forward "\"\\(.*?\\)\"" (point-max) t)
          (setq tags (cons (match-string 1) tags))
          (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward "#\s*\\(.*?\\)\s*$" (point-max) t)
          (dolist ( tag (split-string (match-string 1) " " t))
            (setq tags (cons tag tags))))))
    (delete-dups tags)))


(defun tw-tag-sets ()
  (let ( tag-sets)
    (dolist ( data-file (directory-files-recursively
                     "/home/dan/.timewarrior/data/" "[0-9]+-[0-9]+\\.data"))
      (with-temp-buffer
        (insert-file-contents data-file)
        (goto-char (point-min))
        (while (re-search-forward "#\s*\\(.*?\\)\s*$" (point-max) t)
          (setq tag-sets (cons (match-string 1) tag-sets)))))
    (delete-dups tag-sets)))


(defun tw-running-tag-set ()
  (with-temp-buffer
    (insert-file-contents (car (last (directory-files-recursively
                                        "/home/dan/.timewarrior/data/" "[0-9]+-[0-9]+\\.data"))))
    (goto-char (point-max))
    (beginning-of-line 0)
    (when (looking-at "^inc\s[0-9TZ]+\s#\s*\\(.*?\\)\s*$")
      (match-string 1))))


(defun tw-complete ()
  (let (( tags (or (tw-running-tag-set)
                   (completing-read "Tag set: " (tw-tag-sets))))
        ( all-tags (tw-tags))
        tag)
    (while (not (string= tag ""))
      (when tag
        (when (string-match "\s" tag)
          (setq tag (concat "\"" tag "\"")))
        (if (string-match-p (regexp-quote tag) tags)
            (setq tags (replace-regexp-in-string (regexp-quote tag) "" tags))
          (setq tags (concat tags (and tags " ") tag)))
        (setq tags (replace-regexp-in-string "^\s+\\|\s+$" "" tags)
              tags (replace-regexp-in-string "\s\s+" " " tags)))
      (setq tag (completing-read
                 (concat "Tags:"
                         (unless (string-match-p "^\s*$" tags) " ")
                         tags
                         " (add/remove tag) ")
                 all-tags)))
    tags))


(defun tw-start ()
  (interactive)
  (when (tw-running-p)
    (shell-command (concat "timew @1 tag " (tw-complete))))
  (let ((inhibit-message t))
    (shell-command (concat "timew start")))
  (message "Timewarrior started."))


(defun tw-break-toggle ()
  (interactive)
  (if (tw-running-p)
      (progn
        (let ((inhibit-message t))
          (unless (tw-running-tag-set)
            (shell-command "timew tag @1 '<continue>'"))
          (shell-command (concat "timew stop")))
        (message "Timewarrior paused."))
    (let ((inhibit-message t))
      (shell-command "timew continue")
      (shell-command "timew untag @1 '<continue>'"))
    (message "Timewarrior continued.")))


(defun tw-stop ()
  (interactive)
  (shell-command (concat "timew tag @1 " (tw-complete)))
  (let ((inhibit-message t))
    (shell-command "timew stop" ))
  (message "Timewarrior stopped."))


(defun tw ()
  (interactive)
  (shell-command "timew"))


(defun tw-running-p ()
  (if (> (length (split-string (shell-command-to-string "timew") "\n")) 2)
      t
    nil))


;;** Function for word count


(defun wcount-diff ()
  (interactive)
  (let (( cmd 'wc))
    (message "Word count diff %s"
             (- (string-to-number (wcount-current cmd))
                (string-to-number (wcount-log-find cmd))))))


(defun wcount-log-find ( cmd)
  (let (( path (buffer-file-name)))
    (with-temp-buffer
      (insert-file-contents "/home/dan/.emacs.d/.wcount.log")
      (re-search-forward (concat "^" (regexp-quote path)
                                 " \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
                         (point-max) 'noerror)
      (cond ((eq cmd 'count-words)
             (match-string 2))
            ((eq cmd 'wc)
             (match-string 3))
            ((eq cmd 'texcount)
             (match-string 4))))))


(defun wcount-log-today ()
  (with-temp-buffer
    (insert-file-contents "/home/dan/.emacs.d/.wcount.log")
    (let ( paths
          ( sum-count-words 0)
          ( sum-wc 0)
          ( sum-texcount 0))
      (while (re-search-forward
              (concat "^\"\\(.*?\\)\" "
                      (format-time-string "%Y%m%d ")
                      "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
              (point-max) 'noerror)
        (setq paths (cons (match-string 1) paths)
              sum-count-words (+ sum-count-words (string-to-number (match-string 2)))
              sum-wc (+ sum-wc (string-to-number (match-string 3)))
              sum-texcount (+ sum-texcount (string-to-number (match-string 4)))))
      `( ,paths ,sum-count-words ,sum-wc ,sum-texcount))))
   
      
(defun wcount-current-diff ()
  (let* (( today (wcount-log-today))
         ( paths (nth 0 today))
         ( sum-count-words-0 (nth 1 today))
         ( sum-wc-0 (nth 2 today))
         ( sum-texcount-0 (nth 3 today))
         ( sum-count-words-1 0)
         ( sum-wc-1 0)
         ( sum-texcount-1 0))
    (dolist ( path paths)
      (setq sum-count-words-1 (+ sum-count-words-1 (wcount-count-words path))
            sum-wc-1 (+ sum-wc-1 (wcount-wc path))
            sum-texcount-1 (+ sum-texcount-1 (wcount-texcount path))))
    `(,(- sum-count-words-1 sum-count-words-0)
      ,(- sum-wc-1 sum-wc-0)
      ,(- sum-texcount-1 sum-texcount-0))))


(defun wcount-current ( cmd)
  (let (( path (buffer-file-name)))
    (cond ((eq cmd 'count-words)
           (wcount-count-words path))
          ((eq cmd 'wc)
           (wcount-wc path))
          ((eq cmd 'texcount)
           (wcount-texcount path)))))
  

(defun wcount-log-prepend ()
  (let (( path (buffer-file-name)))
    ; disable for now
    (setq path nil)
    (when (and path
               (not (string= (file-name-extension path) "log"))
               (not (wcount-buffer-log-p path)))
      (with-temp-file "/home/dan/.emacs.d/.wcount.log"
        (insert (format "\"%s\" %s %s %s %s\n" path (format-time-string "%Y%m%d")
                        (wcount-count-words path)
                        (wcount-wc path)
                        (wcount-texcount path)))
        (insert-file-contents "/home/dan/.emacs.d/.wcount.log"))
      (message "Wcount buffer added."))))


(defun wcount-buffer-log-p ( path)
  (with-temp-buffer
    (insert-file-contents "/home/dan/.emacs.d/.wcount.log")
    (re-search-forward (concat "^\""(regexp-quote path) "\" "
                               (format-time-string "%Y%m%d "))
                       (point-max) 'noerror)))


(defun wcount-modify-contents-org ( mode)
  (when (eq mode 'org-mode)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\s+[^\n]*:\\(noexport\\|nowcount\\):" nil t)
      (let (( start (line-beginning-position)))
        (if (re-search-forward (concat "^\\*\\{1," (number-to-string (length (match-string 1))) "\\}\s+") nil t)
            (beginning-of-line)
          (goto-char (point-max)))
        (delete-region start (point))))
    (goto-char (point-min))
    (let (( start (point)) found)
      (while (re-search-forward "^\\(\\*+\\)\s+[^\n]*:wcount:" nil t)
        (delete-region start (line-end-position))
        (unless (re-search-forward (concat "^\\*\\{1," (number-to-string (length (match-string 1))) "\\}\s+") nil t)
          (goto-char (point-max)))
        (beginning-of-line)
        (setq start (point) found t))
      (when found
        (delete-region start (point-max))))
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward org-property-drawer-re nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward org-block-regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward org-footnote-re nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward org-keyword-regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward (concat org-comment-regexp "[^\n]*") nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (and (re-search-forward org-drawer-regexp nil t)
                (not (string= (match-string 1) "END")))
      (let (( beg (match-beginning 0)))
        (when (and (re-search-forward org-drawer-regexp nil t)
                   (string= (match-string 1) "END"))
          (delete-region beg (match-end 0)))))
    t))


(defvar wcount-default-daily-goal 500)
(defvar wcount-default-total-goal 5000)
(defvar wcount-before-count-hook nil)
(defvar-local wcount-init nil)
(add-hook 'wcount-before-count-hook 'wcount-modify-contents-org)


(defun wcount-count-words ( &optional filepath)
  (let (( mode major-mode)
        ( buffer-string (unless filepath
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max)))))
    (with-temp-buffer
      (if filepath
          (insert-file-contents filepath)
        (insert buffer-string))
      ;; modify contents here according to actual words
      (run-hook-with-args-until-success 'wcount-before-count-hook mode)
      (count-words (point-min) (point-max)))))


(defun wcount-write-log-file ( &optional filepath)
  (let* (( wc (wcount-count-words filepath))
         ( path (or filepath (buffer-file-name)))
         ( init wcount-init))
    (when init
      (with-temp-buffer
        (insert "1970-01-01 00:00:00 4"
                " " (number-to-string init)
                " " path "\n")
        (write-region (point-min) (point-max) "~/.wcount.log" t))
      (setq-local wcount-init nil))
    (with-temp-buffer
      (insert (format-time-string "%Y-%m-%d %H:%M:%S %u")
              " " (number-to-string wc)
              " " path "\n")
      (write-region (point-min) (point-max) "~/.wcount.log" t))))


(defun wcount-today-p ( time-string)
  (let* (( time (decode-time
                 (save-match-data
                   (org-time-string-to-time time-string))))
         ( now (decode-time)))
    (and (= (nth 3 time) (nth 3 now))
         (= (nth 4 time) (nth 4 now))
         (= (nth 5 time) (nth 5 now)))))


(defun wcount-write-org-drawer ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\s*:WCOUNT:\s*$" nil t)
      (let* (( folded (org-fold-folded-p nil 'drawer))
             ( wcount (wcount-count-words)))
        (when (and (re-search-forward org-drawer-regexp nil t)
                   (string= (match-string-no-properties 1) "END"))
          (beginning-of-line 0)
          (if (and (looking-at "\s*\\(\\(\\[[^]]*\\]\\)[^\n]*\n\\)")
                   (wcount-today-p (match-string-no-properties 2)))
              (delete-region (match-beginning 1) (match-end 1))
            (beginning-of-line 2))
          (org-time-stamp-inactive '(16))
          (insert  " " (number-to-string wcount) "\n")
          (when folded
            (org-fold-hide-drawer-toggle)))))))

(defun wcount-create-org-drawer ()
  (interactive)
  (org-insert-drawer nil "WCOUNT")
  (let (( wcount (wcount-count-words)))
    (org-time-stamp-inactive '(16))
    (insert  " " (number-to-string wcount) "\n")
    (org-time-stamp-inactive '(16))
    (insert  " " (number-to-string wcount))))


(defun wcount-org-drawer-new-mark ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\s*:WCOUNT:\s*$" nil t)
      (let* (( folded (org-fold-folded-p nil 'drawer))
             ( wcount (wcount-count-words)))
        (when (and (re-search-forward org-drawer-regexp nil t)
                   (string= (match-string-no-properties 1) "END"))
          (beginning-of-line 1)
          (org-time-stamp-inactive '(16))
          (insert  " " (number-to-string wcount) "\n")
          (when folded
            (org-fold-hide-drawer-toggle)))))))


(defun wcount-parse-log-file ( path start-time-ts end-time-ts)
  (let ( word-count-start word-count-end start-found end-found)
    (with-temp-buffer
      (insert-file-contents "~/.wcount.log")
      (goto-char (point-min))
      (while (re-search-forward path nil t)
        (let* (( fields (split-string
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
               ( time-string (concat (nth 0 fields) " " (nth 1 fields)))
               ( day-of-week (nth 2 fields))
               ( time-string-ts (date-to-time time-string))
               ( word-count (string-to-number (nth 3 fields))))
          (if (time-less-p time-string-ts start-time-ts)
              (setq word-count-start word-count)
            (setq start-found t))
          (if (time-less-p time-string-ts end-time-ts)
              (setq word-count-end word-count)
            (setq end-found t)))))
    `( ,(and start-found word-count-start) .
       ,(and end-found word-count-end))))


(defun wcount-parse-org-drawer ( &optional total-count)
  (save-excursion
    (goto-char (point-min))
    (let ( found)
      (when (re-search-forward ":WCOUNT:" nil t)
        (let (( bound (match-end 0)))
          (re-search-forward org-drawer-regexp nil t)
          (when (string= (match-string-no-properties 1) "END")
            (while (and (setq found
                              (re-search-forward "\\(\\[[^]]*\\]\\)\s+\\([0-9]+\\)"
                                                 bound t -1))
                        (wcount-today-p (match-string-no-properties 1)))))))
      (if found
          (let (( last-count (string-to-number
                              (match-string-no-properties 2))))
            (when (and total-count (> last-count total-count))
              (setq last-count total-count)
              (replace-match (number-to-string last-count)
                             t nil nil 2))
            last-count)
        0))))


(defun wcount-current-count ( &optional filepath time-range-start time-range-end)
  (let* (( path (or filepath (buffer-file-name)))
         ( now (decode-time))
         ( current-day `( 0 0 0 ,(nth 3 now) ,(nth 4 now) ,(nth 5 now)
                                ,(nth 6 now) ,(nth 7 now) ,(nth 8 now)))
         ( start-time-ts (or time-range-start
                             (encode-time current-day)))
         ( end-time-ts (or time-range-end
                           (time-add start-time-ts (* 60 60 24))))
         ( log (wcount-parse-log-file path start-time-ts end-time-ts))
         ( start (car log))
         ( end (or (cdr log)
                   (wcount-count-words filepath))))
    (if start
        (- end start)
      (unless wcount-init
        (setq-local wcount-init end))
      (- end wcount-init))))


(defun wcount-texcount ( filepath)
  (let (( out (shell-command-to-string (concat "texcount "
                                               filepath))))
    (string-match "Words in text: *\\([0-9]+\\)" out)
    (string-to-number (match-string 1 out))))


(defun wcount-wc ( filepath)
  (let (( out (shell-command-to-string (concat "wc -w "
                                               filepath))))
    (string-match "^ *\\([0-9]+\\)" out)
    (string-to-number (match-string 1 out))))


(defvar-local wcount-lighter nil
  "This variable holds the per-buffer word-count statistics used to
update the modeline.")


(defun wcount-lighter-string ()
  "Return a string to update the modeline appropriately"
  (let* (( daily-goal (save-excursion
                        (goto-char (point-min))
                        (if (re-search-forward
                             "wcount-daily-goal[:=]?\s+\\([0-9]+\\)" nil t)
                            (match-string-no-properties 1)
                          (number-to-string wcount-default-daily-goal))))
         ( total-goal (save-excursion
                        (goto-char (point-min))
                        (if (re-search-forward
                             "wcount-total-goal[:=]?\s+\\([0-9]+\\)" nil t)
                            (match-string-no-properties 1)
                          (number-to-string wcount-default-total-goal))))
         ( total-count (wcount-count-words))
         ( last-count (wcount-parse-org-drawer)))
    (concat " wcount["
            (number-to-string (- total-count last-count)) "/" daily-goal
            "|" (number-to-string total-count) "/" total-goal
            "]")))


(setq wcount-timer-tracker
      (run-with-idle-timer
       0 t
       '(lambda ()
          (when wcount-mode
            (setq wcount-lighter (wcount-lighter-string))))))


;; (define-minor-mode wcount-mode
;;   "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
;;   :init-value nil :lighter (:eval wcount-lighter) :keymap nil
;;   (cond (wcount-mode
;;          (add-hook 'after-save-hook
;;                    'wcount-write-log-file nil 'local))
;;         (t
;;          (setq-local wcount-init nil)
;;          (remove-hook 'after-save-hook
;;                       'wcount-write-log-file 'local))))


(define-minor-mode wcount-mode
  "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
  :init-value nil :lighter (:eval wcount-lighter) :keymap nil
  (cond (wcount-mode
         (add-hook 'before-save-hook
                   'wcount-write-org-drawer nil 'local))
        (t
         (remove-hook 'before-save-hook
                      'wcount-write-org-drawer 'local))))


(defun wcount-find-file-at-mouse (event)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (let* (( event-start (event-start event))
         ( file (mouse-posn-property event-start 'file)))
    (find-file-other-window file)))


(define-derived-mode wcount-buffer-mode text-mode "WCount"
    "Major mode for listing emacs buffers."
    (unless (eq (current-buffer) (get-buffer "*WCount Buffer*"))
      (error "WCount: current buffer is no WCount buffer."))
    (setq truncate-lines t)
    (define-key wcount-buffer-mode-map (kbd "g") 'wcount-buffer-update)
    (define-key wcount-buffer-mode-map
      (kbd "<mouse-2>") 'wcount-find-file-at-mouse))


(defun wcount-buffer-update ()
  (interactive)
  (wcount-buffer 'UPDATE))


(defun wcount-buffer ( &optional update)
  (interactive)
  (unless update
    (switch-to-buffer-other-window "*WCount Buffer*"))
  (let (( inhibit-read-only t))
    (erase-buffer)
    (wcount-buffer-mode)
    (dolist ( buffer (buffer-list))
      (let ( wcount-p wcount-info)
        (with-current-buffer buffer
          (when (symbol-value 'wcount-mode)
            (setq wcount-p t
                  wcount-info (wcount-lighter-string))))
        (when wcount-p
          (insert (format "%-30s" wcount-info)
                  (propertize (buffer-file-name buffer)
                              'face 'link
                              'mouse-face 'highlight
                              'file (buffer-file-name buffer))
                  "\n"))))
    (set-buffer-modified-p nil))
  (read-only-mode 1))


(add-hook 'first-change-hook
  (lambda ()
    (wcount-log-prepend)))


;;** Global key bindings


(global-set-key "\C-x\C-x" 'obuffer-open)
(global-set-key "\C-x\C-y" 'obuffer-use-other-window)
(global-set-key "\C-xd" 'dired-fullscreen-one-pan-view)
(global-set-key "\C-xx" 'dired-fullscreen-two-pan-view)
(global-set-key "\C-xtt" 'tw-start)
(global-set-key "\C-xte" 'tw-stop)
(global-set-key "\C-xtb" 'tw-break-toggle)
(global-set-key (kbd "\C-x\C-g") 'find-file-at-point)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-s M-s") 'swap-buffer-window)
(global-set-key (kbd "<f9>") 'browser-google-region)
(global-set-key (kbd "C-c <f9>") 'browser-google-region-quoted)
(global-set-key (kbd "<f11>") 'collapse-toggle-frame-fullscreen)
(global-set-key (kbd "<C-return>") 'temp-edit-toggle)
(global-set-key (kbd "<C-M-return>") 'electric-indent-just-newline)
(global-set-key "\C-cg" 'grep-region)
(global-set-key "\C-ch" 'locate)
(global-set-key (kbd "M-<wheel-down>") 'e/mwheel-scroll-left)
(global-set-key (kbd "M-<wheel-up>") 'e/mwheel-scroll-right)
(global-set-key (kbd "C->") 'fill-to-width)
(global-unset-key (kbd "<C-down-mouse-1>"))
(global-set-key (kbd "C-c n f") 'e/ivy-org-roam-node-find)
(global-set-key (kbd "C-c n b") 'e/org-roam-browser-node-find)
(global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-date)
(global-set-key (kbd "C-c n n") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-x") 'org-clock-in-last)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-n C-n") 'e/org-roam-note-list-plain)
(global-set-key (kbd "C-c C-n C-e") 'e/org-roam-note-list-plain-exclude)
(global-set-key (kbd "C-c j") 'e/region-remove-spaces)
(global-set-key [left-margin mouse-1]  'mouse-goto-bol)
(global-set-key [left-fringe mouse-1]  'mouse-goto-bol)


(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)


;;** Modeline


;; (setq mode-line-format
;; '("%e"
;;   mode-line-front-space
;;   mode-line-mule-info
;;   mode-line-client
;;   mode-line-modified
;;   mode-line-remote
;;   mode-line-frame-identification
;;   mode-line-buffer-identification
;;   "   " mode-line-position
;;  (vc-mode vc-mode)
;;  "  " mode-line-modes
;;  mode-line-misc-info
;;  mode-line-end-spaces))

;; Display date and time in bright on dark face.

(require 'time)
(defface mode-line-time-face
   '((((type x w32 mac))
      (:foreground "#00ffff" :background "#000000" :inherit bold))
     (((type tty))
      (:foreground "blue")))
   "Face used to display the time in the mode line.")

(setq display-time-string-forms '((propertize
       (concat " " day "." month "." year " | " 24-hours ":" minutes " ")
       'face
       'mode-line-time-face)))


;;** Set global colors and custom faces

;; (set-background-color "#454545") ;; Gimp gui color
;; (set-background-color "#656565") ;; neutral gray (Geomean of 60:1)
;; This makes everything transparent, including the text.
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (set-foreground-color "white")

(defun e/set-background-dark ()
  (interactive)
  (set-background-color "#454545") ;; Gimp gui color
  (set-foreground-color "white"))

;; #ee82ee =^= violet
;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#ee82ee")
(set-face-attribute 'region nil :background "#826271")
;; (set-face-attribute 'mode-line nil  :height 80)
(set-face-foreground 'fringe "dark gray")
(set-face-background 'fringe "gray")
;; (setq fringe-mode '(40 . 40))
;; (set-fringe-mode '(40 . 40))

;; Set global font size for emacs *1/10pt, 110=11pt
(set-face-attribute 'default nil :height 120)


;;** Set variables display and frame

(setq frame-resize-pixelwise t
      display-time-24hr-format t
      display-time-day-and-date t
      line-spacing 0.0)


;;** Set variables desktop

(setq desktop-restore-frames nil
      desktop-restore-in-current-display t
      desktop-save-mode t)


;;** Set variables misc

(require 'browse-url)
(require 'markdown-mode)
(setq ansi-color-faces-vector
        [default default default italic
         underline success warning error]
      ansi-color-names-vector
        ["#242424" "#e5786d" "#95e454" "#cae682"
         "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
      backup-directory-alist
        '(("." . "/home/dan/backup/emacs-auto-save"))
      backup-by-copying t
      delete-old-versions t
      tab-bar-close-last-tab-choice 'tab-bar-mode-disable
      delete-by-moving-to-trash t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      delete-selection-mode t
      use-dialog-box nil
      browse-url-firefox-new-window-is-tab t
      browse-url-new-window-flag t
      compilation-ask-about-save nil
      completions-format 'vertical
      completion-ignore-case t
      eval-expression-print-length 1000
      custom-enabled-themes nil
      custom-safe-themes
        '( "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710"
           default)
      dynamic-completion-mode t
      electric-indent-mode t
      display-buffer-alist
      '(("*Async Shell Command*" (display-buffer-no-window (nil)))
        ("*Shell Command Output*" (display-buffer-no-window (nil)))
        ("*Root Info Command*" (display-buffer-no-window (nil)))
        ("concat\\.org" .
         (
          ( display-buffer-reuse-window display-buffer-pop-up-frame) . ;; action functions
          (( reusable-frames . t)) ;; alist
          ))
          ;; ("*Help*\\|*Faces*" .
          ;;  (( display-buffer-reuse-window display-buffer-pop-up-frame) .
          ;;   (( reusable-frames . t))))
        )
      garbage-collection-messages nil
      markdown-list-indent-width 3
      pcomplete-autolist t
      mouse-autoselect-window t
      scroll-bar-adjust-thumb-portion nil
      scroll-conservatively 0
      show-trailing-whitespace nil
      comint-buffer-maximum-size 2048
      standard-indent 0
      use-file-dialog nil
      user-mail-address "dj@danieljohannsen.de"
      word-wrap t
      text-quoting-style 'grave) ;; fix for describe command error


(add-to-list 'default-frame-alist '( width . 100))
(add-to-list 'default-frame-alist '( height . 20))


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(add-to-list 'magic-mode-alist '("<\\?xml" . nxml-mode))
(add-to-list 'imagemagick-types-inhibit 'CUT)
(imagemagick-register-types)


;;** Set dir locals

;; Define a read-only directory class
(dir-locals-set-class-variables 'read-only
 '(( nil . (( buffer-read-only . t)))))


;; Associate directories with the read-only class
(dir-locals-set-directory-class (file-truename "~/archive") 'read-only)


;;** Environment variables

(setenv "PATH" (concat "~/bin:" (getenv "PATH")))


;;** Spell checking


;;     spc accepts the word, this time only,
;;     i   accepts the word and inserts it in your personal dictionary,
;;     a   accepts the word for this session,
;;     A   accepts the word for this file, and inserts it in the local file dictionary
;;     r   allows you to correct the word by hand
;;     R   allows you to correct all the occurrences of the misspelled word,
;;     x   stops the checking, and puts the cursor back in place,
;;     X   stops the checking and leaves the cursor where it is, letting you correct your file; you will be able to continue the spell-checking later if you type Meta-x ispell-continue,
;;     ?   gives you online help.


;; hunspell aliase:
;;  ("american" "/usr/share/hunspell/en_US.dic" "/usr/share/hunspell/en_US.aff")
;;  ("english" "/usr/share/hunspell/en_US.dic" "/usr/share/hunspell/en_US.aff")
;;  ("en_US" "/usr/share/hunspell/en_US.dic" "/usr/share/hunspell/en_US.aff"))
;;  ("deutsch" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("deutsch8" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("german" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("german8" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("de_DE" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")


;; The default personal dictionary depends on the locale settings.
;; The following environment variables are searched:
;; LC_ALL, LC_MESSAGES, and LANG.
;; If none  are  set  then  the  default  personal  dictionary  is
;; $HOME/.hunspell_default.
;; Setting -d or  the DICTIONARY environmental variable, personal dictionary will be
;; $HOME/.hunspell_dicname

;; Example
;; -d en_US,en_geo,en_med,de_DE,de_med
;; Meaning:
;; Base dictionary: en_US, Personal dictionaries: en_geo, en_med
;; Base dictionary: de_DE, Personal dictionaries: de_med

;; ispell-program-name: Program invoked by M-$ and M-x ispell-region commands.
;; ispell-dictionary: Default dictionary to use if `ispell-local-dictionary' is nil (nil -> use hunspell standard dictionary).
;; ispell-local-dictionary: Buffer local dictionary
;; ispell-personal-dictionary: Personal dictionary (nil -> use hunspell standard location $HOME/.hunspell_dicname)
;;    $HOME/.hunspell_default.  Default path to personal dictionary.
;; ispell-dictionary-alist: (nil) List of ispell-dictionaries
;; ispell-local-dictionary-alist: (nil) Override definitions in ispell-dictionary-alist
;; ispell-alternate-dictionary: ("/usr/share/dict/words") Plain word-list dictionary for spelling help
;; ispell-complete-word-dict: (nil) Plain word-list dictionary used for word completion if different from `ispell-alternate-dictionary'.
(require 'ispell)
(setq ispell-program-name "hunspell"
      ispell-dictionary "en_US"
      ispell-dictionary-alist
      '(("en_US" ; DICTIONARY-NAME used by ispell-dictionary
         "[[:alpha:]]" ; CASECHARS valid characters that comprise a word
         "[^[:alpha:]]" ; NOT-CASECHARS is the opposite regexp of CASECHARS
         "[']" ; OTHERCHARS from NOT-CASECHARS parsed as part of a word in special cases
         nil ; MANY-OTHERCHARS-P only one OTHERCHAR is allowed in a single word
         ("-d" "en_US") ; ISPELL-ARGS contains ACTUAL parameters passed to hunspell ("-d" "en_US,en_US-med")
         nil ; EXTENDED-CHARACTER-MODE For example, umlauts can be encoded as \"a, a\", "a, ...
         utf-8) ; CHARACTER-SET text coding sent to hunspell when the text contains non-ASCII characters))
        ("de_DE" ; DICTIONARY-NAME used by ispell-dictionary
         "[[:alpha:]]" ; CASECHARS valid characters that comprise a word
         "[^[:alpha:]]" ; NOT-CASECHARS is the opposite regexp of CASECHARS
         "[']" ; OTHERCHARS from NOT-CASECHARS parsed as part of a word in special cases
         nil ; MANY-OTHERCHARS-P only one OTHERCHAR is allowed in a single word
         ("-d" "de_DE") ; ISPELL-ARGS contains ACTUAL parameters passed to hunspell ("-d" "en_US,en_US-med")
         nil ; EXTENDED-CHARACTER-MODE For example, umlauts can be encoded as \"a, a\", "a, ...
         utf-8)) ; CHARACTER-SET text coding sent to hunspell when the text contains non-ASCII characters))
)
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^"))


;;** Visual Line Mode and Fill Column


(global-visual-line-mode t)
(setq-default fill-column 80)


;;* Emacs mode specific


;;** General mode info
;; How Emacs chooses modes:
;; 1. file-local mode variable
;; 2. file's contents begin with ‘#!’
;; 3. text at the start of the buffer, based on the variable *magic-mode-alist*
;; 4. file's name *auto-mode-alist*


;;** Cfold minor mode


(push "~/.emacs.d/lisp/cfold" load-path)
(require 'cfold)


;;** Doxy (minor/major) modes

;;*** Define variables


(defvar doxyfile ""
  "Set this variable to the doxygen project name. The
configuration file for doxygen is then 'doxyfile.doxy' (the
extension '.doxy' is added to the project name by default).")
(make-variable-buffer-local 'doxyfile)
(put 'doxyfile 'safe-local-variable #'stringp)


(defvar doxy-page-file-patterns '("md" "html" "dox")
  "Valid file patterns for pure documentation files (no source
  code files).")


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
    (LaTeX-environment-list-filtered)
    ))


(defvar doxy-common-mode-map
  (let (( map (make-sparse-keymap))
        ( menu-entry-compile '(menu-item "Compile Doxygen"
                                         doxy-compile-buffer
                                         :help
"Compile project.
Configuration file should be specified as local variable,
doxyfile: \"~/path/to/project-configuration.doxy\"."))
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
  (add-file-local-variable 'doxyfile new)
  (set-buffer-modified-p t)
  (save-buffer)
  (revert-buffer t t nil)))


(defun doxy-config-file ()
  (let* (( default-dir (file-name-as-directory (getenv "HOME")))
         ( conf-file (if (local-variable-p 'doxyfile)
                         (expand-file-name doxyfile)
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


;;** SrcDoc minor mode (blogging about source code)

(push "~/blog/srcdoc" load-path)
(require 'srcdoc)


;;** Outline major/minor mode


;;*** Set variables


(setq outline-blank-line t)


;;*** Function for level determination


(defun outline-minor-mode-level ()
  (let (( beg (match-beginning 1))
        ( end (match-end 1))
        ( key (match-string 2))
        ( modified-alist (copy-tree outline-heading-alist)))
    (when key
      (setq key (string-trim key))
      (when case-fold-search
        (setq key (downcase key))
        (dolist ( key-level modified-alist)
          (setf (car key-level) (downcase (car key-level))))))
    (or (cdr (assoc key modified-alist))
        (- end beg))))


;;*** Function for fontification


(defun outline-fontify-headline ( limit)
  (when (re-search-forward (concat "^" outline-regexp) nil t)
    (when (match-string 1)
      (let (( beg (match-beginning 0))
            ( end (line-end-position))
            ( outline-face (outline-font-lock-face))
            ( level (outline-minor-mode-level))
            ( bg (face-attribute 'default :background)))
        (remove-text-properties beg end
                '( font-lock-face nil
                   face nil
                   invisible nil))
        (add-text-properties beg end
                `( face ,outline-face))
        (put-text-property beg (+ beg (1+ level))
              'face `(:foreground ,bg)))
      t)))


;;*** Function fold state


(defun outline-folded-p ()
  "Returns non-nil if point is on a folded headline or plain list
item."
  (and (outline-on-heading-p)
       (invisible-p (line-end-position))))


(defun outline-save-fold-state ()
  (setq outline-heading-fold-states nil)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (when (looking-at outline-regexp)
        (setq outline-heading-fold-states
              (cons (outline-folded-p)
                    outline-heading-fold-states)))
      (while (outline-next-heading)
             (setq outline-heading-fold-states
                   (cons (outline-folded-p)
                         outline-heading-fold-states))))))


(defun outline-restore-fold-state ()
  (when outline-heading-fold-states
    (save-excursion
      (goto-char (point-max))
      (save-match-data
        (while (outline-previous-heading)
          (if (pop outline-heading-fold-states)
              (outline-hide-subtree)
            (outline-show-entry)))))))


;;*** Function mouse control


(defun outline-mouse-hideshow-subtree ( event)
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (if (outline-folded-p)
               (outline-show-entry)
               (outline-hide-entry)))))


;; Redefine outline original function
(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (setq invisible-ok t)
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (looking-at outline-regexp))))


;; Redefine outline original function
(defun outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (setq invisible-ok t)
  (beginning-of-line)
  (or (outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				    nil t)
                (error "Before first heading"))
	    (setq found (and (or invisible-ok (not (outline-invisible-p)))
			     (point)))))
	(goto-char found)
	found)))


(defun outline-mouse-toggle-children ( event)
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (outline-toggle-children))))
;; FIXME blog post about font-lock-flush and alike functions


(defun outline-mouse-show-all ( event)
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))) file)
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (outline-show-all))))


(defun outline-first-heading-level ()
  (save-excursion
    (goto-char (point-min))
    (outline-next-heading)
    (outline-minor-mode-level)))


(defun outline-mouse-hide-sublevels ( event)
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))) file)
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (if (member major-mode '( outline-mode obuffer-mode))
               (outline-hide-sublevels 1)
             (outline-hide-sublevels (outline-first-heading-level))))))


;;*** Function for promoting/demoting headings


(defun outline-parent-alist ()
  (let (( min-level 80)
          headings)
    (save-excursion
      (while (outline-previous-heading)
        (looking-at (concat "^" outline-regexp))
        (let* (( re-prefix (concat "^\s*\\|"
                                   (regexp-quote
                                    (concat
                                     (substring
                                      (match-string-no-properties 0)
                                      0 2)
                                     (match-string-no-properties 1)))))
               ( level (outline-minor-mode-level))
               ( title (concat (propertize (make-string level ?*)
                                           'face font-lock-comment-face)
                               " "
                               (replace-regexp-in-string re-prefix ""
                                 (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))))
          (when (< level min-level)
            (setq headings (cons (cons title level) headings)
                  min-level level)))))
    headings))


(defun outline-set-level ()
  (interactive)
  (beginning-of-line)
  (looking-at (concat "^" outline-regexp))
  (when (match-string 1)
    (let* (( char (string-to-char (match-string-no-properties 1)))
           ( re-char (regexp-quote (string char)))
           ( prefix (regexp-quote
                     (substring (match-string-no-properties 0) 0 2)))
           ( parents (outline-parent-alist))
           ( level (1+ (cdr (assoc
                             (minibuffer-with-setup-hook
                                 #'minibuffer-completion-help
                               (completing-read "Parent heading: " parents))
                             parents)))))
      (looking-at (concat "^" prefix "\\(" re-char "+\\)"))
      (replace-match (make-string level char) t nil nil 1))))


;;*** Hooks and key bindings


(add-hook 'outline-minor-mode-hook
  (lambda ()
    (setq-local outline-level 'outline-minor-mode-level)
    (font-lock-add-keywords nil
                            '((outline-fontify-headline 0 nil append t))
             'append)
    (define-key outline-minor-mode-map (kbd "<S-mouse-2>")
      'outline-mouse-toggle-children)
    (define-key outline-minor-mode-map (kbd "<C-M-mouse-2>")
      'outline-mouse-show-all)
    (define-key outline-minor-mode-map (kbd "<C-M-mouse-3>")
      'outline-mouse-hide-sublevels)
    (define-key outline-minor-mode-map (kbd "<M-up>")
      'outline-move-subtree-up)
    (define-key outline-minor-mode-map (kbd "<M-down>")
      'outline-move-subtree-down)
    (define-key outline-minor-mode-map (kbd "C-c <right>")
      'outline-set-level)))


(add-hook 'outline-mode-hook
  (lambda ()
    (define-key outline-mode-map (kbd "<S-mouse-2>")
      'outline-mouse-toggle-children)
    (define-key outline-mode-map (kbd "<C-M-mouse-2>")
      'outline-mouse-show-all)
    (define-key outline-mode-map (kbd "<C-M-mouse-3>")
      'outline-mouse-hide-sublevels)
    (define-key outline-mode-map (kbd "<M-up>")
      'outline-move-subtree-up)
    (define-key outline-mode-map (kbd "<M-down>")
      'outline-move-subtree-down)))


;;** Org mode


;; org export to markdown:
(require 'ox-md)


;;*** Org Babel


;; make file executable after tangle process
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
                        :foreground "dim gray" :background nil)
    (set-face-attribute 'org-drawer nil
                        :foreground fg
                        :height 0.7
                        :inherit 'org-document-info-keyword)
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


;;*** Publish projects


(org-link-set-parameters
 "emacs"
 :follow 'e/org-cite-open-file
 :export 'e/org-emacs-export
 :face 'org-link)


(setq org-publish-project-alist
  '(("projects-to-html"
     :base-directory "~/.emacs.d/org/projects"
     :publishing-directory "~/public_html/projects"
     :base-extension "org"
     :publishing-function org-html-publish-to-html
     :auto-sitemap t
     :sitemap-title "Projects"
     :sitemap-format-entry e/org-publish-sitemap-entry
     :recursive t
     :headline-levels 3
     :preparation-function e/org-publish-preparation-function
     :completion-function e/org-publish-completion-function
     :html-link-home "http://localhost:8080/sitemap.html"
     :html-link-up "http://localhost:8080/sitemap.html")
    ("projects-to-pdf"
     :base-directory "~/.emacs.d/org/projects"
     :publishing-directory "~/notes/pdfs"
     :base-extension "org"
     :publishing-function org-latex-publish-to-pdf
     :completion-function e/org-publish-completion-pdf
     :auto-sitemap nil
     :recursive t)
    ("projects-static"
     :base-directory "~/.emacs.d/org/projects"
     :base-extension "css\\|js\\|png\\|jpe?g\\|gif\\|webp\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory "~/public_html/projects"
     :recursive t
     :auto-sitemap nil
     :publishing-function org-publish-attachment)
    ("projects" :components ("projects-static" "projects-to-html"))
    ("roam-to-html"
     :base-directory "~/.emacs.d/org-roam"
     :publishing-directory "~/public_html/org-roam"
     :base-extension "org"
     :publishing-function org-html-publish-to-html
     :html-link-home "http://localhost/~dan/org-roam/"
     :html-link-up ""
     :preparation-function e/org-publish-preparation-function
     :completion-function e/org-publish-completion-function
     :with-toc nil
     :with-broken-links mark
     :auto-sitemap nil
     :recursive t
     :headline-levels 3)
    ("roam-static"
     :base-directory "~/.emacs.d/org-roam"
     :base-extension "css\\|js\\|png\\|jpe?g\\|gif\\|webp\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory "~/public_html/org-roam"
     :recursive t
     :auto-sitemap nil
     :publishing-function org-publish-attachment)
    ("roam" :components ("roam-static" "roam-to-html")))
  org-export-with-timestamps t
  org-export-with-drawers nil
  org-export-with-tags nil)


;; pandoc test.org -f org -t html -s -o test.html --bibliography /home/dan/library/database/reference.bib --citeproc --mathjax=""


(setq org-html-head "<!-- org-html-head begin -->
<script>
  MathJax = {
    tex: {
      tags: 'ams',  // should be 'ams', 'none', or 'all'
      inlineMath: [['$', '$']]
    }
  };
</script>
<script
  id=\"MathJax-script async for align environments\"
  async
  src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\">
</script>
<!-- org-html-head end -->"
      org-html-style-begin "<!-- org-html-head-extra begin -->\n<style>\n"
      org-html-style-end "</style>\n<!-- org-html-head-extra end -->"
      org-html-style-main
      "  /* org-html-style-main begin */
  #content { max-width: 800px; margin: auto;}
  img { max-width: 100%; }
  h2 { font-size: 1.3em; }
  h3 { font-size: 1.15em; }
  h4 { font-size: 1em; }
  h5, h6 { font-size: 1em; }
  blockquote { background-color: #e3e3e3; padding: 10px; border-radius: 3px;}
  blockquote p:first-child { margin: 0px; }
  blockquote p:last-child { margin: 0px; }
  .thumbs { display: flex; flex-wrap: wrap; }
  .thumbs .figure { padding: 0.2em; }
  .thumbs img { max-height: 150px; }
  #preamble {text-align: center; border-bottom: 1px solid; padding-bottom: 16px;}
  #preamble a {font-size: 1.3em; font-weight: bold; text-decoration: none;}
  #preamble ul {list-style-type: none; display:flex; justify-content: center; padding: 0px; margin-bottom: 0px;}
  #preamble ul li {margin: 0px 5px; color: gray; font-size: 0.8em;}
  #postamble {border-top: 1px solid;}
  #nav { display: inline-block; }
  .title { display: none; }
  .link-section { border-top: 1px solid; }
  .link-section h2 { font-size: 1em; }
  .active {font-weight: bold;}
  /* org-html-style-main end */\n"
      org-html-head-extra
      (concat org-html-style-begin
              org-html-style-main
              "  /* org-html-style-comic begin */
  .outline-3 h3 {margin-bottom: 5px; margin-top: 20px;}
  .outline-text-3 b {font-weight: normal;}
  .outline-text-3 p {margin: 5px auto;}
  .org-dl {display: grid; grid-template-columns: max-content auto;
           margin-left: 20px; margin-top: 3px; grid-gap: 3px;}
  .org-dl dt {font-weight: normal; grid-column-start: 1;}
  .org-dl dt::after {content: \":\"; margin-left: 3px;}
  .org-dl dd {grid-column-start: 2;}
  /* org-html-style-comic end */\n"
              org-html-style-end))


(setenv "PATH" (concat "/home/dan/.nvm/versions/node/v18.15.0/bin:"
                       (getenv "PATH")))
(defun e/org-publish-live-server ( &optional dir open)
  (unless dir
    (setq dir "~/public_html"))
  (unless (get-process "live-server")
    (let (( default-directory dir)
          ( command (concat "live-server"
                            (when open (concat " --open=" open)))))
      (start-process-shell-command "live-server" nil command)
      (message "%s" command))
    t))


(defun e/org-publish-preparation-function ( plist)
  ;; Update sitemap always
  ;; (org-publish-remove-all-timestamps)
  (setq org-export-with-section-numbers nil)
  (setq e/org-publish-base-directory (expand-file-name (plist-get plist :base-directory))
        e/org-publish-html-link-home (plist-get plist :html-link-home)
        e/org-publish-html-link-up (plist-get plist :html-link-up))
  ;; (add-hook 'org-export-before-processing-functions #'e/org-export-navbar)
  ;; (add-hook 'org-export-before-processing-functions #'e/org-roam-links-update)
  )


(defun e/org-publish-completion-function ( plist)
  ;; (let (( publishing-directory (plist-get plist :publishing-directory)))
  ;;   (e/org-publish-live-server publishing-directory))
  (setq org-export-with-section-numbers t)
  ;; (remove-hook 'org-export-before-processing-functions #'e/org-export-navbar)
  ;; (remove-hook 'org-export-before-processing-functions #'e/org-roam-links-update)
  )


(defun e/org-publish-completion-pdf ( plist)
  (dolist ( base-file (org-publish-get-base-files (cons "dummy" plist)))
    (let (( pdf-file (concat (file-name-sans-extension base-file) ".pdf")))
      (delete-file pdf-file t)
      (message "Removed: %s" pdf-file))))


(defun e/org-publish-get-html-project ( &optional filename)
  (unless filename
    (setq filename (buffer-file-name (buffer-base-buffer))))
  (let ((project (org-publish-get-project-from-filename filename 'up)))
    (when project
      (let ( project-plist)
        (if (eq (org-publish-property :publishing-function project)
                'org-html-publish-to-html)
            (setq project-plist project)
          (dolist ( name (org-publish-property :components project))
            (let (( subproject (assoc name org-publish-project-alist)))
              (when (eq (org-publish-property :publishing-function subproject)
                        'org-html-publish-to-html)
                (setq project-plist subproject)))))
        project-plist))))


(defun e/org-publish-current-project ()
  (interactive)
  (let (( project (e/org-publish-get-html-project)))
    (unless project
      (user-error "Buffer not part of any publishing project"))
    (save-buffer)
    ;; (e/org-publish-live-server (expand-file-name
    ;;                             (org-publish-property
    ;;                              :publishing-directory
    ;;                              project)))
    (org-publish-current-project nil t)))


(defun e/org-publish-current-file ( &optional project file)
  (unless file
    (setq file (buffer-file-name (buffer-base-buffer))))
  (unless project
    (setq project (e/org-publish-get-html-project file)))
  (when project
    (let (( plist (cdr project))
          ( publishing-directory (expand-file-name
                                  (org-publish-property
                                   :publishing-directory
                                   project)))
          ( preparation-function (org-publish-property
                                  :preparation-function
                                  project))
          ( completion-function (org-publish-property
                                 :completion-function
                                 project)))
      (funcall preparation-function plist)
      (org-publish-file file)
      (e/org-roam-publish-attachments plist publishing-directory)
      (funcall completion-function plist))))


(defun e/org-publish-check-file ( &optional file project)
  (unless file
    (setq file (buffer-file-name (buffer-base-buffer))))
  (unless project
    (setq project (e/org-publish-get-html-project file)))
  (let (( base-directory (expand-file-name
                          (org-publish-property
                           :base-directory
                           project)))
        ( publishing-directory (expand-file-name
                                (org-publish-property
                                 :publishing-directory
                                 project))))
    (unless (file-exists-p
             (concat (file-name-as-directory publishing-directory)
                     (concat
                      (file-name-sans-extension
                       (file-relative-name file base-directory))
                      ".html")))
      (e/org-publish-current-file project file))))


(defun e/org-roam-publish-after-save ()
  (when (org-roam-buffer-p)
    (let (( id (org-id-get (point-min)))
          ( project (e/org-publish-get-html-project)))
      (when project
        (let (( plist (cdr project))
              ( base-directory (expand-file-name
                                (org-publish-property
                                 :base-directory
                                 project)))
              ( publishing-directory (expand-file-name
                                      (org-publish-property
                                       :publishing-directory
                                       project)))
              ( preparation-function (org-publish-property
                                      :preparation-function
                                      project))
              ( completion-function (org-publish-property
                                     :completion-function
                                     project)))
          ;; (e/org-publish-live-server
          ;;  publishing-directory
          ;;  (concat
          ;;   (file-name-sans-extension
          ;;    (file-relative-name (buffer-file-name (buffer-base-buffer))
          ;;                        base-directory))
          ;;   ".html#ID-" id))
          (e/org-publish-current-file project))))))


(defun e/org-roam-publish-attachments ( plist pub-dir)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (when (string-match-p "^file:" (match-string 1))
        (message "e/org-roam-publish-attachments: %s" (match-string 1))
        (let* (( filename (replace-regexp-in-string
                           "^file:" "" (match-string-no-properties 1)))
               ( rel-dir (file-name-directory filename)))
          (message "e/org-roam-publish-attachments: %s %s %s" filename pub-dir rel-dir)
          (org-publish-attachment plist filename
                                  (concat (file-name-as-directory pub-dir)
                                          rel-dir)))))))


(defun e/org-publish-base-files ()
  (let ( base-files)
    (dolist (project org-publish-project-alist)
      (let (( id (car project))
            ( base-directory (plist-get (cdr project) :base-directory)))
        (when base-directory
          (setq base-files
                (append base-files
                        (seq-filter (lambda ( path) (string= (file-name-extension path) "org"))
                                    (org-publish-get-base-files project)))))))
    (delete-dups base-files)
    base-files))


(defun e/org-publish-sitemap-entry ( entry style project)
  "Custom format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
	 (format "[[file:%s][%s]]"
		 entry
		 (e/org-publish-find-title entry project)))
	((eq style 'tree)
	 ;; Return only last subdir.
	 (file-name-nondirectory (directory-file-name entry)))
	(t entry)))


(defun e/org-publish-find-title ( file project)
  "Find the title of FILE in PROJECT."
  (let (( file (org-publish--expand-file-name file project))
        ( sitemap-title (org-publish-cache-get-file-property file :title nil t)))
    (unless sitemap-title
	  (let* (( parsed-title (car (org-publish-find-property file :title project)))
             ( parsed-subtitle (car (org-publish-find-property file :subtitle project 'latex)))
             ( parsed-title-subtitle (list (concat parsed-title
                                                   (and parsed-subtitle " - ")
                                                   parsed-subtitle)))
	         ( title (if parsed-title
		                 (org-no-properties
		                  (org-element-interpret-data parsed-title-subtitle))
		               (file-name-nondirectory (file-name-sans-extension file)))))
	      (setq sitemap-title (org-publish-cache-set-file-property file :title title))))
    sitemap-title))


(defun e/org-emacs-export ( path description back-end export-channel)
  (pcase back-end
    ('html (concat "<a href=\"emacs:id:" path "\">"
                   description
                   "</a>"))
    (_ path)))


(defun e/org-publish-link-to-html ( link &optional name)
  (when (and link (string-match org-link-bracket-re link))
    (let* (( type-id (save-match-data
                       (string-split (match-string 1 link) ":")))
           ( type (car type-id))
           ( id (cadr type-id))
           ( description (match-string 2 link))
           ( file (caar (org-roam-db-query [:select [file]
                                                      :from nodes
                                                      :where (= id $s1)]
                                           id)))
           ( relative-file (when file
                             (file-name-sans-extension
                              (file-relative-name file e/org-publish-base-directory)))))
      (concat "<a href=\"" relative-file ".html"
              "#ID-" id
              "\">" (or name description "link") "</a>"))))


(defun e/org-publish-html-prev-next ( id)
  (let (( nodes (sort (org-roam-db-query [:select [title id file]
                                                  :from nodes])
                      (lambda ( a b)
                        (string< (car a) (car b)))))
        prev current next)
    (while (and nodes (not (string= (cadr current) id)))
      (setq prev current
            current (pop nodes)
            next (car nodes)))
    (let (( prev-file (when prev
                            (file-name-sans-extension
                             (file-relative-name (caddr prev)
                                                 e/org-publish-base-directory))))
          ( next-file (when next
                            (file-name-sans-extension
                             (file-relative-name (caddr next)
                                                 e/org-publish-base-directory)))))
      (list (when prev
              (concat "<a class=\"nav-btn\" href=\"" prev-file ".html"
                      "#ID-" (cadr prev)
                      "\">" (car prev) "&lt;</a>"))
            (when next
              (concat "<a class=\"nav-btn\" href=\"" next-file ".html"
                      "#ID-" (cadr next)
                      "\">&gt;" (car next) "</a>"))))))


(defun e/org-export-navbar ( backend)
  (let* (( id (org-id-get (point-min)))
         ( title (cadar (org-collect-keywords '("TITLE"))))
         ( prev-next-links (e/org-publish-html-prev-next id)))
    (setq org-html-preamble-format
          `(("en" ,(concat "<div id=\"nav\">"
                           (or (car prev-next-links) "none")
                           "\n<a class=\"nav-btn active\" href=\"emacs:id:" id "\">" title "</a>\n"
                           (or (cadr prev-next-links) "none")
                           "</div>"))))))


(defun e/org-roam-export-preamble ( backend)
  (setq org-html-preamble-format '(("en" "")))
  (when (and (eq backend 'html) (org-id-get (point-min)))
    (let* (( id (org-id-get (point-min)))
           ( title (cadar (org-collect-keywords '("TITLE"))))
           ( filetags (cadar (org-collect-keywords '("FILETAGS"))))
           ( listtags (when filetags
                      (string-join
                       (mapcar (lambda ( tag) (concat "<li>#" tag "</li>"))
                               (split-string filetags
                                             ":" 'OMIT-NULLS))))))
      (setq org-html-preamble-format
            `(("en" ,(concat "\n<a href=\"emacs:id:" id "\">" title "</a><br>\n"
                             (when listtags
                               (concat "<ul>" listtags "</ul>")))))))))


;;*** Extended org: Convenient insert commands


(defun e-org-insert-macro ()
  "Insert a macro at point.

Use active region as argument for the macro."
  (interactive)
  (let (( beg (point))
        argument)
    (when (use-region-p)
      (setq beg (region-beginning)
            argument (delete-and-extract-region beg
                                                (region-end))))
    (insert (concat "{{{(" argument ")}}}"))
    (goto-char (+ beg 3))))


(defun e-org-choose-macro ()
  "Insert macro at point or region."
  (interactive)
  (let (( macro "")
        ( beg (point))
        ( end (point))
        macros)
    (if (use-region-p)
        ;; With a region, the argument is assumed to be known
        (let (( region (buffer-substring (region-beginning)
                                         (region-end))))
          (setq beg (region-beginning)
                end (region-end))
          (add-to-list 'macros (concat "{{{(" region ")}}}"))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "#\\+macro: \\([A-Za-z]+\\)"
                                      nil t)
              (add-to-list 'macros (concat "{{{"
                                           (match-string-no-properties 1) "("
                                           region
                                           ")}}}")))))
      ;; No region, the argument is gathered from existing macros
      (setq macros '("{{{}}}" "{{{()}}}"))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat "{{{\\([A-Za-z]+\\)(\\([^{}]*\\))}}}"
                                          "\\|#\\+macro: \\([A-Za-z]+\\)")
                                  nil t)
          (if (match-string-no-properties 3)
              (add-to-list 'macros (concat "{{{" (match-string-no-properties 3) "()}}}"))
            (add-to-list 'macros
                         (replace-regexp-in-string "\n" " "
                                                   (match-string-no-properties 0)))))))
    ;; All macros are gathered - now provide the list to choose from
    (setq macro (completing-read "Choose macro: " macros nil t "{{{"))
    (delete-region beg end)
    (insert macro))
  ;; Place point at convenient position
  (re-search-backward "{{{" (line-beginning-position) t)
  (if (or (looking-at "{{{}}}") (looking-at "{{{("))
      (re-search-forward "{{{" (line-end-position) t)
    (if (looking-at "{{{[A-Za-z]+()}}}")
        (re-search-forward "{{{[A-Za-z]+(" (line-end-position) t)
      (re-search-forward "}}}" (line-end-position) t))))


(defun e-org-choose-link ()
  "Insert link at point or region."
  (interactive)
  (let (( macro "")
        ( beg (point))
        ( end (point))
        macros)
    (if (use-region-p)
        ;; With a region, the argument is assumed to be known
        (let (( region (buffer-substring (region-beginning)
                                         (region-end))))
          (setq beg (region-beginning)
                end (region-end))
          (add-to-list 'macros (concat "[[:][" region "]]"))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "<<\\([A-Za-z0-9]+\\):>>"
                                      nil t)
              (add-to-list 'macros (concat "[["
                                           (match-string-no-properties 1) ":"
                                           (replace-regexp-in-string "\n\\| " "_" region)
                                           "][" region "]]")))))
      ;; No region, the argument is gathered from existing link
      (setq macros '("[[:][]]"))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat "\\[\\[\\([A-Za-z0-9]+\\):\\([^]]*\\)\\]\\[\\([^]]*\\)\\]\\]"
                                          "\\|<<\\([A-Za-z0-9]+\\):>>")
                                  nil t)
          (if (match-string-no-properties 4)
              (add-to-list 'macros (concat "[[" (match-string-no-properties 4) ":][]]"))
            (add-to-list 'macros
                         (replace-regexp-in-string "\n" " "
                                                   (match-string-no-properties 0)))))))
    ;; All macros are gathered - now provide the list to choose from
    (setq macro (completing-read "Choose link: " macros nil nil "[["))
    ;; For adding new links, the description part can be omitted
    (when (string-match ":\\([^]]*\\)\\'" macro)
      (setq macro (concat macro "]["
                          (replace-regexp-in-string "_" " " (match-string 1 macro))
                          "]]")))
    (delete-region beg end)
    (insert macro)))


(defun e-org-num-format-last ( NUMBERING)
  "Format org-num-mode string by only the last number.

Take NUMBERING and use only the last element."
  (let* (( str (number-to-string (car (last NUMBERING))))
         ( str-len (length str)))
    (concat
     ;; (when (< str-len 2)
     ;;   (make-string (- 2 str-len) ? ))
     str " ")))


(defun e-org-filter-headline ()
  "Extract string with link structure from headline.

Point is at the heading.  Link structure is
identifier:link_string (no spaces)."
  (let (( headline (nth 4 (org-heading-components))))
    (string-match "[A-Za-z0-9]+:[[:alnum:]_]+" headline)
    (match-string 0 headline)))


(defun e-org-get-headlines ( keyword)
  "Get a list of all headlines under a parent heading.

The parent heading is specified by a KEYWORD, normally of the
form <<identifier:>> which is given somewhere directly under the
parent heading.

Start search for parent heading from beginning of buffer.

Get only identifier:link_string part of the headline"
  (save-excursion
    (when keyword
      (goto-char (point-min))
      (re-search-forward keyword nil t))
    (let (( level (+ (car (org-heading-components)) 1))
          headlines)
      (while (and (re-search-forward "^\\*+" nil t)
                  (= (car (org-heading-components)) level))
        (setq headlines (cons (e-org-filter-headline) headlines)))
      headlines)))


(defun e-org-insert-heading ( keyword link)
  "Find heading with KEYWORD and insert LINK as subheading.

KEYWORD is often given in the form <<identifier:>> mentioned
somewhere in the content under the headline.

Start search for parent heading from beginning of buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward keyword nil t)
      (let (( links (e-org-get-headlines keyword)))
        (unless (member link links)
          (org-insert-heading-respect-content)
          (org-do-demote)
          (insert " " link))))))


(defun e-org-headlines-from-links ()
  "Create headings according to links given in the text.

Start from beginning of buffer and gather all

         identifier:link_string

texts from the links (remember structure of hyperlinks:
[[identifier:link_string][description]]).

In the second step extract identifier: string from link strings
and then find parent heading with <<identifier:>> keyword
somewhere in the parent heading content. Insert
identifier:link_string as subheading, if not already present."
  (interactive)
  (let ( types links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([A-Za-z0-9]+:[^]]*\\)\\]\\[\\([^]]*\\)\\]\\]" nil t)
        (add-to-list 'links (match-string-no-properties 1)))
      ;; Leaves point after last link
      (dolist (link links)
        (string-match "^[A-Za-z0-9]+:" link)
        (e-org-insert-heading (concat "<<" (match-string 0 link) ">>")
                              link)))))


(defun e-org-occur ()
  "Create sparse tree according to special construct at point.

The special construct is e.g. keyword:identifier."
  (interactive)
  (unless (looking-at "[] .,;\n]")
    (save-excursion
      (if (looking-at "[^[]*\\]")
          (re-search-backward "\\[\\[" nil t)
        (re-search-backward "[ \n]" nil t))
      (when (looking-at "[[ \n]*\\([A-Za-z0-9]+:[[:alnum:]_]+\\)")
        (org-occur (concat "\\[" (match-string 1) "\\]") t)))))


(defun e-org-count-headings-per-link ( link keyword)
  "Count headings containing at least one LINK.

Count only headings where the last word is KEYWORD."
  (let (( count 0)
        ( subcount 0))
    (save-excursion
      (when (re-search-forward (concat "^\\(\\*+\\).*" keyword "[ \n]") nil t)
        (while (re-search-forward (concat "\\(\\[" link "\\]\\)"
                                          "\\|^\\(\\*+\\).*" keyword "[ \n]")
                                  nil t)
          (if (match-string 2)
              (setq count (+ count (if (> subcount 0) 1 0))
                    subcount 0)
            (setq subcount (+ subcount 1))))
        (setq count (+ count (if (> subcount 0) 1 0)))
        count))))


(defun e-org-link-list-add-count ()
  "Update counts in headlines of subheadings.

Point is inside the parent heading. Counts have the structure

      X/Y

where X and Y are integers, where X is being updated. Only
headings are counted, which contain a specified keyword at the
end of the headline."
  (interactive)
  (let (( level (+ (car (org-heading-components)) 1))
        ( keyword (completing-read "Headline keyword: "
                                   '("Scene" "Paragraph") nil nil "")))
    (while (and (re-search-forward "^\\*+ " nil t)
                (= (car (org-heading-components)) level))
      (let* (( link (e-org-filter-headline))
             ( count (number-to-string
                      (e-org-count-headings-per-link link keyword))))
        (if (re-search-forward "\\([0-9]+\\)/[0-9]+ "
                               (line-end-position) t)
            (replace-match count t t nil 1)
          (insert count))))))


(defun e-org-count-headings ()
  "Count headings having a special keyword.

If point is on a number, replace that number with the headings
count. Otherwise insert count at point."
  (interactive)
  (let (( keyword (completing-read "Headline keyword: "
                                   '("Scene" "Paragraph") nil nil ""))
        ( count 0))
    (save-excursion
      (while (re-search-forward (concat "^\\(\\*+\\).*" keyword "[ \n]") nil t)
        (setq count (+ count 1))))
    ;; (beginning-of-line)
    ;; (when (re-search-forward "\\([0-9]+\\)/[0-9]+ "
    ;;                          (line-end-position) t)
    ;;   (replace-match (number-to-string count) t t nil 1))
    (if (not (looking-at "[[:digit:]]"))
        (insert (number-to-string count))
      (re-search-backward "[^[:digit:]]" nil t)
      (re-search-forward "[[:digit:]]+" nil t)
      (replace-match (number-to-string count) t t nil 0))))


(defun e-org-count-words ()
  "Count words in current subtree."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (beginning-of-line 2)
    (let (( mes (call-interactively 'count-words)))
      (deactivate-mark)
      (message mes))))


(defun e-org-copy-link ()
  (interactive)
  (let* (( element (org-element-context))
         (type (org-element-type element))
         (beg (org-element-property :begin element))
         (end (org-element-property :end element)))
    (when (eq type 'link)
      (copy-region-as-kill beg end))))


(defun e-org-cut-link ()
"Cut org-link at point and save it in the kill ring.

Kill ('cut') text between point and mark.  This deletes the text
from the buffer and saves it in the kill ring.  The command C-y
can retrieve it from there."
  (interactive)
  (let* (( element (org-element-context))
         ( type (org-element-type element))
         ( beg (org-element-property :begin element))
         ( end (org-element-property :end element)))
    (when (eq type 'link)
      (when (= (char-before end) 32)
        (setq end (1- end)))
      (kill-region beg end))))


(defun e/org-remove-link ()
  (interactive)
  (let* (( element (org-element-context))
         ( type (org-element-type element))
         ( beg (org-element-property :begin element)))
    (when (eq type 'link)
        (goto-char beg)
        (re-search-forward org-bracket-link-regexp (line-end-position) t)
        (replace-match (match-string 2)))))


(defun e/org-link-description ()
  (interactive)
  (let* (( element (org-element-context))
         ( type (org-element-type element))
         ( beg (org-element-property :contents-begin element))
         ( end (org-element-property :contents-end element)))
    (when (eq type 'link)
      (buffer-substring-no-properties beg end))))


(defun e/org-insert-drawer-comment ()
  (interactive)
  (insert ":COMMENT:\n"
          "- REFERENCES :: \n"
          "- NOTES :: \n"
          ":END:\n"))


(defun e/org-insert-block-quote ()
  (interactive)
  (if (not (region-active-p))
      (insert "#+begin_quote\n#+end_quote")
    (goto-char (region-end))
    (when (re-search-backward "[^\s\n]" nil t)
      (goto-char (1+ (point))))
    (insert "\n#+end_quote")
    (goto-char (region-beginning))
    (beginning-of-line)
    (insert "#+begin_quote\n")))


(defun e/org-tags-visible-headings-add ()
  (interactive)
  (let (( tag (read-string "Add Tag: "))
        c)
    (org-toggle-tag tag 'on)
    (org-next-visible-heading 1)
    (while (not (= 113 (setq c (read-char "Continue (quit with q) y? "))))
      (when (= 121 c)
        (org-toggle-tag tag 'on))
      (org-next-visible-heading 1))))


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
          ;; (add-hook 'after-save-hook 'e/org-roam-copy-id-after-save nil 'local)
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
      (setq aliases (remove title (add-to-list 'aliases description 'append)))
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


;;*** Org Roam Browser Actions


(defun e/org-live-server-open-file ( file id)
  (let* (( project (e/org-publish-get-html-project file))
         ( base-directory (expand-file-name
                           (org-publish-property
                            :base-directory
                            project)))
         ( publishing-directory (expand-file-name
                                 (org-publish-property
                                  :publishing-directory
                                  project))))
    (e/org-publish-check-file file project)
    (e/org-publish-live-server
     publishing-directory
     (concat
      (file-name-sans-extension
       (file-relative-name file base-directory))
      ".html#ID-" id))))


(defun e/org-roam-browser-url ( file id)
  (let* (( project (e/org-publish-get-html-project file))
         ( base-directory (expand-file-name
                           (org-publish-property
                            :base-directory
                            project)))
         ( publishing-directory (expand-file-name
                                 (org-publish-property
                                  :publishing-directory
                                  project))))
    (concat org-roam-html-server-url
            (file-name-sans-extension
             (file-relative-name file base-directory))
            ".html#ID-" id)))


(defun e/org-browser-open-file ( file id &optional running-live-server-only)
  (or (e/org-live-server-open-file file id)
      (shell-command
       (concat "firefox " 
               (e/org-roam-browser-url file id)))))


(defun e/org-roam-browser-open-id ( id)
  (let (( file (expand-file-name
                (caar (org-roam-db-query [:select [file]
                                                  :from nodes
                                                  :where (= id $s1)]
                                         id)))))
    (e/org-browser-open-file file id)))


(defun e/org-roam-browser-node-find ()
  (interactive)
  (let* (( completing-read-function 'ivy-completing-read)
         ( node (org-roam-node-read)))
    (if (org-roam-node-file node)
        (e/org-roam-browser-open-id (org-roam-node-id node))
      (org-roam-capture-
       :node node
       :templates org-roam-capture-templates
       :props '(:finalize find-file)))))


(defun e/org-roam-browser-open-link ()
  (let* (( element (org-element-context))
         ( type (org-element-property :type element))
         ( path (org-element-property :path element)))
    (when (string= type "id")
      (e/org-roam-browser-open-id  path))))


(defun e/org-roam-browser-open-current ()
  (interactive)
  (e/org-roam-browser-open-id (org-id-get (point-min))))


(defun e/org-roam-browser-open-link-next ( &optional SEARCH-BACKWARD)
  (interactive)
  (when (org-next-link SEARCH-BACKWARD)
    (e/org-roam-browser-open-link)))


(defun e/org-roam-browser-open-link-previous ()
  (interactive)
  (e/org-roam-browser-open-link-next 'SEARCH-BACKWARD))
  

(defun e/org-roam-browser-open-link-at-mouse ( event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (if (eq (car (org-element-context)) 'link)
          (e/org-roam-browser-open-link)
        (e/org-roam-browser-open-current)))))


;;*** Comic Script


(defun org-comic-lighter-string ()
  (setq org-comic-lighter
        (concat " comic["
                (number-to-string org-comic-page-count) "|"
                (number-to-string org-comic-panel-count) "]")))


(defvar-local org-comic-lighter nil
  "This variable holds the per-buffer word-count statistics used to
update the modeline.")


;; (setq org-comic-timer-tracker
;;       (run-with-idle-timer
;;        0 t
;;        '(lambda ()
;;           (when (org-comic-heading-p)
;;             (setq org-comic-lighter (org-comic-lighter-string))))))


(define-minor-mode org-comic-mode
  "Toggle use of org mode comic fontification."
  :init-value nil
  :lighter (:eval org-comic-lighter)
  (cond (org-comic-mode
         (org-comic-enumerate)
         (add-hook 'after-change-functions #'org-comic-after-change nil 'local)
         (add-hook 'org-export-before-processing-functions
                   #'org-comic-export-before-processing nil 'local)
         (add-hook 'org-ctrl-c-ctrl-c-hook #'org-comic-new-character nil 'local)
         (font-lock-fontify-buffer)
         )
        (t
         (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-comic-new-character 'local)
         (remove-hook 'after-change-functions #'org-comic-after-change 'local)
         (remove-hook 'org-export-before-processing-functions
                      #'org-comic-export-before-processing 'local)
         (remove-overlays (point-min) (point-max) 'name 'org-comic)
         (org-comic-display-upcase-remove)
         (font-lock-fontify-buffer))))


(defvar-local org-comic-page-count 0)


(defvar-local org-comic-panel-count 0)


(defun org-comic-heading-p ()
  (and org-comic-mode
       (save-excursion
         (beginning-of-line)
         (when (and (looking-at org-complex-heading-regexp)
                    (match-beginning 4))
           (goto-char (match-beginning 4))
           (when (looking-at "p\\|\\[")
             (re-search-forward "\\(page\\|panel\\)" (match-end 4) t))))))


(defun org-comic-after-change ( beg end _)
  (when (org-comic-heading-p)
    (org-comic-enumerate)))


(defun org-comic-enumerate ( &optional export)
  (remove-overlays (point-min) (point-max) 'name 'org-comic)
  (let (( page 0)
        ( panel 0)
        ( panelTotal 0))
    (org-map-entries
     (lambda ()
       (when (org-comic-heading-p)
         (let* (( face (get-text-property (line-beginning-position) 'face))
                ( overlay (make-overlay (match-beginning 1)
                                        (match-end 1))))
           (overlay-put overlay 'name 'org-comic)
           (cond ((string= (downcase (match-string 1)) "page")
                  (setq page (1+ page)
                        panel 0)
                  (if export
                      (replace-match (concat "PAGE " (number-to-string page))
                                     t nil nil 1)
                    (overlay-put overlay 'after-string
                                 (concat " " (propertize (number-to-string page)
                                                         'font-lock-face face)))))
                 ((string= (downcase (match-string 1)) "panel")
                  (setq panel (1+ panel)
                        panelTotal (1+ panelTotal))
                  (if export
                      (replace-match (concat "PANEL " (number-to-string panel))
                                     t nil nil 1)
                    (overlay-put overlay 'after-string
                                 (concat " " (propertize (number-to-string panel)
                                                         'font-lock-face face))))))))))
    (setq org-comic-page-count page
          org-comic-panel-count panelTotal)
    (org-comic-lighter-string)))


(defun org-comic-export-before-processing ( backend)
  (org-comic-enumerate 'export))
  

(defun org-comic-get-characters ()
  (let (( current-point (point))
        characters)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-list-full-item-re nil t)
        (when (and (match-string 4)
                   (or (< current-point (match-beginning 4))
                       (< (match-end 4) current-point )))
          (setq characters (cons (upcase (match-string-no-properties 4))
                                 characters))))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\([A-Z][^*]+[A-Z]\\)\\*" nil t)
        (when (and (match-string 1)
                   (or (< current-point (match-beginning 1))
                       (< (match-end 1) current-point )))
          (setq characters (cons (upcase (match-string-no-properties 1))
                                 characters)))))
    (delq nil (delete-dups characters))))


(defvar org-comic-ac-default-dictionary
  '("PAGE" "PANEL" "CAPTION" "SFX" "NARRATION"))


(defvar org-comic-panel-descriptors
  '("CLOSE-UP" "LONG SHOT" "DETAIL" "INT." "EXT."
    "DAY" "NIGHT" "LOW-ANGLE" "HIGH-ANGLE" "BIRD'S-EYE"
    "OVER-THE-SHOULDER" "ESTABLISHING"))


(defvar org-comic-caption-descriptors
  '("TIME/PLACE" "VOICE OVER"))


(defvar org-comic-dialogue-descriptors
  '("OFF" "WHISPER" "BURST" "WEAK" "SINGING" "THINKING"))


(defun org-comic-set-ac-dictionary ( &optional remove)
  "Provide the word list for auto-completion.

Set local variable `ac-buffer-dictionary' to the value of
`org-comic-get-characters'. `ac-buffer-dictionary' is then
referenced by variable `ac-source-dictionary', which in turn must
be added to local variable `ac-sources' list."
  (if remove
      (setq ac-buffer-dictionary nil
            ac-sources '(ac-source-words-in-same-mode-buffers))
    (setq ac-buffer-dictionary (append org-comic-ac-default-dictionary
                                       org-comic-panel-descriptors
                                       org-comic-caption-descriptors
                                       org-comic-dialogue-descriptors
                                       (org-comic-get-characters))
          ac-sources '( ac-source-dictionary))))


(defun org-comic-new-character ()
  (interactive)
  (atomic-change-group
    (beginning-of-line)
    (unless (looking-at "^\s*$")
      (if (org-at-item-p)
          (org-end-of-item)
        (end-of-paragraph-text)
        (beginning-of-line 2)))
    (insert "\n")
    (beginning-of-line 0)
    (let (( character (completing-read "Character: "
                                       (org-comic-get-characters)))
          ( descriptor (completing-read "Descriptor: "
                                        (append org-comic-caption-descriptors
                                                org-comic-dialogue-descriptors))))
      (insert "- " (upcase character)
              (if (string-empty-p descriptor)
                  ""
                (concat " (" (upcase descriptor) ")"))
              " :: ")))
  t)

  
(defun org-comic-new-character-inline ()
  (interactive)
  (insert "*" (completing-read "Character: "
                               (org-comic-get-characters))
          "*"))


(defun org-comic-fontify-links-old ( limit)
  (when org-comic-mode
    (let ( match)
      (while (and (setq match (text-property-search-forward 'face))
                  (< (point) limit))
        (let (( start (prop-match-beginning match))
              ( end (prop-match-end match))
              ( value (prop-match-value match)))
          (if (listp value)
              (when (member 'org-link value)
                (remove-text-properties start end '(face nil))
                (setq value (delete 'org-link value))
                (dolist ( face value)
                  (add-face-text-property start end face 'append)))
            (when (eq value 'org-link)
              (remove-text-properties start end '(face nil)))))))))


(defun org-comic-display-upcase ( start end)
  (while (< start end)
    (compose-region start (1+ start)
                    (upcase (char-to-string (char-after start))))
    (setq start (1+ start))))


(defun org-comic-display-upcase-remove ()
  (save-excursion
    (goto-char (point-min))
    (let ( match)
      (while (and (setq match (text-property-search-forward 'display))
                  (string= (upcase (buffer-substring (1- (point)) (point)))
                           (prop-match-value match)))
        (remove-text-properties (1- (point)) (point) '(display nil))))))
                          

(defun org-comic-fontify-links ( limit)
  (when org-comic-mode
    (let ( success)
      (while (setq success (re-search-forward org-bracket-link-regexp limit t))
        (let* (( beg (match-beginning 0))
               ( end (match-end 0))
               ( face (get-text-property beg 'face))
               ( beg2 (match-beginning 2))
               ( end2 (match-end 2)))
          (if (and (listp face)
                   (member 'org-link face))
              (let (( add-faces (delete 'org-link face)))
                (remove-text-properties beg end '(face nil))
                (dolist ( item add-faces)
                  (add-face-text-property beg end item 'append)))
            (when (eq face 'org-link)
              (remove-text-properties beg end '(face nil))))
          (when (match-string 2)
            (org-comic-display-upcase beg2 end2))))
      success)))
            

(defun org-comic-upcase-links ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp nil t)
      (when (match-string 2)
        (replace-match (upcase (match-string 2)) t nil nil 2)))))


(defun org-comic-upcase-links-overlay ()
  (when (re-search-forward org-bracket-link-regexp (line-end-position) t)
    (when (match-string 2)
      (let (( overlay (make-overlay (match-beginning 2) (match-end 2))))
        (overlay-put overlay 'name 'org-comic-upcase)
        (overlay-put overlay 'display (upcase (match-string 2)))))))
  

(defun org-comic-font-lock-add-keywords ()
  (add-to-list 'org-font-lock-extra-keywords
               '( org-comic-fontify-links ( 0 nil append t))
               'append))


(add-hook 'org-font-lock-set-keywords-hook
          #'org-comic-font-lock-add-keywords)


;;*** Bibliography, Reference, DOI, ISBN


(defun e/org-collect-doi-links ()
  (goto-char (point-min))
  (let (( url-mime-accept-string "text/x-bibliography;style=apa")
        dois bibs)
    (while (re-search-forward org-link-bracket-re nil t)
      (when (string-match-p "doi\\.org" (match-string 1))
        (let (( doi (match-string-no-properties 1))
              ( fn (car (org-footnote-at-definition-p))))
          (if (setq bib (assoc doi dois))
              (let (( footnotes (cdr bib)))
                (setq footnotes (cons fn footnotes))
                (map-put! dois doi footnotes 'equal))
            (setq dois (cons (cons doi (list fn)) dois))))))
    (dolist (doi dois)
      (setq bibs (cons (list
                        (nreverse (cdr doi))
                        (with-temp-buffer
                          (url-insert-file-contents (car doi))
                          (buffer-substring (point-min) (point-max))))
                       bibs)))
    bibs))

    
(defun e/org-collect-doi-links-bibtex ()
  (goto-char (point-min))
  (let (( url-mime-accept-string "text/x-bibliography;style=bibtex")
        dois bibs)
    (while (re-search-forward org-link-bracket-re nil t)
      (when (string-match-p "doi\\.org" (match-string 1))
        (let (( doi (match-string-no-properties 1))
              ( fn (car (org-footnote-at-definition-p))))
          (if (setq bib (assoc doi dois))
              (let (( footnotes (cdr bib)))
                (setq footnotes (cons fn footnotes))
                (map-put! dois doi footnotes 'equal))
            (setq dois (cons (cons doi (list fn)) dois))))))
    (dolist ( doi dois)
      (setq bibs (cons (list
                        (nreverse (cdr doi))
                        (e/org-collect-doi-links-clean
                         (with-temp-buffer
                           (url-insert-file-contents (car doi))
                           (bibtex-parse-entry))))
                       bibs)))
    bibs))


(defun e/org-collect-doi-links-clean ( entry)
  (mapcar (lambda ( field)
            (cons (car field) (string-trim (cdr field) "{" "}")))
          entry))


(defun e/org-collect-doi-links-authors ( field-string)
  (setq field-string (string-trim field-string "{" "}"))
  (let (authors-list)
    (dolist ( author (split-string field-string "and" 'omit-nulls "\s"))
      (let (( name-parts (split-string author "," 'omit-nulls "\s")))
        (setq authors-list
              (cons (string-join (list (cadr name-parts) (car name-parts)) " ")
                    authors-list))))
    (string-join (nreverse authors-list) ", ")))


(defun e/org-collect-doi-links-html ()
  (let (( bibs (e/org-collect-doi-links-bibtex))
        ( slug (cadar (org-collect-keywords '("SLUG"))))
        ( num 0))
    (switch-to-buffer-other-window "*HTML References*")
    (erase-buffer)
    (web-mode)
    (insert "<ol>\n")
    (dolist ( bib bibs)
      (setq num (1+ num))
      (insert "  <li id=\"" (number-to-string num) "\">\n")
      (let (( footnotes (car bib))
            ( alist (cadr bib)))
        (insert "<!-- Footnotes: "  (string-join footnotes " ") " -->\n")
        (when slug
          (insert "<!-- Slug: " slug "#" (number-to-string num) " -->\n"))
        (insert (e/org-collect-doi-links-authors (cdr (assoc "author" alist))) ".\n")
        (insert "<b>" (cdr (assoc "title" alist)) "</b>.\n")
        (when (cdr (assoc "journal" alist))
          (insert (cdr (assoc "journal" alist)) ", "))
        (when (cdr (assoc "year" alist))
          (insert (cdr (assoc "year" alist)) "; "))
        (when (cdr (assoc "volume" alist))
          (insert (cdr (assoc "volume" alist)) " "))
        (when (cdr (assoc "number" alist))
          (insert "(" (cdr (assoc "number" alist)) ")\n"))
        (insert "DOI: <a href=\"https://doi.org/" (cdr (assoc "DOI" alist)) "\">")
        (insert (cdr (assoc "DOI" alist)) "</a>\n")
        )
      (insert "  </li>\n"))
    (insert "</ol>\n")))
        

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


(require 'org-superstar)


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
    (require 'org-colview)
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


(require 'org-journal)


(setq org-journal-dir "~/.emacs.d/journal/")


(add-hook 'org-journal-after-entry-create-hook
  (lambda ()
    (save-excursion
      (beginning-of-line 0)
      (unless (looking-at "^\s*$")
        (beginning-of-line 2)
        (insert "\n")))))


;;*** org-download


(require 'org-download)


;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


(setq org-download-display-inline-images nil
      org-download-link-format "[[file:%s][file:%s]]\n"
      org-download-link-format-function 'e/org-download-link-format-function)


(defun e/org-download-link-format-function (filename)
  "Custom function."
  (if (and (>= (string-to-number org-version) 9.3)
           (eq org-download-method 'attach))
      (format "[[attachment:%s]]\n"
              (org-link-escape
               (file-relative-name filename (org-attach-dir))))
    (replace-regexp-in-string "%s" 
                              (org-link-escape
                               (funcall org-download-abbreviate-filename-function
                                        filename))
                              org-download-link-format)))


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
    

;;*** org-insert-url from grab-x-link


(defun grab-x-link--shell-command-to-string (command)
  (with-temp-buffer
    (if (and (zerop (call-process-shell-command command nil t))
             (> (buffer-size) 0))
        (substring (buffer-string) 0 -1)
      nil)))


(defun grab-x-link--title-strip (string suffix)
  "Remove SUFFIX from STRING."
  (cond ((< (length string) (length suffix)) string)
        ((string= (substring string (- (length string) (length suffix))) suffix)
         (substring string 0 (- (length suffix))))
        (t string)))


(defun grab-x-link--get-clipboard ()
  (if (display-graphic-p)
      ;; NOTE: This function is obsolete since 25.1
      (x-get-clipboard)
    (cond ((executable-find "xsel") (grab-x-link--shell-command-to-string "xsel --clipboard"))
          ((executable-find "xclip") (grab-x-link--shell-command-to-string "xclip -selection clipboard -o"))
          (t (error "Can't get clipboard because xsel or xclip is not installed")))))


(defun grab-x-link-firefox ()
  (let (( emacs-window
          (grab-x-link--shell-command-to-string
           "xdotool getactivewindow"))
        ( firefox-window
          (or (grab-x-link--shell-command-to-string
               "xdotool search --classname Navigator")
              (error "Can't detect Firefox Window -- is it running?"))))
    ;; (shell-command (format "xdotool windowactivate --sync %s key ctrl+l ctrl+c" firefox-window))
    ;; (shell-command (format "xdotool search --class firefox key --window %s ctrl+l ctrl+c" firefox-window))
    (shell-command (format "xdotool key --window %s ctrl+l ctrl+c" firefox-window))
    (shell-command (format "xdotool key --window %s Left Escape" firefox-window))
    (sit-for 0.2)
    (shell-command (format "xdotool windowactivate %s" emacs-window))
    (sit-for 0.2)
    (let (( url (substring-no-properties (grab-x-link--get-clipboard)))
          ( title (grab-x-link--title-strip
                   (grab-x-link--shell-command-to-string
                    (concat "xdotool getwindowname " firefox-window))
                   " – Mozilla Firefox"))) ;; this is an EN DASH!
      (cons url title))))


(defun e/org-insert-link-url-firefox ()
  (interactive)
  (let* (( url-title (grab-x-link-firefox))
         ( url (car url-title))
         ( title (cdr url-title))
         ( domain (url-host (url-generic-parse-url url))))
    (insert (org-link-make-string url domain))))


(defun e/org-insert-link-url-firefox-heading ()
  (interactive)
  (let* (( url-title (grab-x-link-firefox))
         ( url (car url-title))
         ( title (cdr url-title))
         ( domain (url-host (url-generic-parse-url url))))
    (insert (concat "* " domain))
    (org-set-property "url"
                      (org-link-make-string url domain)))
  (re-search-forward ":END:\n" nil t))


;;*** org-roam


(require 'org-roam)
(require 'org-roam-export)

 
;;**** Set variables, hook, ivy usage


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
  (let (( candidate (org-roam-node-title node))
        ( title (caar (org-roam-db-query [:select [title]
                                                  :from nodes
                                                  :where (= id $s1)]
                                         (org-roam-node-id node)))))
    (if (string= candidate title)
        ""
      (propertize (concat " " title) 'face 'warning))))


(setq org-roam-directory (file-truename "~/.emacs.d/org-roam/")
      org-roam-base-url "http://localhost/~dan/org-roam/"
      org-roam-html-server-url "http://127.0.0.1:8080/"
      ivy-use-selectable-prompt t
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
    

;;**** ivy actions


(defun e/org-roam-ivy-preview ( x)
  (let (( node (get-text-property 0 'node x)))
    (when node
      (display-buffer
       (find-file-noselect (org-roam-node-file node)))
      ;; (e/org-browser-open-file (org-roam-node-file node)
      ;;                          (org-roam-node-id node))
      )))


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


(defun e/org-roam-ivy-exit-no-action ( x)
  (message "Exiting ivy without action."))


(defun e/org-roam-ivy-done ()
  (interactive)
  ;; (if (ivy--prompt-selected-p)
  ;;     (ivy-immediate-done)
  ;;   (ivy-exit-with-action 'e/org-roam-ivy-exit-no-action))
  (e/ivy-done)
  )


(defun e/ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (if (ivy--prompt-selected-p)
      (ivy-immediate-done)
    (setq ivy-current-prefix-arg current-prefix-arg)
    (let ((require-match (ivy-state-require-match ivy-last))
          (input (ivy--input)))
      (delete-minibuffer-contents)
      (cond ((and (= ivy--length 0)
                  (eq this-command 'ivy-dispatching-done))
             (message "10")
             (ivy--done ivy-text))
            ((or (> ivy--length 0)
                 ;; the action from `ivy-dispatching-done' may not need a
                 ;; candidate at all
                 (eq this-command 'ivy-dispatching-done))
             ;; in case of chosing existing node
             (message "20")
             ;; (ivy--done (ivy-state-current ivy-last))
             (ivy-exit-with-action 'e/org-roam-ivy-exit-no-action)
             )
            ((string= " (confirm)" ivy--prompt-extra)
             (message "30")
             (ivy--done ivy-text))
            ((or (and (memq (ivy-state-collection ivy-last)
                            '(read-file-name-internal internal-complete-buffer))
                      (eq confirm-nonexistent-file-or-buffer t))
                 (and (functionp require-match)
                      (setq require-match (funcall require-match))))
             (message "40")
             (setq ivy--prompt-extra " (confirm)")
             (insert input)
             (ivy--exhibit))
            ((memq require-match '(nil confirm confirm-after-completion))
             ;; in case of new node
             (message "50")
             (ivy--done ivy-text))
            (t
             (setq ivy--prompt-extra " (match required)")
             (message "60")
             (insert ivy-text)
             (ivy--exhibit))))))


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


(require 'org-roam-dailies)


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


;;*** org-transclusion


(require 'org-transclusion)


(set-face-attribute
 'org-transclusion-fringe nil
 :foreground "dark green"
 :background "dark green")


(setq org-transclusion-include-first-section nil)
;; (delete '(keyword "transclude") org-roam-db-extra-links-exclude-keys)


;;*** org-zet


(setq e/org-zet-file-extensions '(".org" ".md" ".txt"))


(defun e/org-zet-file-exists-p ( basename)
  (let (( extensions e/org-zet-file-extensions)
        found filename)
    (while (and (not found)
                extensions)
      (when (file-exists-p (setq filename (concat basename (pop extensions))))
        (setq found t)))
    found))


(defun e/org-zet-new-filename ()
  (let (( basename (file-name-base))
        ( extension (file-name-extension (buffer-name) 'period))
        filename numbers)
    (while (string-match "^\\([0-9]+\\)-?" basename)
      (setq numbers (cons (string-to-number (match-string 1 basename)) numbers)
            basename (replace-match "" nil nil basename 0)))
    (setq numbers (cons (1+ (pop numbers)) numbers))
    (dolist ( n numbers)
      (setq basename (concat "-" (number-to-string n) basename)))
    (setq basename (string-trim basename "-"))
    (if (not (e/org-zet-file-exists-p basename))
        (concat basename extension)
      (setq basename (concat (file-name-base) "-1"))
      (while (e/org-zet-file-exists-p basename) 
        (setq basename (concat basename "-1")))
      (concat basename extension))))


(defun e/org-zet-new ()
  )
      
  
;;** Calender settings

 
(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))
(calendar-set-date-style 'european)
(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map "\C-x\C-x" 'obuffer-open)))

(require 'calendar)
(set-face-attribute 'calendar-today nil :box t :underline nil)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;;*** Set variables

(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"]
      calendar-month-abbrev-array ["Jan" "Feb" "Mär" "Apr" "Mai"
                                 "Jun" "Jul" "Aug" "Sep"
                                 "Okt" "Nov" "Dez"]
      solar-n-hemi-seasons '("Frühlingsanfang" "Sommeranfang"
                             "Herbstanfang" "Winteranfang")
      holiday-general-holidays '((holiday-fixed 1 1 "Neujahr")
                                 (holiday-fixed 5 1 "1. Mai")
                                 (holiday-fixed 10 3 "Tag der Deutschen Einheit"))
      holiday-christian-holidays '((holiday-float 12 0 -4 "1. Advent" 24)
                                   (holiday-float 12 0 -3 "2. Advent" 24)
                                   (holiday-float 12 0 -2 "3. Advent" 24)
                                   (holiday-float 12 0 -1 "4. Advent" 24)
                                   (holiday-fixed 12 25 "1. Weihnachtstag")
                                   (holiday-fixed 12 26 "2. Weihnachtstag")
                                   (holiday-fixed 1 6 "Heilige Drei Könige")
                                   (holiday-easter-etc -48 "Rosenmontag")
                                   (holiday-easter-etc  -2 "Karfreitag")
                                   (holiday-easter-etc   0 "Ostersonntag")
                                   (holiday-easter-etc  +1 "Ostermontag")
                                   (holiday-easter-etc +39 "Christi Himmelfahrt")
                                   (holiday-easter-etc +49 "Pfingstsonntag")
                                   (holiday-easter-etc +50 "Pfingstmontag")
                                   (holiday-easter-etc +60 "Fronleichnam")
                                   (holiday-fixed 8 15 "Mariae Himmelfahrt")
                                   (holiday-fixed 11 1 "Allerheiligen")
                                   (holiday-float 11 3 1 "Buss- und Bettag" 16)
                                   (holiday-float 11 0 1 "Totensonntag" 20)))
 

;;** Dired mode and auto-revert mode

;;*** Require

 
(require 'dired-x)
;; (require 'dired+)


;;*** Set variables

(setq dired-dwim-target t
      dired-isearch-filenames t
      truncate-lines t
      ;; dired-listing-switches "-alht --group-directories-first"
      dired-listing-switches "-alh"
      ;; dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$"
      ;; dired-omit-files "^\\.?#\\|^\\.$"
      ;; dired-omit-files (concat dired-omit-files "\\|^\\.[^.].+$")
      auto-revert-verbose nil
      dired-omit-verbose nil)


;;*** Function definitions

(defun dired-select-other-pane ()
  (interactive)
  (other-window 1))


(defun dired-open-file ()
  (interactive)
  (let (( path (dired-get-filename)))
    (open-file-choice path)))


(defun dired-mouse-open-file ( event)
  (interactive "e")
  (mouse-set-point event)
  (dired-open-file))


(defun dired-mouse-2-button ( event)
  (interactive "e")
  (mouse-set-point event)
  ;; (dired-find-file)
  (dired-find-alternate-file))


(defun dired-mouse-3-button ( event)
  (interactive "e")
  (mouse-set-point event)
  (dired-find-file-other-window))


(defun kill-all-dired-buffer ()
  (interactive)
  (let ( kills)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (string= major-mode "dired-mode")
          (setq name (buffer-name buf))
          (add-to-list 'kills name t))))
    (dolist (bufName kills)
      (message "Kill dired buffer: %s" bufName)
      (kill-buffer (get-buffer bufName)))))


(defun dired-fullscreen-one-pan-view ()
  (interactive)
  (let (( dirPath default-directory)
        ( file (file-name-nondirectory (buffer-file-name))))
    (other-window 1)
    (kill-all-dired-buffer)
    (dired dirPath)
    (re-search-forward (regexp-quote file) nil t)))


(defun dired-one-pan-view-file ()
  (interactive)
  (let (( file (buffer-name))
        ( dir default-directory))
       (dired dir)
       (revert-buffer)
       (goto-char (point-min))
       (re-search-forward file)))


(defun dired-fullscreen-two-pan-view ( &optional dir)
  (interactive)
  (unless dir
    (setq dir default-directory))
  (kill-all-dired-buffer)
  (dired dir)
  (mouse-delete-other-windows)
  (i3-fullscreen-by-killing)
  (split-window-right))


(defvar dired-ls-switches
  '("-lhat --group-directories-first"
    "-lha --group-directories-first"
    "-lh --group-directories-first"))


(defun dired-ls-cycle-switches ()
  (interactive)
  (let* (( rest (member dired-listing-switches dired-ls-switches))
         ( next (if (= (length rest) 1)
                    (car dired-ls-switches)
                  (cadr rest))))
    (dired-sort-other
     (setq dired-listing-switches (if next
                                      next
                                    "-lh --group-directories-first")))))
    


;;*** Running hook

(add-hook 'dired-mode-hook
  (lambda ()
    (hl-line-mode)
    ;; (turn-on-gnus-dired-mode)
    (define-key dired-mode-map [tab] 'dired-select-other-pane)
    (define-key dired-mode-map "\C-co" 'dired-open-file)
    (define-key dired-mode-map "\M-h" 'dired-ls-cycle-switches)
    (define-key dired-mode-map (kbd "<mouse-2>")
      'dired-mouse-2-button)
    (define-key dired-mode-map (kbd "<mouse-3>")
      'dired-mouse-3-button)
    (define-key dired-mode-map (kbd "<S-mouse-2>")
      'dired-up-directory)
    (define-key dired-mode-map (kbd "<C-M-mouse-3>")
               'dired-mouse-open-file)))
 

;;** IBuffer mode


;;*** Require


(require 'ibuffer)


;;*** Define variables


(defvar ibuffer-default-groups-set "group0"
  "Default set of groups after initialization.")


(defvar ibuffer-static-groups-sets
  '(("group0"
     ("doc"
       (filename . "/doc/"))
     ("latex-include"
       (filename . "/tex/include/"))
     ("python-include"
       (filename . "/py/include/"))
     ("fortran-include"
       (filename . "/for/include/"))
     ("latex" (or
       (mode . latex-mode)
       (mode . TeX-output-mode)
       (mode . bibtex-mode)
       (name . "\\.aux\\'")
       (name . "^\\*TeX Help\\*$")))
     ("python"
      (mode . python-mode))
     ("fortran" (or
      (mode . fortran-mode)
      (mode . f90-mode)))
     ("php"
      (mode . web-mode))
     ("emacs" (or
       (mode . emacs-lisp-mode)
       (mode . lisp-interaction-mode)
       (mode . Custom-mode)
       (mode . Buffer-menu-mode)
       (mode . Info-mode)
       (mode . messages-buffer-mode)
       (mode . help-mode)
       (mode . completion-list-mode)
       (mode . compilation-mode)
       (mode . dired-mode)
       (mode . debugger-mode)
       (mode . grep-mode)
       (mode . locate-mode)
       (mode . calendar-mode)
       (mode . profiler-report-mode)
       (mode . apropos-mode)
       (mode . package-menu-mode)
       (name . "^\\*scratch\\*$")
       (name . "^\\*Messages\\*$")
       (name . "^emacs.org$")))  ;emacs.org before org-mode!
     ("org" (or
       (mode . org-mode)
       (mode . org-agenda-mode)
       (name . "^\\*Org PDF LaTeX Output\\*$")))
     ("shell" (or
       (mode . sh-mode)
       (mode . shell-mode)
       (name . "^\\*Shell Command Output\\*$")))
     ("config" (or
       (mode . conf-xdefaults-mode)
       (mode . conf-unix-mode)
       (mode . conf-space-mode)
       (mode . conf-colon-mode)
       (name . "^.*rc$")))
     ;; ("mail" (or
     ;;   (mode . mu4e-headers-mode)
     ;;   (mode . mu4e-view-mode)
     ;;   (mode . mu4e-compose-mode)
     ;;   (mode . rmail-summary-mode)
     ;;   (mode . rmail-mode)))
     ;; ("contact" (or
     ;;   (mode . bbdb-mode)
     ;;   (name . "^bbdb$")))
     ("input" (or
      (mode . feap-mode)
      (mode . code-aster-mode)
      (mode . elmer-mode)))
     ("text"
      (mode . text-mode))
     ("html"
      (mode . html-mode))
     ("markdown"
      (mode . markdown-mode))
     ("dict"
      (mode . nxml-mode))
     ("docview"
      (mode . doc-view-mode))
     ("gnuplot" (or
      (mode . gnuplot-mode)
      (mode . gnuplot-comint-mode)
      (name . "\\.dat\\'")))
     ("image"
      (mode . image-mode)))))


(defvar ibuffer-groups-sets-formats
  '(("group0" . 0) ("version-control" . 2) ("major-mode" . 1)))


;;*** Function for collecting buffer groups


(defun ibuffer-generate-filter-groups-by-mode ()
  (let ( modes)
    (dolist (buf (buffer-list))
      (setq modes (cons (with-current-buffer buf major-mode)
                        modes)))
    (setq modes (delete-dups modes))
    (let ( groups)
      (dolist (mode modes)
        (setq groups (cons
                      (list (replace-regexp-in-string
                             "-mode$" "" (symbol-name mode))
                            (cons 'mode mode))
                      groups)))
      groups)))


(defun ibuffer-set-groups-sets ()
  (setq ibuffer-saved-filter-groups
        (append ibuffer-static-groups-sets
                `(,(cons "version-control" (ibuffer-vc-generate-filter-groups-by-vc-root)))
                `(,(cons "major-mode" (ibuffer-generate-filter-groups-by-mode))))))


;;*** Set variables


(setq ibuffer-formats
      '(( mark modified read-only " "
          ( name 18 18 :left :elide) " "
          ( size 9 -1 :right) " "
          ( mode 16 16 :left :elide) " "
          filename-and-process)
       ( mark " "
         ( name 43 -1) " "
         filename)
       ( mark modified read-only " "
         ( name 18 18 :left :elide) " "
         ( size 9 -1 :right) " "
         ( mode 16 16 :left :elide) " "
         ( vc-status 16 16 :left) " "
         vc-relative-file)))


(setq ibuffer-show-empty-filter-groups nil
      ibuffer-expert t)


;;*** Function definitions

(defun ibuffer-use-other-window ()
  (interactive)
  (when (< (count-windows) 2)
    (split-window-sensibly))
  (other-window 1)
  (ibuffer))


(defun ibuffer-collapse-all-groups ()
  "Close all groups expanded ibuffer-groups.

Similar in parts to `ibuffer-toggle-filter-group' but this
function will not toggle but close all filter groups."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\[\s\\(.*\\)\s\\]$" nil t)
    (let (( name (match-string 1)))
      (unless (member name ibuffer-hidden-filter-groups)
        (push name ibuffer-hidden-filter-groups))))
  (ibuffer-update nil t))


(defun ibuffer-format-index-by-groups-set ( groups-set)
  (setq ibuffer-current-format
        (or (cdr (assoc groups-set ibuffer-groups-sets-formats))
            0))
  (ibuffer-update-format))


(defun ibuffer-switch-groups-set ()
  "Cycle ibuffer groups sets."
  (interactive)
  (let* (( sets (mapcar 'car ibuffer-saved-filter-groups))
         ( name (or (cadr (member ibuffer-default-groups-set sets))
                    (car sets))))
    (ibuffer-switch-to-saved-filter-groups name)
    (ibuffer-format-index-by-groups-set name)
    (setq-local ibuffer-default-groups-set name)))


(defun ibuffer-open-dwim ()
  "Cycle ibuffer columns formats or open ibuffer window.

If current buffer is not an ibuffer, don't cycle but open ibuffer
window. Otherwise invoking the `ibuffer-switch-format' will
automatically cycle the column formats."
  (interactive)
  (if (not (string= major-mode "ibuffer-mode"))
      (ibuffer)
    (ibuffer-set-groups-sets)
    (ibuffer-switch-groups-set)
    (ibuffer-collapse-all-groups)))


;;*** Functions define-ibuffer...

(define-ibuffer-sorter filename-or-dired
  "Sort the buffers by their pathname."
  (:description "filenames plus dired")
  (string-lessp 
   (with-current-buffer (car a)
     (or buffer-file-name
         (when (eq major-mode 'dired-mode)
           (expand-file-name dired-directory))
         ;; so that all non pathnames are at the end
         "~"))
   (with-current-buffer (car b)
     (or buffer-file-name
         (when (eq major-mode 'dired-mode)
           (expand-file-name dired-directory))
         ;; so that all non pathnames are at the end
         "~"))))


;;*** Hook

(add-hook 'ibuffer-mode-hook
   (lambda ()
     (setq truncate-lines t)
     (ibuffer-set-groups-sets)
     (ibuffer-switch-to-saved-filter-groups "group0")
     (ibuffer-collapse-all-groups)
     (hl-line-mode)
     (font-lock-add-keywords nil
        '(("code-aster" . font-lock-warning-face)))
     (define-key ibuffer-mode-map (kbd "s p")
       'ibuffer-do-sort-by-filename-or-dired)
     (define-key ibuffer-mode-filter-group-map (kbd "<S-mouse-2>")
       'ibuffer-mouse-toggle-filter-group)
     (define-key ibuffer-mode-map (kbd "M-p")
       'previous-buffer)))


;;** OBuffer mode


(push "~/.emacs.d/lisp/obuffer" load-path)
(require 'obuffer)


;;** VC Version control

;;*** Function

(defun vc-dir-open-marked-files ()
  (interactive)
  (dolist ( file (vc-dir-marked-files))
    (find-file-noselect file))
  (vc-dir-unmark-all-files 'all-states)
  (obuffer-update))


(defun vc-dir-delete-marked-files ()
  (interactive)
  (dolist ( file (vc-dir-marked-files))
    (when (y-or-n-p (format "Delete file %s? " file))
      (delete-file file)))
  (vc-dir-refresh)
  (obuffer-update))


(defun vc-dir-unhide ( &optional all)
  (interactive)
  (let* (( root (file-name-as-directory
                 (expand-file-name (vc-root-dir))))
         ( home (file-name-as-directory (expand-file-name "~")))
         ( database (concat home ".obuffer.db"))
         ( files (split-string
                  (shell-command-to-string
                   (concat "locate -d " database
                           " -r " root))
                  "\n" 'omit-nulls))
           regular-files)
    (dolist ( file files)
      (when (and (file-regular-p file)
                 (or all (get-file-buffer file)))
        (setq regular-files (cons file regular-files))))
    (let (( file-count (length regular-files)))
      (message "Resynching %s files..." file-count)
      (let (( message-log-max nil)
            ( i 0))
        (dolist ( file regular-files)
          (setq i (1+ i))
          (message "Resynch file %s of %s..." i file-count)
          (vc-dir-resynch-file file))))))


(defun vc-dir-unhide-all ()
  (interactive)
  (vc-dir-unhide 'all))

;;*** Hooks

(add-hook 'vc-dir-mode-hook
  (lambda ()
    (define-key vc-dir-mode-map (kbd "O") 'vc-dir-open-marked-files)
    (define-key vc-dir-mode-map (kbd "d") 'vc-dir-delete-marked-files)))

(add-hook 'vc-checkin-hook 'obuffer-update)


;;** Buffer menu mode

 
(add-hook 'buffer-menu-mode-hook
   (lambda ()
     (hl-line-mode)
     (font-lock-add-keywords nil
        '(("Emacs.*" . font-lock-warning-face)
          ("Python.*" . font-lock-variable-name-face)
          ("Org.*" . font-lock-constant-face)
          ("LaTeX.*" . font-lock-function-name-face)
          ("Shell.*" . font-lock-warning-face)
          ("Text.*" . font-lock-keyword-face)))))
 

;;** Package list mode


 
(add-hook 'package-menu-mode-hook 'hl-line-mode)
 

;;** Grep mode


;;*** Function for grep region and goto match


(setq grep-base-directory nil)

 
(defun grep-outline-show-all ()
  (with-current-buffer
      (find-file-noselect (thing-at-point 'filename t) t nil)
    (outline-show-all)))


(defun grep-set-base-directory ()
  (interactive)
  (setq grep-base-directory
        (read-directory-name "Grep base directory: "
                             default-directory)))

(defun grep-region ()
  (interactive)
  (unless grep-base-directory
    (grep-set-base-directory))
  (let* (( default-cmd (concat "cd " grep-base-directory
                               "; grep --color -nH -e "
                               (when (region-active-p)
                                 (buffer-substring (region-beginning)
                                                   (region-end)))
                               " -r ."))
         ( cmd (read-from-minibuffer "Run grep (like this): "
                                     default-cmd nil nil 'grep-history)))
    (grep cmd)))


(defun e/grep-absolute-path ()
  (goto-char (point-min))
  (when (re-search-forward "^grep[^/]+/" nil t)
    (file-name-as-directory (thing-at-point 'filename))))


(defun e/grep-show-only-relative-path ()
  (interactive)
  (let* (( path (e/grep-absolute-path))
         overlay)
    (while (re-search-forward path nil t)
      (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                   'invisible t))))


(defun grep-match-open-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '(display-buffer-same-window
           (inhibit-same-window . nil))))
    (compile-goto-error))
  (sleep-for 1)
  (set-buffer (window-buffer))
  (outline-show-all))
 

;;*** Function for mouse button 2 support


(defun grep-mouse-goto-match ( event)
  (interactive "e")
  (let (( window (posn-window (event-end event)))
        ( pos (posn-point (event-end event))))
         (select-window window)
         (goto-char pos)
         (grep-match-open-same-window)))


;;*** Hook


(add-hook 'grep-mode-hook
  (lambda ()
    (message "grep-mode-hook executed")
    ;; (setq truncate-lines t)
    (visual-line-mode 1)
    (message "grep-mode-hook compilation-button-map mouse-2: %s"
      (lookup-key compilation-button-map (kbd "<mouse-2>")))
    (define-key grep-mode-map (kbd "r")
      'e/grep-show-only-relative-path)
    (define-key compilation-button-map (kbd "RET")
      'grep-match-open-same-window)
    (define-key compilation-button-map (kbd "<mouse-2>")
      'grep-mouse-goto-match)))


;;** Locate mode

;;*** Function for mouse open file



(defun locate-open-file ()
  (interactive)
  (let (( path (locate-get-filename)))
    (open-file-choice path)))
(defun locate-mouse-open-file ( event)
  (interactive "e")
  (point-set-to-mouse event)
  (locate-open-file))


;;*** Hook



(add-hook 'locate-mode-hook
  (lambda ()
    (message "locate-mode-hook executed")
    (define-key locate-mode-map "\C-co" 'locate-open-file)
    (define-key locate-mode-map (kbd "<C-M-mouse-3>")
               'locate-mouse-open-file)))


;;** Goto URL address

(advice-add #'goto-address-at-point :after
            #'goto-address-at-point-hook)


(defun goto-address-at-point-hook ( &optional event)
  "Provide a hook for postprocessing of
`TeX-source-correlate-sync-source' function. This will treat the
sync direction evince -> emacs."
  (crowded-close-others)
  (message "Run goto-address-at-point."))


;;*** Hook


(add-hook 'conf-unix-mode-hook
  (lambda ()
    (setq-local outline-regexp "#[#*]\\(\\*\\**\\)\s[^\n]")
    (outline-minor-mode 1)
    (outline-hide-body)
    (define-key conf-unix-mode-map"\C-cc" 'compile)
    ;; (font-lock-add-keywords nil
    ;;                         '((hyperlink-fontify-button
    ;;                            0 nil append t))
    ;;                         'append)
))
;; (setq conf-unix-mode-hook nil)


;;** Compilation and Run

;;*** Hook

;; - Fix key binding for previous buffer "M-p"
(add-hook 'compilation-mode-hook
  (lambda ()
    (define-key compilation-mode-map (kbd "M-p")
      'previous-buffer)))


(add-hook 'compilation-filter-hook
  (lambda ()
    (comint-truncate-buffer)))


;;** Writeroom

(require 'writeroom-mode)


;;*** Define variables

(defvar writeroom-save-cursor-type nil)


;;*** Set variables

(setq writeroom-restore-window-config t)


;;*** Define function for narrowing

(defun writeroom-narrow-to-region ( beg end)
  (interactive "r")
  (setq-local writeroom-width 40)
  (setq-local writeroom-extra-line-spacing 0)
  (writeroom-mode 1)
  (font-lock-mode 0)
  (unless (region-active-p)
    (let (( pos (point)))
      (mark-paragraph-dwim)
      (if (< (region-beginning) pos)
          (setq beg (region-beginning)
                end (region-end))
        (setq beg (line-beginning-position)
              end (line-end-position)))))
  (narrow-to-region beg end)
  (deactivate-mark)
  (text-scale-increase 2)
  (setq-local writeroom-save-cursor-type cursor-type)
  (setq-local cursor-type 'hbar)
  (setq-local default-text-properties '(line-height 50))
  (show-paren-mode 0)
  (auto-fill-mode 1)
  (add-hook 'after-change-functions 'append-final-newline nil t))


(defun writeroom-widen ()
  (interactive)
  (remove-hook 'after-change-functions 'append-final-newline)
  (text-scale-increase 0)
  (writeroom-mode 0)
  (font-lock-mode 1)
  (widen)
  (show-paren-mode 1)
  (auto-fill-mode 0)
  (setq-local cursor-type writeroom-save-cursor-type)
  (setq-local default-text-properties '(line-height nil)))


(defun writeroom-narrow-toggle ( beg end)
  (interactive "r")
  (if writeroom-mode
      (writeroom-widen)
    (writeroom-narrow-to-region beg end)))


;;** Ledger


(setq ledger-accounts-file "/home/dan/accounting/ledger/accounts.ledger")


(defun ledger-do-check ( buffer)
  "Run a check command ."
  (goto-char (point-min))
  (let ((data-pos (point))
        (have-warnings nil))
    (shell-command
     ;;  ledger balance command will just return empty if you give it
     ;;  an account name that doesn't exist.  I will assume that no
     ;;  one will ever have an account named "e342asd2131".  If
     ;;  someones does, this will probably still work for them.
     ;;  I should only highlight error and warning lines.
     (concat "ledger -f " buffer " bal e342asd2131 --strict --explicit")
     t nil)
    (goto-char data-pos)

    ;; format check report to make it navigate the file

    (while (re-search-forward "^.*: \"\\(.*\\)\", line \\([0-9]+\\)" nil t)
      (let ((file (match-string 1))
            (line (string-to-number (match-string 2))))
        (when file
          (set-text-properties (line-beginning-position) (line-end-position)
                               (list 'ledger-source (cons file (save-window-excursion
                                                                 (save-excursion
                                                                   (find-file file)
                                                                   (widen)
                                                                   (ledger-navigate-to-line line)
                                                                   (point-marker))))))
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'font-lock-face 'ledger-font-report-clickable-face))
          (setq have-warnings 'true)
          (end-of-line))))
    (if (not have-warnings)
        (insert "No errors or warnings reported."))))


(defun ledger-check-buffer ()
  "Run ledge with --explicit and --strict report errors and assist with fixing them.

The output buffer will be in `ledger-check-mode', which defines
commands for navigating the buffer to the errors found, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
                (y-or-n-p "Buffer modified, save it? "))
       (save-buffer))))
  (let ((buf (find-file-noselect (ledger-master-file)))
        (cbuf (get-buffer ledger-check-buffer-name))
        (wcfg (current-window-configuration)))
    (if cbuf
        (kill-buffer cbuf))
    (with-current-buffer
        (pop-to-buffer (get-buffer-create ledger-check-buffer-name))
      (ledger-check-mode)
      (set (make-local-variable 'ledger-original-window-cfg) wcfg)
      (ledger-do-check (buffer-name buf))
      (shrink-window-if-larger-than-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (message "q to quit; r to redo; k to kill"))))


;;** Flycheck


;; (setq flycheck-check-syntax-automatically '( save
;;                                              idle-change
;;                                              mode-enabled)
;;       flycheck-idle-change-delay 2)

(setq flycheck-check-syntax-automatically nil
      flycheck-emacs-lisp-load-path 'inherit)


;;** RJSX mode

(setq js2-strict-missing-semi-warning nil)


;;** Highlight Indent Guide

(setq highlight-indent-guides-method 'character
      highlight-indent-guides-character ?│)


;;** ido mode

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
              " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)


;;** ivy mode


(require 'ivy)


(define-key ivy-minibuffer-map (kbd "<C-return>") 'e/org-roam-ivy-preview-browser)
(define-key ivy-minibuffer-map (kbd "<C-down>") 'e/org-roam-ivy-preview-next-browser)
(define-key ivy-minibuffer-map (kbd "<C-up>") 'e/org-roam-ivy-preview-previous-browser)
;; C-g: Exit Ivy

(ivy-set-actions
 t
 '(("o" e/org-roam-ivy-preview "preview roam file")))
(define-key ivy-minibuffer-map (kbd "<C-M-return>") 'e/org-roam-ivy-done)
(define-key ivy-minibuffer-map (kbd "<return>") 'e/org-roam-ivy-done)
;; C-M-m: Show current in emacs
;; C-M-n, C-M-p: Show next,previous in emacs
;; C-M-return: Exit Ivy and show current in emacs


(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order))
      ivy-display-style nil
      ivy-case-fold-search-default t)
;; default:
;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)))


(set-face-attribute 'ivy-current-match nil
                    :foreground nil
                    :background nil
                    :inherit 'ivy-highlight-face)


;;** Orcom Mode

(push "~/.emacs.d/lisp/orcom" load-path)
(require 'orcom)


;;* Programming languages derived from prog-mode


;;** Prog mode (all programming modes)


;;*** Define variable

(defvar cmt-block-paragraph-start nil
  "This variable adds extra regular expressions to the
orginal (global) `paragraph-start'")


(defvar cmt-block-paragraph-separate nil
  "This variable adds extra regular expressions for separating
paragraphs inside a comment-block. Separation of the comment
block itself is already taken care of.")


;;*** Variable definitions

(defvar code-block-lang nil
  "Language for highlighting fenced code blocks in markdown.")


(defvar comment-start-extra ">"
  "Extra comment symbol for indicating of special documentation
lines.")


(defvar major-mode-fill-paragraph-functions nil
  "List connecting major-mode with its original
`fill-paragraph-function' value.")


;;*** Comment new line and fill function


(defun line-prefix ( &optional pos prefix-extra)
  (save-match-data
    (save-excursion
      (when pos (goto-char pos))
      (beginning-of-line)
      (unless prefix-extra (setq prefix-extra "[^\s\n]+"))
      (message "line-prefix")
      (when (looking-at (concat "^" (string-trim comment-start)
                                prefix-extra "\s"))
        (match-string-no-properties 0)))))


(defun prefix-comment-new-line ()
  "Create new comment line respecting extra comment prefix.

When no comment prefix is found at the beginning of the line, use
`comment-indent-new-line' instead."
  (interactive)
  (let (( prefix (line-prefix)))
    (if prefix
        (if (>= (current-column) (length prefix))
            (insert (concat "\n" prefix))
          (beginning-of-line)
          (insert (concat prefix "\n"))
          (end-of-line 0))
      (comment-indent-new-line))))


(defmacro with-paragraph-dwim ( &rest body)
  "Execute body at mouse click position without moving point."
  `(let (( cmt (string-trim comment-start))
        ( prefix (when (line-prefix)
                    (string-trim (line-prefix)))))
    (if prefix
        (let* (( cmt-extra (substring prefix 1))
               ( fill-paragraph-function nil)
               ;; Prevent the first line of the comment block being
               ;; consumed by fill-paragraph, when the first line
               ;; matches a regular expression in paragraph-separate.
               ( fill-paragraph-handle-comment nil)
               ( paragraph-ignore-fill-prefix t)
               ( fill-prefix (concat prefix " "))
               ( start-extra
                 (when cmt-block-paragraph-start
                   (concat "\\|" prefix
                           "\\(" cmt-block-paragraph-start "\\)")))
               ( paragraph-start
                 (concat "\f\\|[ \t]*$" start-extra))
               ( separate-extra
                 (when cmt-block-paragraph-separate
                   (concat "\\|" prefix
                           "\\(" cmt-block-paragraph-separate "\\)")))
               ( paragraph-separate
                 (concat prefix "\s*$"
                         "\\|\s*[^" cmt "]"
                         "\\|" prefix "\s*$"
                         "\\|" cmt "[^" cmt-extra "]"
                         separate-extra)))
          ,@body)
      ,@body)))


(defun forward-paragraph-dwim (&optional arg)
  (interactive "^p")
  (with-paragraph-dwim (forward-paragraph arg)))


(defun backward-paragraph-dwim (&optional arg)
  (interactive "^p")
  (with-paragraph-dwim (backward-paragraph arg)))


(defun mark-paragraph-dwim ( &optional arg allow-extend)
  "Mark paragraph at point respecting custom prefix.

\"Do what i mean\" version of `mark-paragraph' fills paragraphs
respecting arbitrary prefix strings, e.g. doxygen prefix *!>*."
  (interactive "p\np")
  (with-paragraph-dwim (mark-paragraph arg allow-extend)))


(defun fill-paragraph-dwim ( &optional justify region)
  "Fill paragraph at point respecting custom prefix.

\"Do what i mean\" version of `fill-paragraph' fills paragraphs
respecting arbitrary prefix strings, e.g. doxygen prefix *!>*."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (with-paragraph-dwim (fill-paragraph justify region)))


;;*** Function for converting to markdown

(defun convert-source-to-markdown ( in-buffer out-md lang)
  "Convert any source code into markdown formatting."
  (with-temp-file out-md
    (insert-buffer-substring in-buffer)
    (goto-char (point-min))
    ;; Delete shebang
    (when (looking-at "#!.*\n")
      (replace-match ""))
    ;; Fencing of code blocks
    (while (re-search-forward "^#.*\\(\n\s*\\)+[^#\n]" nil t)
      (beginning-of-line)
      (insert (format "\n```%s\n" lang)))
    (goto-char (point-min))
    (while (re-search-forward "^\s*[^#\n].*\\(\n\s*\\)+#" nil t)
      (goto-char (match-beginning 0))
      (beginning-of-line 2)
      (insert "```\n\n"))
    ;; Fix code at EOF
    (goto-char (point-max))
    (re-search-forward "^\s*\\([^\s\n]\\)" nil t -1)
    (unless (string= (match-string 1) "#")
      (end-of-line)
      (insert "\n```"))
    ;; Prepare outline-mode headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(#.\\)\\*+\s[^\s\n]" nil t)
      (replace-match "" t nil nil 1))
    (goto-char (point-min))
    ;; Erase all comments at beginning of line
    (while (re-search-forward "^#+\s?" nil t)
      (replace-match ""))
    ;; Finish outline-mode headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\s[^\s\n]" nil t)
      (replace-match
       (make-string (length (match-string 1)) ?#)
       t nil nil 1))))


(defun markdown-to-jekyll-kramdown ( in-file)
  (let (( jekyll-root "/home/dan/jekyll")
        ( image-dir "/assets/images/"))
    (with-temp-file in-file
      (insert-file-contents in-file)
      (goto-char (point-min))
      (while (re-search-forward "!\\[.*?\\](\\(.*?/\\)\\([^/\n]+\\.[a-z]+\\))\\({\\)[^:].*?}" nil t)
        (copy-file (concat (match-string 1) (match-string 2))
                   (concat jekyll-root image-dir (match-string 2)) t)
        (replace-match image-dir t nil nil 1)
        (replace-match "{:" t nil nil 3)))))


(defun file-name-add-jekyll-time ( dir-path name &optional extension)
  (concat (file-name-as-directory dir-path)
          (format-time-string "%Y-%m-%d-" (current-time))
          name extension))


(defun convert-source-to-final ()
  "Convert programming language source file into documentation
and blog."
  (interactive)
  (save-buffer)
  (let* (( in-buffer (current-buffer))
         ( in-lang code-block-lang)
         ( in-base (file-name-sans-extension (buffer-name)))
         ( out-dir (concat (file-name-directory (buffer-file-name))
                           (file-name-as-directory "doc")))
         ( jekyll-dir "/home/dan/jekyll/_posts/")
         ( out-dir-base (concat out-dir in-base))
         ( out-new (file-name-add-jekyll-time jekyll-dir in-base ".md"))
         ( out-md (concat out-dir-base ".md"))
         ( out-html (concat out-dir-base ".html"))
         ( out-pdf (concat out-dir-base ".pdf"))
           out-old)
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    ;; produce markdown as base format
    (convert-source-to-markdown in-buffer out-md in-lang)
    ;; from markdown to jekyll
    (when (setq out-old (car (directory-files-recursively jekyll-dir
                                                          in-base)))
      (if (y-or-n-p "Update post timestamp? ")
          (delete-file out-old)
        (setq out-new out-old)))
    (copy-file out-md out-new t)
    (markdown-to-jekyll-kramdown out-new)
    ;; from markdown produce other formats
    (shell-command (concat "pandoc " out-md
                           " -s --highlight-style zenburn "
                           "-o " out-html))
    (shell-command (concat "pandoc " out-md
                           " -s --highlight-style zenburn "
                           "-o " out-pdf))))


;;*** Hook


(add-hook 'prog-mode-hook
  (lambda ()
    (auto-complete-mode)
    ;; (highlight-indent-guides-mode)
    (define-key prog-mode-map "\M-j" 'prefix-comment-new-line)))


;;** Emacs lisp mode


;;*** Function for documenting

(defun elisp-insert-doc-template ()
  (interactive)
  (end-of-line)
  (when (re-search-forward "(defun" nil t -1)
    (beginning-of-line 2)
    (insert
"  \"

Args:
  ARG1:
Returns: \"
")))


;;*** Hook

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (cfold-minor-mode 1)
    (flycheck-mode 1)
    ;; (outline-minor-mode)
    ;; (setq-local outline-regexp ";[;*]\\(\\*\\**\\)\s[^\n].*")
    ;; (setq outline-blank-line t)
    ;; (outline-hide-body)
    ))


;;** C mode


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c++-mode-hook
 (lambda ()
   (define-key c++-mode-map"\C-cc" 'compile)))
 

;;** Shell mode

(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))


;;** Python mode


;;*** Function separation lines and outline


(defun python-separation-line ()
  (interactive)
  (beginning-of-line)
  (let (( indent (python-indent-calculate-indentation))
        ( start nil))
       (if (looking-at "\\(\s*\\|\s*#--+\\)$")
           (progn
             (setq start (string-match "#" (match-string 1)))
             (when start
                   (setq indent start))
             (delete-region (point) (line-end-position)))
           (progn
             (insert "\n")
             (beginning-of-line 0)))
       (insert (make-string indent ? ))
       (insert (string-trim comment-start))
       (insert (make-string (- 69 indent) ?-))))


;;*** Variables


(setq python-outline-regexp (concat
                             "#\\*\\(\\*\\**\\)\s[^\n].*\\|"
                             "\s*\\(class\s\\|def\s\\|"
                             "#\\+BEGIN_DOXY def\\|"
                             "#\\+BEGIN_DOXY class\\)")
      python-outline-heading-alist '(("class" . 9)
                                     ("#+BEGIN_DOXY class" . 9)
                                     ("def" . 10)
                                     ("#+BEGIN_DOXY def" . 10)
                                     ("!+BEGIN_DOXY file" . 1))
      python-outline-end-regexp "\n"
      python-indent-guess-indent-offset nil
      python-indent-guess-indent-offset-verbose nil
      python-fill-docstring-style 'pep-257-nn)


;;*** Blender Python Addon Update


(defun bpy-save-with-init-buffer ()
  (interactive)
  (save-buffer)
  (with-current-buffer 
      (find-file-noselect 
       (concat (file-name-directory (buffer-file-name)) "__init__.py"))
    (set-buffer-modified-p t)
    (save-buffer)))


;;*** Hook

;; When using the outline minor mode for code folding together with
;; doxygen documentation, it is recommended to use "#**", "#***",
;; etc. as outline headline keys. When "###" is given as headline key,
;; doxygen will interprete this line as a documentation line.
;;
;; For outline headlines from python commands, use levels form 9
;; upwards.

;; (define-minor-mode bpy-minor-mode
;;   "Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode)."
;;   :init-value nil :lighter " bpy" :keymap nil
;;   (unless (eq major-mode 'python-mode)
;;     (user-error "Not in a PYTHON buffer."))
;;   (cond (bpy-minor-mode
;;          (e/org-roam-links-highlight)
;;          (add-hook 'org-metadown-hook 'e/org-roam-links-highlight-persistent)
;;          (add-hook 'org-metaup-hook 'e/org-roam-links-highlight-persistent))
;;         (t
;;          (remove-overlays (point-min) (point-max) 'type 'links-highlight)
;;          (remove-hook 'post-command-hook 'e/org-roam-links-highlight-refresh)
;;          (remove-hook 'org-metadown-hook 'e/org-roam-links-highlight-persistent)
;;          (remove-hook 'org-metaup-hook 'e/org-roam-links-highlight-persistent)
;;          )))


(add-hook 'python-mode-hook
  (lambda ()
    ;; (setq-local outline-regexp python-outline-regexp)
    ;; (setq-local outline-heading-alist python-outline-heading-alist)
    ;; (setq-local outline-heading-end-regexp python-outline-end-regexp)
    ;; (outline-minor-mode 1)
    (cfold-minor-mode 1)
    (flycheck-mode 1)
    ;; (font-lock-add-keywords nil
    ;;   '((hyperlink-fontify-button 0 nil append t)))
    (define-key python-mode-map "\C-cc" 'obuffer-compile)
    ))
 

;;** Fortran mode


;;*** Variables


(setq fortran-outline-regexp (concat
                              "!\\*\\(\\*\\**\\)\s[^\n].*\\|"
                              "\s*\\(subroutine\s\\|module\s\\|"
                              "contains\\|!\\+BEGIN_DOXY sub"
                              "\\|!\\+BEGIN_DOXY mod"
                              "\\|!\\+BEGIN_DOXY file\\)")
      fortran-outline-heading-alist '(("module" . 9) ("contains" . 10)
                                      ("subroutine" . 11)
                                      ("!+BEGIN_DOXY sub" . 11)
                                      ("!+BEGIN_DOXY mod" . 9)
                                      ("!+BEGIN_DOXY file" . 1))
      fortran-outline-end-regexp "\n")


;;*** Indentation functions


;: --- #
;: layout: post
;: author: Daniel Johannsen
;: title: Multiline list indentation
;: date: 2019-02-28
;: ---
;: 
;: When editing *.f90 files under f90-mode in emacs in most of the
;: cases automatic indentation does a pretty good job in producing
;: readable code. The only case, where indenentation works not as
;: expected, is inside comma separated lists, e.g. the argument list
;: of a call to a subroutine.
;: 
;: We therefore have to check, weather our point is indeed inside an
;: open list.
;: ```elisp

(defun find-open-parenthesis ()
  "Find most recent open parenthesis positions in current line.

When an open parenthesis is found, leave point after the position
of this parenthesis."
  (let (( open 0) positions)
    (while (re-search-forward "\\((\\)\\|)" (line-end-position) t)
      (if (match-string 1)
          (setq open (1+ open)
                positions (cons (point) positions))
        (setq open (1- open))
        (pop positions)))
    (car positions)))


(defun find-open-parenthesis-column ( &optional line-num)
  (save-excursion
    (beginning-of-line line-num)
    (let (( beg (point)))
      (when (setq end (find-open-parenthesis))
        (- end beg)))))


(defun f90-indent-line-dwim ()
  "Indent line according to context of previous line.

Fix indentation for a comma separated list right after the list
beginning line. For the list to continue on further lines, the
function `f90-calculate-indent' manages indentation correctly."
  (indent-line-to (or (find-open-parenthesis-column 0)
                      (f90-calculate-indent))))

;: ```


;;*** Hooks


(add-hook 'f90-mode-hook
  (lambda ()
    ;; (setq-local outline-regexp fortran-outline-regexp)
    ;; (setq-local outline-heading-alist fortran-outline-heading-alist)
    ;; (setq-local outline-heading-end-regexp fortran-outline-end-regexp)
    ;; (outline-minor-mode 1)
    ;; (setq-local f90-program-indent 4)
    (setq-local indent-line-function 'f90-indent-line-dwim)
    (flycheck-mode 1)
    (define-key f90-mode-map "\C-cc" 'obuffer-compile)))
;; (setq f90-mode-hook nil)

(add-hook 'fortran-mode-hook
  (lambda ()
    (setq-local outline-regexp fortran-outline-regexp)
    (setq-local outline-heading-end-regexp fortran-outline-end-regexp)
    (outline-minor-mode 1)
    ;; (font-lock-add-keywords nil
    ;;   '((hyperlink-fontify-button 0 nil append t))
    ;;   'append)
    (define-key fortran-mode-map "\C-cc" 'compile)))
 

;;** Web mode

;;*** Set automatic mode

(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . web-mode))
(add-to-list 'magic-mode-alist '("<?php" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;;*** Web mode Hooks


(add-hook
 'web-mode-hook
 (lambda ()
   (setq web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2)
   ))


;;* Markup


;;** Text mode (all markup modes)

;; (add-hook 'text-mode-hook
;;   (lambda ()
;;     (goto-address-mode)
;;     (font-lock-add-keywords nil
;;       '((hyperlink-fontify-button 0 nil append t)) 'append)
;; ))
;; (setq text-mode-hook nil)


;;** TeX/LaTeX mode

;;*** Require necessary packages for org-reftex

;; Make sure to packages for citations are loaded.

(require 'org)
;; (require 'org-bibtex)
;; (require 'ox-bibtex)
 

;;*** Font definition


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


(setq LaTeX-map-block-colors '(("yellow" latex-block-yellow)
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
    (insert "\\label\{"))
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
          (insert "\}")
          (newline)))


;;*** Functions for sparse tree
 

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
  (insert "(\\ref\{\})")
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
                  "\{" (region-tex-insert) "\}"))
  (goto-char (1- (point))))


(defun insert-tex-texteq()
  (interactive)
  (insert (concat "\$ " (region-tex-insert) "\$"))
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
    (insert (concat "\{" (setq extracted (region-tex-insert)) "\}"))
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
                  "\{" (setq extracted (region-tex-insert)) "\}")
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
  (insert "\\frac\{\\partial  \}\{\\partial \}")
  (goto-char (- (point) 13)))


(defun insert-tex-derivative ()
  (interactive)
  (insert "\\frac\{\\mathrm{d}  \}\{\\mathrm{d}  \}")
  (goto-char (- (point) 16)))


(defun insert-tex-fraction ()
  (interactive)
  (insert "\\frac\{  \}\{  \}")
  (goto-char (- (point) 6)))


(defun insert-tex-biblink ()
  (interactive)
  (let* (( masterFile (buffer-file-name))
         ( biblinkFile (concat biblink-path-commands-dir
                               (file-name-base masterFile)
                               "-biblink.tex"))
         ( key (car (reftex-citation t)))
         ( path (latex-locate-citation key))
         ( fields (bibtex-key-get-fields key "author" "year"))
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
                       (add-to-list 'remainFiles baseExt)))
                ((string= "bibliography" (match-string 1))
                 (setq base (get-filename-at-pos start dir))
                 (setq baseExt (basename-add-extension
                                 base base ".bib" ".bib"))
                 (when (file-exists-p baseExt)
                       (add-to-list 'remainFiles baseExt)))
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
                (add-to-list 'files baseExt)))))
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
              fields (bibtex-key-get-fields key "author" "year")
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


;;*** Function for viewing bibtex keys in external app

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


;;*** Function for highlighting


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
  (let (( count 0)
        ( pos (point)))
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


;;*** Function layer folding


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
        (when (setq overlay (begin-end-block-folded-p))
          (LaTeX-show-layer))))
  (font-lock-flush))


;;*** Function for buffer change before and after compilation


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


;;*** Function for source file position to pdf position


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
`TeX-source-correlate-sync-source' function. This will treat the
sync direction evince -> emacs."
  (delete-overlays-at-point)
  (message "Run TeX-source-correlate-after-hook finished."))


;; (dbus-call-method
;; 		 :session "org.gnome.evince.Daemon"
;; 		 "/org/gnome/evince/Daemon"
;; 		 "org.gnome.evince.Daemon"
;; 		 "FindDocument"
;; 		 "file:///home/dan/tex/out/Dissertation.pdf"
;; 		 t) -> ":1.306"

;; (dbus-get-name-owner :session ":1.306")
;; (dbus-get-name-owner :session "org.gnome.evince.Daemon") -> ":1.272"

;; (dbus-list-queued-owners :session "org.gnome.evince.Daemon") -> (":1.272")

;; (dbus-list-known-names :session)

;; (dbus-introspect :session "org.gnome.evince.Daemon"
;; 		 "/org/gnome/evince/Daemon")

;; (dbus-introspect-get-all-nodes :session ":1.308" "/")
;;    -> "/org/gnome/evince/Window/0"

;; (dbus-introspect :session ":1.308"
;; 		 "/org/gnome/evince/Window/0")

;; (dbus-register-signal
;;        :session nil "/org/gnome/evince/Window/0"
;;        "org.gnome.evince.Window"
;;        "SyncSource"
;;        '(lambda ( file linecol &rest ignored) (message "%s %s %s" file linecol ignored)))

;; -> on ctrl-left click in evince
;;       -> file:///home/dan/tex/include/Dissertation/chapter01.tex (457 -1)


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


(defun pdf-find-LaTeX-source-show ( file linecol &rest ignored)
  (find-file (url-filename (url-generic-parse-url file)))
  (goto-line (car linecol))
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


;;*** AUCTeX-dnd


(defcustom AUCTeX-dnd-format "\\includegraphics[width=\\textwidth]{%s}"
  "What to insert, when a file is dropped on Emacs window. %s is
replaced by the actual file name. If the filename is located
under the directory of .tex document, only the part of the name
relative to that directory in used."
  :type 'string
  :group 'AUCTeX)


;; Modified version
(defun AUCTeX-dnd-includegraphics (uri action)
  "Insert the text defined by `AUCTeX-dnd-format' when a file is
dropped on Emacs window."
  (let ((file (dnd-get-local-file-name uri t)))
    (when (and file (file-regular-p file))
      (if (string-match "/my/texinputs/path/to/images" file)
      (insert (format AUCTeX-dnd-format (file-name-nondirectory file)))
    (insert (format AUCTeX-dnd-format file))
    )
      )
    )
  )


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
    (LaTeX-add-environments '("layer" LaTeX-env-layer)
                            '("layer-split" LaTeX-env-layer-split))
    (font-lock-add-keywords nil
      '(;; (hyperlink-fontify-button 0 nil append t)
        ("\\(\\\\,\\)" 1 font-lock-keyword-face t)
        ( LaTeX-fontify-environments ( 0 nil append t))
        ;; ( LaTeX-fontify-keywords-inside ( 0 nil append t))
        ( LaTeX-fontify-level-indentation ( 0 nil append t))))))


(add-hook 'TeX-after-compilation-finished-functions
          'LaTeX-change-after-compilation)


;;** BibTeX mode


;;*** Set variables


(setq bibtex-comma-after-last-field t)


(defvar bibtex-extra-fields
  '("abstract" "keywords" "chapter" "url" "isbn" "isbn_ebook"
    "isbn_soft" "isbn_hard" "issn" "issn_print" "issn_online"
    "issn_cdrom" "eprint" "doi" "year" "publisher" "fullname"
    "oldbibtexkey")
  "Extra fields to add to `bibtex-BibTeX-entry-alist' for the
function `bibtex-insert-field'.")


;;*** Function for formatting bibtex keys


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
        

(defun bibtex-key-get-fields ( key &rest fields)
  (save-match-data
    (with-temp-buffer
      (insert-file-contents (car reftex-default-bibliography))
      (when (bibtex-search-entry key)
        (let (( values nil))
          (dolist ( field fields)
            (setq values (cons (ebibtex-entry-get-field field)
                               values)))
          (nreverse values))))))


(defun bibtex-field-end-comma-entry ()
  (bibtex-end-of-entry)
  (re-search-forward "\\([^ \n{}]\\)" nil t -1)
  (unless (string= (match-string 1) ",")
    (end-of-line)
    (insert ",")))


(defun bibtex-field-end-comma ()
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


(defun bibtex-author-fullname ()
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


(defun bibtex-provide-fullname ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@.*{\\(.*\\),.*$" (point-max) t)
           (bibtex-author-fullname))))


(defun bibtex-duplicate-entries ()
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


(defun bibtex-show-duplicate-entry ()
  (interactive)
  (let (( dup (car (bibtex-duplicate-entries))))
    (if dup
      (let (( title "")
            ( i 0)
            ( pos1 (point-min))
            ( pos2 (point-min))
            beg end)
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


(defun bibtex-format-keys ()
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
    (bibtex-provide-fullname)
    (bibtex-reformat)
    (bibtex-field-end-comma)
    (message "Changed BibTeX keys: %s/%s" countMod countAll)))


(defun bibtex-fix-key ()
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


(defun bibtex-reformat-key ()
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


(defun bibtex-format-key ()
  "Assume point somewhere inside the entry (at or after the
at-symbol)."
  (interactive)
  (unless (find-nonascii-char)
    (bibtex-fix-key)
    (e/bibtex-replace-comment-symbol)
    (bibtex-clean-entry)
    (bibtex-author-fullname)
    (bibtex-fill-entry)
    (bibtex-field-end-comma-entry)))


;;*** Function file handling - extended bibtex


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


;; (defun ebibtex-next-missing-file ()
;;   (interactive)
;;   (beginning-of-line)
;;   (let ( type)
;;     (when (looking-at ebibtex-entry-beginning-re)
;;       (setq type (match-string 1)))
;;     (message "type %s" type)
;;     (while (and (re-search-forward ebibtex-entry-beginning-re (point-max) t)
;;                 (if (not type)
;;                     (ebibtex-locate (match-string 2))
;;                   (if (string= type (match-string 1))
;;                       (ebibtex-locate (match-string 2))
;;                     'cycle))))))


;;*** Function for inserting (making) new fields


(defun bibtex-gather-field-names ()
  (let ( all-fields)
    (dolist ( type bibtex-BibTeX-entry-alist)
      (dolist ( fields type)
        (when (listp fields)
          (dolist ( field fields)
            (setq all-fields (cons (car field) all-fields))))))
    (reverse (delete-dups all-fields))))


(defun bibtex-insert-field ()
  "Insert new field in current BibTeX entry.

This is a variant of function `bibtex-make-field' providing all
possible fields regardless of the entry type. Custom field names
can be added with the variable `bibtex-extra-fields'."
  (interactive)
  (let (( fieldTag nil)
        ( valueColumn 17)
        ( all-fields (delete-dups
                      (append (bibtex-gather-field-names)
                              bibtex-extra-fields))))
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


;;*** Retrieving BibTex Entries from Web


(defun ebibtex-entry-from-isbn ()
  (interactive)
  (let* (( isbn (read-string "ISBN: "))
         ( url (format "https://openlibrary.org/isbn/%s.json" isbn))
	     ( json (with-temp-buffer
                 (url-insert-file-contents url)
                 (json-parse-buffer :object-type 'alist
                                    :array-type 'list)))
         ( test (progn (message "%s" (parse-time-string (cdr (assoc 'publish_date json)))) "test"))
         ( title (cdr (assoc 'title json)))
         ( date (parse-time-string (cdr (assoc 'publish_date json))))
         ( day (when (nth 3 date) (number-to-string (nth 3 date))))
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
    

;;*** Hook

(add-hook 'bibtex-mode-hook
  (lambda ()
    (define-key bibtex-mode-map "\C-ce" 'bibtex-format-key)
    (define-key bibtex-mode-map "\C-ck" 'bibtex-format-keys)
    (define-key bibtex-mode-map "\C-ci" 'bibtex-insert-field)))


;;** Markdown mode

(defvar jekyll-root-url "http://localhost:4000")


(defun jekyll-url ()
  (string-match "^\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)-\\(.*?\\)\\.md$" (buffer-name))
  (concat (file-name-as-directory jekyll-root-url)
          (file-name-as-directory (match-string 1 (buffer-name)))
          (file-name-as-directory (match-string 2 (buffer-name)))
          (file-name-as-directory (match-string 3 (buffer-name)))
          (match-string 4 (buffer-name)) ".html"))


(defun jekyll-view ()
  (interactive)
  (crowded-close-others)
  (async-shell-command (concat "firefox "
                               (jekyll-url)
                               " -P emacs -no-remote")))


(add-hook 'markdown-mode-hook
  (lambda ()
    (setq-local tab-width 2)
    (when buffer-file-name
      (let* (( infile (file-name-nondirectory buffer-file-name)))
        (set (make-local-variable 'compile-command)
             (concat "mdparse " infile))))
    (define-key markdown-mode-map "\C-cc" 'compile)
    (define-key markdown-mode-map "\C-cv" 'jekyll-view)))


;;** Html mode

;;*** Set variables

(setq sgml-validate-command "tidy")


;;*** Inserting commands

(defun html-insert-tag ()
  (interactive)
  (let (( tag (completing-read "Tag type: "
                    '("b" "a")
                    nil nil "")))
    (insert "<" tag ">" (paste-word-or-region) "</" tag ">"))
  (re-search-forward "</" (line-beginning-position) t -1))


;;*** Function view in browser

(defun html-view-on-this-workspace ()
  (interactive)
  (save-buffer)
  (crowded-close-others)
  (async-shell-command (concat "firefox " (buffer-file-name)
                               " -P emacs -no-remote"))
  (shell-command "/home/dan/bin/i3-focus-window 5 emacs"))


;;*** Hook


(add-hook 'html-mode-hook
  (lambda ()
    (define-key html-mode-map "\C-cc" 'html-view-on-this-workspace)
    (font-lock-add-keywords
     nil '(("$[A-Za-z_][A-Za-z0-9_]*" . font-lock-keyword-face)))))

(add-hook 'mhtml-mode-hook
  (lambda ()
    (font-lock-add-keywords
     nil '(("$[A-Za-z_][A-Za-z0-9_]*" . font-lock-keyword-face)))))


;;** nXML mode

(defun insert-ar-region-command (start end)
  "Article.
The <ar> tag groups together all the stuff related to one key-phrase.
Insert a markup <ar></ar> around a region."
  (interactive "r")
  (goto-char end) (insert "</ar>")
  (goto-char start) (insert "<ar>")
  (goto-char (- (point) 0)))
(defun insert-k-region-command (start end)
  "Key Phrase. Insert a markup <k></k> around a region."
  (interactive "r")
  (goto-char end) (insert "</k>")
  (goto-char start) (insert "<k>")
  (goto-char (- (point) 0)))
(defun insert-def-region-command (start end)
  "Definition.
This tag cotains either whole body of a word article inside <ar>,
or a definition itself or a group of definitions,
that fall into a certain category.
Insert a markup <def></def> around a region."
  (interactive "r")
  (goto-char end) (insert "</def>")
  (goto-char start) (insert "<def>")
  (goto-char (- (point) 0)))
(defun insert-sr-region-command (start end)
  "Semantik Relation. Insert a markup <sr></sr> around a region."
  (interactive "r")
  (goto-char end) (insert "</sr>")
  (goto-char start) (insert "<sr>")
  (goto-char (- (point) 0)))
(defun insert-ex-region-command (start end)
  "Example, Quote. Insert a markup <ex></ex> around a region."
  (interactive "r")
  (goto-char end) (insert "</ex>")
  (goto-char start) (insert "<ex>")
  (goto-char (- (point) 0)))
(defun insert-co-region-command (start end)
  "Comment to Key Phrase. Insert a markup <ex></ex> around a region."
  (interactive "r")
  (goto-char end) (insert "</co>")
  (goto-char start) (insert "<co>")
  (goto-char (- (point) 0)))
(defun insert-kref-region-command (start end)
  "Link. Insert a markup <kref></kref> around a region."
  (interactive "r")
  (goto-char end) (insert "</kref>")
  (goto-char start) (insert "<kref>")
  (goto-char (- (point) 0)))
(defun insert-gr-region-command (start end)
  "Grammar. Insert a markup <gr></gr> around a region."
  (interactive "r")
  (goto-char end) (insert "</gr>")
  (goto-char start) (insert "<gr>")
  (goto-char (- (point) 0)))
(defun insert-opt-region-command (start end)
  "Inside k-tag. Insert a markup <opt></opt> around a region."
  (interactive "r")
  (goto-char end) (insert "</opt>")
  (goto-char start) (insert "<opt>")
  (goto-char (- (point) 0)))
(defun insert-dtrn-region-command (start end)
  "Direct Translation (of key word).
Insert a markup <dtrn></dtrn> around a region."
  (interactive "r")
  (goto-char end) (insert "</dtrn>")
  (goto-char start) (insert "<dtrn>")
  (goto-char (- (point) 0)))

(add-hook 'nxml-mode-hook
    (lambda ()
         (define-key nxml-mode-map "\C-ca" 'insert-ar-region-command)
         (define-key nxml-mode-map "\C-ck" 'insert-k-region-command)
         (define-key nxml-mode-map "\C-cd" 'insert-def-region-command)
         (define-key nxml-mode-map "\C-cs" 'insert-sr-region-command)
         (define-key nxml-mode-map "\C-ce" 'insert-ex-region-command)
         (define-key nxml-mode-map "\C-cc" 'insert-co-region-command)
         (define-key nxml-mode-map "\C-cr" 'insert-kref-region-command)
         (define-key nxml-mode-map "\C-cg" 'insert-gr-region-command)
         (define-key nxml-mode-map "\C-co" 'insert-opt-region-command)
         (define-key nxml-mode-map "\C-ct" 'insert-dtrn-region-command)))
 

;;* FEM


;;** Feap mode


;;*** Set emacs environment variables

(setenv "FEAPPVHOME4_1" "/home/dan/feap/ver4.1")


;;*** Set variables

(defvar feap-mode-syntax-table
  (let (( synTable (make-syntax-table)))
    (modify-syntax-entry ?! "<" synTable)
    (modify-syntax-entry ?\n ">" synTable)
    synTable)
"Syntax table for `feap-mode'.")

(defvar feap-font-lock-keywords
'(("[^\s\n,]\\{15,255\\}" . whitespace-line)
  ("^[ ]*[fF][eE][aA][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[eE][nN][dD][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[bB][lL][oO][cC][^ ,\n]*" . font-lock-constant-face)
  ("^[ ]*[eE][bB][oO][uU][^ ,\n]*" . font-lock-constant-face)
  ("^[ ]*[cC][fF][oO][rR][^ ,\n]*" . font-lock-constant-face)
  ("^[ ]*[bB][aA][tT][cC][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[fF][oO][rR][cC][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[bB][oO][uU][nN][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[mM][aA][tT][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[sS][tT][oO][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[iI][nN][tT][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[pP][lL][oO][tT][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[fF][oO][rR][mM][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][aA][nN][gG][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[uU][tT][aA][nN][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[sS][oO][lL][vV][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[dD][iI][sS][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[eE][dD][iI][sS][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[cC][dD][iI][sS][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[sS][tT][rR][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[lL][oO][oO][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[nN][eE][xX][tT][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][rR][aA][nN][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[dD][tT][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][iI][mM][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[pP][rR][oO][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][oO][lL][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[iI][nN][cC][lL][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][oO][rR][sS][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[cC][hH][eE][cC][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[eE][lL][eE][mM][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[cC][oO][oO][rR][^ ,\n]*" . font-lock-function-name-face)))


;;*** Major mode definition

(define-derived-mode feap-mode fundamental-mode "Feap"
  "Major Mode for Feap Input File"
  :syntax-table feap-mode-syntax-table
  (setq-local font-lock-defaults
    '( feap-font-lock-keywords nil nil nil))
  (setq-local comment-start "!")
  (setq-local comment-end ""))

(add-to-list 'magic-mode-alist '("FEAP" . feap-mode))
(add-to-list 'magic-mode-alist '("!FEAP" . feap-mode))


;;** Elmer mode

;;*** Automatically choose mode

(add-to-list 'auto-mode-alist '("\\.sif\\'" . elmer-mode))
(add-to-list 'auto-mode-alist '("\\.grd\\'" . elmer-mode))
(add-to-list 'magic-mode-alist '("!Elmer" . elmer-mode))
(add-to-list 'magic-mode-alist '("ElmerGrid" . elmer-mode))


;;*** Set variables, syntax table

(defvar elmer-mode-syntax-table nil
  "Syntax table for `elmer-mode'.")


(setq elmer-mode-syntax-table
     (let ( (synTable (make-syntax-table)))
       (modify-syntax-entry ?! "<" synTable)
       (modify-syntax-entry ?\n ">" synTable)
       synTable))


(defvar elmer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'run-elmer-solver)
    map))


(setq ElmerKeywords '(
        ("^\\*.*" . font-lock-comment-face)
        ("^Simulation" . font-lock-function-name-face)
        ("^End" . font-lock-function-name-face)
        ("^Equation" . font-lock-function-name-face)
        ("^Header" . font-lock-function-name-face)
        ("^Body" . font-lock-function-name-face)
        ("^Solver" . font-lock-function-name-face)
        ("^Materials\sInterval" . font-lock-function-name-face)
        ("^Material\sStructure" . font-lock-function-name-face)
        ("^Material" . font-lock-function-name-face)
        ("^Extruded\sStructure" . font-lock-function-name-face)
        ("^Boundary\sCondition" . font-lock-function-name-face)
        ("^Boundary\sDefinitions" . font-lock-function-name-face)
        ("^Initial\sCondition" . font-lock-function-name-face)
        ("Real" . font-lock-constant-face)
        ("String" . font-lock-constant-face)
        ("Integer" . font-lock-constant-face)
        ("logical" . font-lock-constant-face))
      font-lock-keywords-case-fold-search t
      elmer-outline-regexp (concat
                          "![!*]\\(\\*\\**\\)\s[^\n]\\|"
                          "\s*\\(Header\\|Simulation\\|Body\\|Equation\\|"
                          "Solver\\|Initial\\|Boundary\\|Material\\)[^=]*$")
      elmer-outline-heading-alist '(("Header" . 9)
                                    ("Simulation" . 9)
                                    ("Body" . 9)
                                    ("Equation" . 9)
                                    ("Material" . 9)
                                    ("Solver" . 9)
                                    ("Initial" . 9)
                                    ("Boundary" . 9)))

;;*** Function for running elmer problems

(defun run-elmer-solver ()
  "Run elmer solver from Emacs."
  (interactive)
  (let* (( path (buffer-file-name))
         ( sif-home (directory-file-name (file-name-directory path)))
         ( sif-base (file-name-base path))
         ( sif-time (format-time-string "%Y%m%d_%H%M%S" (current-time))))
    (goto-char (point-min))
    (re-search-forward "Results\sDirectory\s+\"\\(.*\\)\"" nil t)
    (let (( out (match-string 1))
          ( run-file ))
      (setq out (replace-regexp-in-string "<SIF_HOME>" sif-home out t)
            out (replace-regexp-in-string "<SIF_BASE>" sif-base out t)
            out (replace-regexp-in-string "<SIF_TIME>" sif-time out t)
            out (file-name-as-directory out))
      (goto-char (point-min))
      (re-search-forward "Solver\sInput\sFile\s*=\s*\"\\(.*\\)\""
                         nil t)
      (let (( run-file (match-string 1)))
        (setq run-file (replace-regexp-in-string "<SIF_BASE>" sif-base run-file t)
              run-file (concat out run-file))
        (let (( command (concat "ElmerSolver " run-file)))
          (when (y-or-n-p (concat "Run: " command))
            (make-directory out t)
            (save-buffer)
            (dired-copy-file path run-file t)
            (with-current-buffer (find-file-noselect run-file)
              (outline-show-all)
              (widen)
              (goto-char (point-min))
              (while (re-search-forward "<SIF_HOME>\\|<SIF_BASE>\\|<SIF_TIME>"
                                        nil t)
                (cond ((string= (match-string 0) "<SIF_HOME>")
                       (replace-match sif-home t))
                      ((string= (match-string 0) "<SIF_BASE>")
                       (replace-match sif-base t))
                      ((string= (match-string 0) "<SIF_TIME>")
                       (replace-match sif-time t))))
              (save-buffer))
            (compile command)
            (with-current-buffer "*compilation*"
              (rename-buffer (format "*Run %s from %s*" sif-base sif-time)))))))))


;;*** Elmer major mode definition

(define-derived-mode elmer-mode fundamental-mode
  "Elmer" "Major Mode for Elmer Input File"
  (set-syntax-table elmer-mode-syntax-table)
  (setq-local font-lock-defaults '(ElmerKeywords))
  (setq-local outline-regexp elmer-outline-regexp)
  (setq-local outline-heading-alist elmer-outline-heading-alist)
  (setq-local outline-heading-end-regexp "[^\n]*\n"))


;;** Code Aster


;;*** Automatically choose mode

(add-to-list 'auto-mode-alist '("\\.comm\\'" . code-aster-mode))


;;*** Set variables

(defvar code-aster-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cd" 'convert-source-to-final)
    map))


;;*** Major mode definition

(define-derived-mode code-aster-mode python-mode "Code_Aster"
    "Major mode for editing Code_Aster input files (*.comm)."
    (setq-local code-block-lang "python"))


;;** Gnuplot mode


;;*** Set variables

;; Automatically open files ending with .gp or .gnuplot in gnuplot mode

(setq auto-mode-alist
  (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode))
          auto-mode-alist))


;;*** Hook

(add-hook 'gnuplot-mode-hook
  (lambda ()
    (remove-hook 'after-change-functions 'gnuplot-scan-after-change t)
    (define-key gnuplot-mode-map "\C-cc" 'compile)
    (define-key gnuplot-mode-map "\C-cv" 'TeX-view-manage-windows)))


;;; emacs.el ends here
