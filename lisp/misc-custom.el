;;; misc-custom --- General customization -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'misc-custom)

;;; Code:

(require 'time)
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
        (kill-buffer buffer)))))

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

OMIT-NULLS is passed to `split-string'."
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string (buffer-string) "\n" omit-nulls)))

(defun global-disable-mode ( mode-fn)
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
  (let ((save (copy-tree buffer-undo-list)))
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
some special cases.  These cases are characterized by multiple
choice selection rather than selection by completion.  Partly
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
       (lambda ()
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


;;** Key bindings

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)


;;** Modeline

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


;;** Visual Line Mode and Fill Column


(global-visual-line-mode t)
(setq-default fill-column 80)

;;** Search current word under cursor

(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_],
independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Created: 2010-05-29
Version: 2025-02-05"
  (interactive)
  (if isearch-mode
      (isearch-repeat-forward 1)
    (let (xbeg xend)
      (if (region-active-p)
          (setq xbeg (region-beginning) xend (region-end))
        (save-excursion
          (skip-chars-backward "-_A-Za-z0-9")
          (setq xbeg (point))
          (right-char)
          (skip-chars-forward "-_A-Za-z0-9")
          (setq xend (point))))
      (when (< xbeg (point)) (goto-char xbeg))
      (isearch-mode t)
      (isearch-yank-string (buffer-substring-no-properties xbeg xend)))))


(provide 'misc-custom)

;;; misc-custom.el ends here
