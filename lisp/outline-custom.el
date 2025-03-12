;;; outline-custom --- My extended outline configuration

;;; Commentary:


;;; Code:

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

(provide 'outline-custom)

;;; outline-custom.el ends here


