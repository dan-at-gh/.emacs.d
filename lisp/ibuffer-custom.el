;;; ibuffer-custom --- IBuffer customization

;;; Commentary:

;;; Code:

;;** IBuffer mode


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


(provide 'ibuffer-custom)

;;; ibuffer-custom.el ends here
