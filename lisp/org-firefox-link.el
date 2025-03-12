;;; org-firefox-link --- Grab firefox link

;;; Commentary:
;; Load this package with:
;; (require 'org-firefox-link)

;;; Code:
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


(provide 'org-firefox-link)

;;; org-firefox-link.el ends here
