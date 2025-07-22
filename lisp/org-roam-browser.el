;;; org-roam-browser --- Org browser actions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'org-roam-browser)

;;; Code:
(require 'org-roam-custom)
(require 'org-roam-publish)

;; required for live-server command:
(setenv "PATH" (concat "/home/dan/.nvm/versions/node/v18.15.0/bin:"
                       (getenv "PATH")))

(defun e/org-publish-live-server ( &optional dir open)
  "Start a live server for org-roam nodes.

DIR is the directory watched by the live server.  OPEN holds the
file to be displayed in the browser."
  (unless dir
    (setq dir "~/public_html"))
  (unless (get-process "live-server")
    (let (( default-directory dir)
          ( command (concat "live-server"
                            (when open (concat " --open=" open)))))
      (start-process-shell-command "live-server" nil command)
      (message "%s" command))
    t))


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
         ;; ( publishing-directory (expand-file-name
         ;;                         (org-publish-property
         ;;                          :publishing-directory
         ;;                          project)))
         )
    (concat org-roam-html-server-url
            (file-name-sans-extension
             (file-relative-name file base-directory))
            ".html#ID-" id)))


(defun e/org-browser-open-file ( file id)
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



(provide 'org-roam-browser)

;;; org-roam-browser.el ends here
