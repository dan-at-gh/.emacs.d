;;; org-roam-publish --- Org Roam publishing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'org-roam-publish)

;;; Code:

(require 'org)
(require 'org-roam)
(require 'ox-publish)


(defvar e/org-publish-base-directory nil)
(defvar e/org-publish-html-link-home nil)
(defvar e/org-publish-html-link-up nil)
(defvar org-html-style-begin "<!-- org-html-head-extra begin -->\n<style>\n"
  "Beginning of header block for org style definitions.")
(defvar org-html-style-end "</style>\n<!-- org-html-head-extra end -->"
  "End of header block for org style definitions.")
(defvar org-html-style-main "")
(defvar org-html-style-comic "")


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
    ("roam-to-gatsby"
     :base-directory "~/.org-roam-public"
     :publishing-directory "/home/dan/business/novel-canvas/gatsby/contents/roam"
     :base-extension "org"
     :publishing-function e/org-html-publish-to-gatsby
     :html-link-home "http://localhost/~dan/org-roam/"
     :body-only t
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
      org-html-style-comic "  /* org-html-style-comic begin */
  .outline-3 h3 {margin-bottom: 5px; margin-top: 20px;}
  .outline-text-3 b {font-weight: normal;}
  .outline-text-3 p {margin: 5px auto;}
  .org-dl {display: grid; grid-template-columns: max-content auto;
           margin-left: 20px; margin-top: 3px; grid-gap: 3px;}
  .org-dl dt {font-weight: normal; grid-column-start: 1;}
  .org-dl dt::after {content: \":\"; margin-left: 3px;}
  .org-dl dd {grid-column-start: 2;}
  /* org-html-style-comic end */\n"
      org-html-head-extra (concat org-html-style-begin
                                  org-html-style-main
                                  org-html-style-comic
                                  org-html-style-end))


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
  "Export of \"emacs\"-links.

PATH: id of the org-roam node,
DESCRIPTION: link title,
BACK-END: exporter back-end,
EXPORT-CHANNEL: export-channel."
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



(provide 'org-roam-publish)

;;; org-roam-publish.el ends here
