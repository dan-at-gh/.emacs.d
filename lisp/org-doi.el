;;; org-doi --- Org DOI collection

;;; Commentary:
;; Load this package with:
;; (require 'org-doi)

;;; Code:
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
        


(provide 'org-doi)

;;; org-doi.el ends here
