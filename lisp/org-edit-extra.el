;;; org-edit-extra --- Org extra edit commands

;;; Commentary:
;; Load this package with:
;; (require 'org-edit-extra)

;;; Code:

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


(provide 'org-edit-extra)

;;; org-edit-extra.el ends here
