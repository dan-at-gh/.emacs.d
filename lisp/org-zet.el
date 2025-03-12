;;; org-zet --- Org Zettelkasten

;;; Commentary:
;; Load this package with:
;; (require 'org-zet)

;;; Code:
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
  

(provide 'org-zet)

;;; org-zet.el ends here
