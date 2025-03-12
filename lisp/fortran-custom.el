;;; fortran-custom --- Fortran Customization

;;; Commentary:
;; Load this package with:
;; (require 'fortran-custom)

;;; Code:



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
 
(provide 'fortran-custom)

;;; fortran-custom.el ends here
