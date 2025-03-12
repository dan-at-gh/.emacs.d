;;; elmer --- Elmer mode

;;; Commentary:
;; Load this package with:
;; (require 'elmer)

;;; Code:
;;** Elmer mode

;;*** Automatically choose mode

(add-to-list 'auto-mode-alist '("\\.sif\\'" . elmer-mode))
(add-to-list 'auto-mode-alist '("\\.grd\\'" . elmer-mode))
(add-to-list 'magic-mode-alist '("!Elmer" . elmer-mode))
(add-to-list 'magic-mode-alist '("ElmerGrid" . elmer-mode))


;;*** Set variables, syntax table

(defvar elmer-mode-syntax-table nil
  "Syntax table for `elmer-mode'.")


(setq elmer-mode-syntax-table
     (let ( (synTable (make-syntax-table)))
       (modify-syntax-entry ?! "<" synTable)
       (modify-syntax-entry ?\n ">" synTable)
       synTable))


(defvar elmer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'run-elmer-solver)
    map))


(setq ElmerKeywords '(
        ("^\\*.*" . font-lock-comment-face)
        ("^Simulation" . font-lock-function-name-face)
        ("^End" . font-lock-function-name-face)
        ("^Equation" . font-lock-function-name-face)
        ("^Header" . font-lock-function-name-face)
        ("^Body" . font-lock-function-name-face)
        ("^Solver" . font-lock-function-name-face)
        ("^Materials\sInterval" . font-lock-function-name-face)
        ("^Material\sStructure" . font-lock-function-name-face)
        ("^Material" . font-lock-function-name-face)
        ("^Extruded\sStructure" . font-lock-function-name-face)
        ("^Boundary\sCondition" . font-lock-function-name-face)
        ("^Boundary\sDefinitions" . font-lock-function-name-face)
        ("^Initial\sCondition" . font-lock-function-name-face)
        ("Real" . font-lock-constant-face)
        ("String" . font-lock-constant-face)
        ("Integer" . font-lock-constant-face)
        ("logical" . font-lock-constant-face))
      font-lock-keywords-case-fold-search t
      elmer-outline-regexp (concat
                          "![!*]\\(\\*\\**\\)\s[^\n]\\|"
                          "\s*\\(Header\\|Simulation\\|Body\\|Equation\\|"
                          "Solver\\|Initial\\|Boundary\\|Material\\)[^=]*$")
      elmer-outline-heading-alist '(("Header" . 9)
                                    ("Simulation" . 9)
                                    ("Body" . 9)
                                    ("Equation" . 9)
                                    ("Material" . 9)
                                    ("Solver" . 9)
                                    ("Initial" . 9)
                                    ("Boundary" . 9)))

;;*** Function for running elmer problems

(defun run-elmer-solver ()
  "Run elmer solver from Emacs."
  (interactive)
  (let* (( path (buffer-file-name))
         ( sif-home (directory-file-name (file-name-directory path)))
         ( sif-base (file-name-base path))
         ( sif-time (format-time-string "%Y%m%d_%H%M%S" (current-time))))
    (goto-char (point-min))
    (re-search-forward "Results\sDirectory\s+\"\\(.*\\)\"" nil t)
    (let (( out (match-string 1))
          ( run-file ))
      (setq out (replace-regexp-in-string "<SIF_HOME>" sif-home out t)
            out (replace-regexp-in-string "<SIF_BASE>" sif-base out t)
            out (replace-regexp-in-string "<SIF_TIME>" sif-time out t)
            out (file-name-as-directory out))
      (goto-char (point-min))
      (re-search-forward "Solver\sInput\sFile\s*=\s*\"\\(.*\\)\""
                         nil t)
      (let (( run-file (match-string 1)))
        (setq run-file (replace-regexp-in-string "<SIF_BASE>" sif-base run-file t)
              run-file (concat out run-file))
        (let (( command (concat "ElmerSolver " run-file)))
          (when (y-or-n-p (concat "Run: " command))
            (make-directory out t)
            (save-buffer)
            (dired-copy-file path run-file t)
            (with-current-buffer (find-file-noselect run-file)
              (outline-show-all)
              (widen)
              (goto-char (point-min))
              (while (re-search-forward "<SIF_HOME>\\|<SIF_BASE>\\|<SIF_TIME>"
                                        nil t)
                (cond ((string= (match-string 0) "<SIF_HOME>")
                       (replace-match sif-home t))
                      ((string= (match-string 0) "<SIF_BASE>")
                       (replace-match sif-base t))
                      ((string= (match-string 0) "<SIF_TIME>")
                       (replace-match sif-time t))))
              (save-buffer))
            (compile command)
            (with-current-buffer "*compilation*"
              (rename-buffer (format "*Run %s from %s*" sif-base sif-time)))))))))


;;*** Elmer major mode definition

(define-derived-mode elmer-mode fundamental-mode
  "Elmer" "Major Mode for Elmer Input File"
  (set-syntax-table elmer-mode-syntax-table)
  (setq-local font-lock-defaults '(ElmerKeywords))
  (setq-local outline-regexp elmer-outline-regexp)
  (setq-local outline-heading-alist elmer-outline-heading-alist)
  (setq-local outline-heading-end-regexp "[^\n]*\n"))


(provide 'elmer)

;;; elmer.el ends here
