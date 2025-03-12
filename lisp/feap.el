;;; feap --- Feap mode

;;; Commentary:
;; Load this package with:
;; (require 'feap)

;;; Code:
;;** Feap mode


;;*** Set emacs environment variables

(setenv "FEAPPVHOME4_1" "/home/dan/feap/ver4.1")


;;*** Set variables

(defvar feap-mode-syntax-table
  (let (( synTable (make-syntax-table)))
    (modify-syntax-entry ?! "<" synTable)
    (modify-syntax-entry ?\n ">" synTable)
    synTable)
"Syntax table for `feap-mode'.")

(defvar feap-font-lock-keywords
'(("[^\s\n,]\\{15,255\\}" . whitespace-line)
  ("^[ ]*[fF][eE][aA][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[eE][nN][dD][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[bB][lL][oO][cC][^ ,\n]*" . font-lock-constant-face)
  ("^[ ]*[eE][bB][oO][uU][^ ,\n]*" . font-lock-constant-face)
  ("^[ ]*[cC][fF][oO][rR][^ ,\n]*" . font-lock-constant-face)
  ("^[ ]*[bB][aA][tT][cC][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[fF][oO][rR][cC][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[bB][oO][uU][nN][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[mM][aA][tT][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[sS][tT][oO][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[iI][nN][tT][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[pP][lL][oO][tT][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[fF][oO][rR][mM][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][aA][nN][gG][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[uU][tT][aA][nN][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[sS][oO][lL][vV][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[dD][iI][sS][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[eE][dD][iI][sS][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[cC][dD][iI][sS][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[sS][tT][rR][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[lL][oO][oO][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[nN][eE][xX][tT][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][rR][aA][nN][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[dD][tT][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][iI][mM][eE][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[pP][rR][oO][pP][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][oO][lL][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[iI][nN][cC][lL][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[tT][oO][rR][sS][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[cC][hH][eE][cC][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[eE][lL][eE][mM][^ ,\n]*" . font-lock-function-name-face)
  ("^[ ]*[cC][oO][oO][rR][^ ,\n]*" . font-lock-function-name-face)))


;;*** Major mode definition

(define-derived-mode feap-mode fundamental-mode "Feap"
  "Major Mode for Feap Input File"
  :syntax-table feap-mode-syntax-table
  (setq-local font-lock-defaults
    '( feap-font-lock-keywords nil nil nil))
  (setq-local comment-start "!")
  (setq-local comment-end ""))

(add-to-list 'magic-mode-alist '("FEAP" . feap-mode))
(add-to-list 'magic-mode-alist '("!FEAP" . feap-mode))



(provide 'feap)

;;; feap.el ends here
