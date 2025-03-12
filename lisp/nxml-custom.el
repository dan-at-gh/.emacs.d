;;; nxml-custom --- nXML mode customization -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'nxml-custom)

;;; Code:
;;** nXML mode

(defun insert-ar-region-command (start end)
  "Article.
The <ar> tag groups together all the stuff related to one key-phrase.
Insert a markup <ar></ar> around a region."
  (interactive "r")
  (goto-char end) (insert "</ar>")
  (goto-char start) (insert "<ar>")
  (goto-char (- (point) 0)))
(defun insert-k-region-command (start end)
  "Key Phrase. Insert a markup <k></k> around a region."
  (interactive "r")
  (goto-char end) (insert "</k>")
  (goto-char start) (insert "<k>")
  (goto-char (- (point) 0)))
(defun insert-def-region-command (start end)
  "Definition.
This tag cotains either whole body of a word article inside <ar>,
or a definition itself or a group of definitions,
that fall into a certain category.
Insert a markup <def></def> around a region."
  (interactive "r")
  (goto-char end) (insert "</def>")
  (goto-char start) (insert "<def>")
  (goto-char (- (point) 0)))
(defun insert-sr-region-command (start end)
  "Semantik Relation. Insert a markup <sr></sr> around a region."
  (interactive "r")
  (goto-char end) (insert "</sr>")
  (goto-char start) (insert "<sr>")
  (goto-char (- (point) 0)))
(defun insert-ex-region-command (start end)
  "Example, Quote. Insert a markup <ex></ex> around a region."
  (interactive "r")
  (goto-char end) (insert "</ex>")
  (goto-char start) (insert "<ex>")
  (goto-char (- (point) 0)))
(defun insert-co-region-command (start end)
  "Comment to Key Phrase. Insert a markup <ex></ex> around a region."
  (interactive "r")
  (goto-char end) (insert "</co>")
  (goto-char start) (insert "<co>")
  (goto-char (- (point) 0)))
(defun insert-kref-region-command (start end)
  "Link. Insert a markup <kref></kref> around a region."
  (interactive "r")
  (goto-char end) (insert "</kref>")
  (goto-char start) (insert "<kref>")
  (goto-char (- (point) 0)))
(defun insert-gr-region-command (start end)
  "Grammar. Insert a markup <gr></gr> around a region."
  (interactive "r")
  (goto-char end) (insert "</gr>")
  (goto-char start) (insert "<gr>")
  (goto-char (- (point) 0)))
(defun insert-opt-region-command (start end)
  "Inside k-tag. Insert a markup <opt></opt> around a region."
  (interactive "r")
  (goto-char end) (insert "</opt>")
  (goto-char start) (insert "<opt>")
  (goto-char (- (point) 0)))
(defun insert-dtrn-region-command (start end)
  "Direct Translation (of key word).
Insert a markup <dtrn></dtrn> around a region."
  (interactive "r")
  (goto-char end) (insert "</dtrn>")
  (goto-char start) (insert "<dtrn>")
  (goto-char (- (point) 0)))

(add-hook 'nxml-mode-hook
    (lambda ()
         (define-key nxml-mode-map "\C-ca" 'insert-ar-region-command)
         (define-key nxml-mode-map "\C-ck" 'insert-k-region-command)
         (define-key nxml-mode-map "\C-cd" 'insert-def-region-command)
         (define-key nxml-mode-map "\C-cs" 'insert-sr-region-command)
         (define-key nxml-mode-map "\C-ce" 'insert-ex-region-command)
         (define-key nxml-mode-map "\C-cc" 'insert-co-region-command)
         (define-key nxml-mode-map "\C-cr" 'insert-kref-region-command)
         (define-key nxml-mode-map "\C-cg" 'insert-gr-region-command)
         (define-key nxml-mode-map "\C-co" 'insert-opt-region-command)
         (define-key nxml-mode-map "\C-ct" 'insert-dtrn-region-command)))
 

(provide 'nxml-custom)

;;; nxml-custom.el ends here
