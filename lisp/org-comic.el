;;; org-comic --- Org comic mode

;;; Commentary:
;; Load this package with:
;; (require 'org-comic)

;;; Code:

(require 'org)
;;*** Comic Script


(defun org-comic-lighter-string ()
  (setq org-comic-lighter
        (concat " comic["
                (number-to-string org-comic-page-count) "|"
                (number-to-string org-comic-panel-count) "]")))


(defvar-local org-comic-lighter nil
  "This variable holds the per-buffer word-count statistics used to
update the modeline.")


;; (setq org-comic-timer-tracker
;;       (run-with-idle-timer
;;        0 t
;;        '(lambda ()
;;           (when (org-comic-heading-p)
;;             (setq org-comic-lighter (org-comic-lighter-string))))))


(define-minor-mode org-comic-mode
  "Toggle use of org mode comic fontification."
  :init-value nil
  :lighter (:eval org-comic-lighter)
  (cond (org-comic-mode
         (org-comic-enumerate)
         (add-hook 'after-change-functions #'org-comic-after-change nil 'local)
         (add-hook 'org-export-before-processing-functions
                   #'org-comic-export-before-processing nil 'local)
         (add-hook 'org-ctrl-c-ctrl-c-hook #'org-comic-new-character nil 'local)
         (font-lock-fontify-buffer)
         )
        (t
         (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-comic-new-character 'local)
         (remove-hook 'after-change-functions #'org-comic-after-change 'local)
         (remove-hook 'org-export-before-processing-functions
                      #'org-comic-export-before-processing 'local)
         (remove-overlays (point-min) (point-max) 'name 'org-comic)
         (org-comic-display-upcase-remove)
         (font-lock-fontify-buffer))))


(defvar-local org-comic-page-count 0)


(defvar-local org-comic-panel-count 0)


(defun org-comic-heading-p ()
  (and org-comic-mode
       (save-excursion
         (beginning-of-line)
         (when (and (looking-at org-complex-heading-regexp)
                    (match-beginning 4))
           (goto-char (match-beginning 4))
           (when (looking-at "p\\|\\[")
             (re-search-forward "\\(page\\|panel\\)" (match-end 4) t))))))


(defun org-comic-after-change ( beg end _)
  (when (org-comic-heading-p)
    (org-comic-enumerate)))


(defun org-comic-enumerate ( &optional export)
  (remove-overlays (point-min) (point-max) 'name 'org-comic)
  (let (( page 0)
        ( panel 0)
        ( panelTotal 0))
    (org-map-entries
     (lambda ()
       (when (org-comic-heading-p)
         (let* (( face (get-text-property (line-beginning-position) 'face))
                ( overlay (make-overlay (match-beginning 1)
                                        (match-end 1))))
           (overlay-put overlay 'name 'org-comic)
           (cond ((string= (downcase (match-string 1)) "page")
                  (setq page (1+ page)
                        panel 0)
                  (if export
                      (replace-match (concat "PAGE " (number-to-string page))
                                     t nil nil 1)
                    (overlay-put overlay 'after-string
                                 (concat " " (propertize (number-to-string page)
                                                         'font-lock-face face)))))
                 ((string= (downcase (match-string 1)) "panel")
                  (setq panel (1+ panel)
                        panelTotal (1+ panelTotal))
                  (if export
                      (replace-match (concat "PANEL " (number-to-string panel))
                                     t nil nil 1)
                    (overlay-put overlay 'after-string
                                 (concat " " (propertize (number-to-string panel)
                                                         'font-lock-face face))))))))))
    (setq org-comic-page-count page
          org-comic-panel-count panelTotal)
    (org-comic-lighter-string)))


(defun org-comic-export-before-processing ( backend)
  (org-comic-enumerate 'export))
  

(defun org-comic-get-characters ()
  (let (( current-point (point))
        characters)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-list-full-item-re nil t)
        (when (and (match-string 4)
                   (or (< current-point (match-beginning 4))
                       (< (match-end 4) current-point )))
          (setq characters (cons (upcase (match-string-no-properties 4))
                                 characters))))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\([A-Z][^*]+[A-Z]\\)\\*" nil t)
        (when (and (match-string 1)
                   (or (< current-point (match-beginning 1))
                       (< (match-end 1) current-point )))
          (setq characters (cons (upcase (match-string-no-properties 1))
                                 characters)))))
    (delq nil (delete-dups characters))))


(defvar org-comic-ac-default-dictionary
  '("PAGE" "PANEL" "CAPTION" "SFX" "NARRATION"))


(defvar org-comic-panel-descriptors
  '("CLOSE-UP" "LONG SHOT" "DETAIL" "INT." "EXT."
    "DAY" "NIGHT" "LOW-ANGLE" "HIGH-ANGLE" "BIRD'S-EYE"
    "OVER-THE-SHOULDER" "ESTABLISHING"))


(defvar org-comic-caption-descriptors
  '("TIME/PLACE" "VOICE OVER"))


(defvar org-comic-dialogue-descriptors
  '("OFF" "WHISPER" "BURST" "WEAK" "SINGING" "THINKING"))


(defun org-comic-set-ac-dictionary ( &optional remove)
  "Provide the word list for auto-completion.

Set local variable `ac-buffer-dictionary' to the value of
`org-comic-get-characters'. `ac-buffer-dictionary' is then
referenced by variable `ac-source-dictionary', which in turn must
be added to local variable `ac-sources' list."
  (if remove
      (setq ac-buffer-dictionary nil
            ac-sources '(ac-source-words-in-same-mode-buffers))
    (setq ac-buffer-dictionary (append org-comic-ac-default-dictionary
                                       org-comic-panel-descriptors
                                       org-comic-caption-descriptors
                                       org-comic-dialogue-descriptors
                                       (org-comic-get-characters))
          ac-sources '( ac-source-dictionary))))


(defun org-comic-new-character ()
  (interactive)
  (atomic-change-group
    (beginning-of-line)
    (unless (looking-at "^\s*$")
      (if (org-at-item-p)
          (org-end-of-item)
        (end-of-paragraph-text)
        (beginning-of-line 2)))
    (insert "\n")
    (beginning-of-line 0)
    (let (( character (completing-read "Character: "
                                       (org-comic-get-characters)))
          ( descriptor (completing-read "Descriptor: "
                                        (append org-comic-caption-descriptors
                                                org-comic-dialogue-descriptors))))
      (insert "- " (upcase character)
              (if (string-empty-p descriptor)
                  ""
                (concat " (" (upcase descriptor) ")"))
              " :: ")))
  t)

  
(defun org-comic-new-character-inline ()
  (interactive)
  (insert "*" (completing-read "Character: "
                               (org-comic-get-characters))
          "*"))


(defun org-comic-fontify-links-old ( limit)
  (when org-comic-mode
    (let ( match)
      (while (and (setq match (text-property-search-forward 'face))
                  (< (point) limit))
        (let (( start (prop-match-beginning match))
              ( end (prop-match-end match))
              ( value (prop-match-value match)))
          (if (listp value)
              (when (member 'org-link value)
                (remove-text-properties start end '(face nil))
                (setq value (delete 'org-link value))
                (dolist ( face value)
                  (add-face-text-property start end face 'append)))
            (when (eq value 'org-link)
              (remove-text-properties start end '(face nil)))))))))


(defun org-comic-display-upcase ( start end)
  (while (< start end)
    (compose-region start (1+ start)
                    (upcase (char-to-string (char-after start))))
    (setq start (1+ start))))


(defun org-comic-display-upcase-remove ()
  (save-excursion
    (goto-char (point-min))
    (let ( match)
      (while (and (setq match (text-property-search-forward 'display))
                  (string= (upcase (buffer-substring (1- (point)) (point)))
                           (prop-match-value match)))
        (remove-text-properties (1- (point)) (point) '(display nil))))))
                          

(defun org-comic-fontify-links ( limit)
  (when org-comic-mode
    (let ( success)
      (while (setq success (re-search-forward org-bracket-link-regexp limit t))
        (let* (( beg (match-beginning 0))
               ( end (match-end 0))
               ( face (get-text-property beg 'face))
               ( beg2 (match-beginning 2))
               ( end2 (match-end 2)))
          (if (and (listp face)
                   (member 'org-link face))
              (let (( add-faces (delete 'org-link face)))
                (remove-text-properties beg end '(face nil))
                (dolist ( item add-faces)
                  (add-face-text-property beg end item 'append)))
            (when (eq face 'org-link)
              (remove-text-properties beg end '(face nil))))
          (when (match-string 2)
            (org-comic-display-upcase beg2 end2))))
      success)))
            

(defun org-comic-upcase-links ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp nil t)
      (when (match-string 2)
        (replace-match (upcase (match-string 2)) t nil nil 2)))))


(defun org-comic-upcase-links-overlay ()
  (when (re-search-forward org-bracket-link-regexp (line-end-position) t)
    (when (match-string 2)
      (let (( overlay (make-overlay (match-beginning 2) (match-end 2))))
        (overlay-put overlay 'name 'org-comic-upcase)
        (overlay-put overlay 'display (upcase (match-string 2)))))))
  

(defun org-comic-font-lock-add-keywords ()
  (add-to-list 'org-font-lock-extra-keywords
               '( org-comic-fontify-links ( 0 nil append t))
               'append))


(add-hook 'org-font-lock-set-keywords-hook
          #'org-comic-font-lock-add-keywords)



(provide 'org-comic)

;;; org-comic.el ends here
