;;; blender --- Blender Addon Development

;;; Commentary:
;; Load this package with:
;; (require 'blender)

;;; Code:

(defun bpy-save-with-init-buffer ()
  "Mark Blender addon as modified and prepare for reloading."
  (interactive)
  (save-buffer)
  (with-current-buffer
      (find-file-noselect
       (concat (file-name-directory (buffer-file-name)) "__init__.py"))
    (set-buffer-modified-p t)
    (save-buffer)))

(provide 'blender)

;;; blender.el ends here
