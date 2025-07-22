;;; emacs --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(require 'desktop)
;; Bug string-trim is undefined
(require 'subr-x)
;; Bug dead-key is undefined
(require 'iso-transl)

;; Put here custom packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/business/novel-canvas/gatsby/lisp/")

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(and window-system (server-start))

(require 'global-keys)
(require 'org)
(require 'org-journal)
(require 'org-agenda-custom)
(require 'org-download)
(require 'org-ai)
(require 'ox-md)
(require 'org-roam-dailies)
(require 'org-roam-export)
(require 'time)
(require 'browse-url)
(require 'markdown-mode)
(require 'ispell)
(require 'dired-x)
(require 'conf-mode)
(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)

(use-package writeroom-mode
  :config
  (setq writeroom-restore-window-config t))

;; local
(require 'org-download-custom)
(require 'org-capture-custom)
(require 'org-roam-gatsby)
(require 'org-latex-custom)
(require 'org-cite-custom)
(require 'org-roam-publish)
(require 'org-roam-notation)
(require 'org-roam-custom)
(require 'dailies-drift)
(require 'org-roam-ivy)
(require 'org-doi)
(require 'org-roam-browser)
(require 'org-edit-extra)
(require 'org-firefox-link)
(require 'org-custom)
(require 'org-journal-custom)
(require 'org-comic)
(require 'ispell-custom)
(require 'outline-custom)
(require 'cfold)
(require 'calendar-custom)
(require 'dired-custom)
(require 'obuffer)
(require 'misc-custom)
(require 'grep-custom)
(require 'ivy-custom)
(require 'elisp-custom)
(require 'prog-custom)
(require 'c-custom)
(require 'python-custom)
(require 'blender)
(require 'shell-custom)
(require 'fortran-custom)
(require 'web-custom)
(require 'latex-custom)
(require 'bibtex-custom)
(require 'markdown-custom)
(require 'feap)
(require 'elmer)
(require 'gnuplot-custom)
(require 'gptel-custom)

(add-hook 'buffer-menu-mode-hook
   (lambda ()
     (hl-line-mode)
     (font-lock-add-keywords nil
        '(("Emacs.*" . font-lock-warning-face)
          ("Python.*" . font-lock-variable-name-face)
          ("Org.*" . font-lock-constant-face)
          ("LaTeX.*" . font-lock-function-name-face)
          ("Shell.*" . font-lock-warning-face)
          ("Text.*" . font-lock-keyword-face)))))

(add-hook 'package-menu-mode-hook 'hl-line-mode)
 
(add-hook 'conf-unix-mode-hook
  (lambda ()
    (setq-local outline-regexp "#[#*]\\(\\*\\**\\)\s[^\n]")
    (outline-minor-mode 1)
    (outline-hide-body)
    (define-key conf-unix-mode-map "\C-cc" 'compile)))

(add-hook 'compilation-filter-hook
  (lambda ()
    (comint-truncate-buffer)))


(provide 'emacs)

;;; emacs.el ends here
