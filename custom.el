(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))))
 '(indent-tabs-mode nil)
 '(org-agenda-files '("~/.emacs.d/org/test.org"))
 '(org-html-mathjax-options
   '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
     (scale 1.0)
     (align "center")
     (font "mathjax-modern")
     (overflow "scroll")
     (tags "ams")
     (indent "0em")
     (multlinewidth "85%")
     (tagindent ".8em")
     (tagside "right")))
 '(org-roam-notation-mathjax-macros
   '(("rforce" "Z" "\\rforce,\\, \\mathbf{\\rforce}" "Constraining (reaction) force vector" "z" "alpha" "\\newt, \\kg\\frac{\\meter}{\\sec^2}")
     ("pos" "x" "\\pos_i, \\,\\mathbf{\\pos}" "Position vector in actual configuration" "x" "alpha" "\\meter")
     ("POS" "X" "\\POS_i, \\, \\mathbf{\\POS}" "Position vector in actual configuration" "x" "alpha" "\\meter")
     ("work" "W" "\\work" "Work" "w" "alpha" "\\kg \\frac{\\meter^2}{\\sec^2}")
     ("VOL" "V" "\\VOL" "Volume in the reference configuration" "v" "alpha" "\\meter^3")
     ("vol" "v" "\\vol" "Volume in the actual configuration" "v" "alpha" "\\meter^3")
     ("vel" "v" "\\vel_i, \\, \\mathbf{\\vel}" "Velocity" "v" "alpha" "\\frac{\\meter}{\\sec}")
     ("trac" "t" "\\trac_i, \\, \\mathbf{\\trac}" "Traction" "t" "alpha" "")
     ("temp" "\\\\theta" "\\temp" "Temperature" "t" "greek" "K")
     ("strscau" "T" "\\strscau_{ij},\\, \\mathbf{\\strscau}" "Cauchy stress tensor" "t" "alpha" "")
     ("time" "t" "t" "Time" "t" "alpha" "\\sec")
     ("sec" "s" "s" "Second" "s" "unit" "")
     ("mdens" "\\\\rho" "\\mdens" "Mass density" "r" "greek" "\\frac{\\kg}{\\meter^3}")
     ("fenergy" "\\\\psi" "\\fenergy" "Helmholtz free energy density" "psi" "greek" "")
     ("FENERGY" "\\\\Uppsi" "\\FENERGY" "Helmholtz free energy" "psi" "greek" "")
     ("pres" "p" "\\pres" "Pressure" "p" "alpha" "\\kg\\frac{\\meter^2}{\\sec^2}")
     ("spint" "\\\\Omega" "\\spint_{ij},\\, \\boldsymbol{\\spint}" "Spin tensor" "o" "greek" "\\frac{1}{\\sec}")
     ("nrml" "n" "n_i, \\, \\mathbf{\\nrml}" "Normal in actual configuration" "n" "alpha" "-")
     ("newt" "N" "N" "Newton" "n" "unit" "")
     ("mom" "p" "\\mom_i, \\, \\mathbf{\\mom}" "Momentum, linear-" "m" "alpha" "\\kg\\frac{\\meter}{\\sec}")
     ("mass" "m" "\\mass" "Mass" "m" "alpha" "\\kg")
     ("meter" "m" "m" "Meter" "m" "unit" "")
     ("velgrad" "L" "\\velgrad_{ij}, \\, \\mathbf{\\velgrad}" "Velocity gradient" "l" "alpha" "\\frac{1}{\\sec}")
     ("lforce" "K" "\\lforce_i, \\, \\mathbf{\\lforce}" "Load (impressed) force vector" "k" "alpha" "\\newt, \\kg\\frac{\\meter}{\\sec^2}")
     ("kg" "kg" "kg" "Kilo gramm" "k" "unit" "")
     ("kel" "K" "K" "Kelvin" "k" "unit" "")
     ("hamil" "H" "\\hamil" "Hamiltonian" "h" "alpha" "")
     ("force" "F" "\\force_i,\\, \\mathbf{\\force}" "Force vector" "f" "alpha" "\\newt, \\kg\\frac{\\meter}{\\sec^2}")
     ("deformg" "F" "\\deformg_{ij}, \\,\\mathbf{\\deformg}" "Deformation gradient" "f" "alpha" "-")
     ("bfrc" "b" "\\bfrc_i, \\, \\mathbf{\\bfrc}" "Body force (density)" "b" "alpha" "")
     ("AREA" "A" "\\AREA" "Area in the reference configuration" "a" "alpha" "\\meter^2")
     ("area" "a" "\\area" "Area in the actual configuration" "a" "alpha" "\\meter^2")))
 '(package-selected-packages
   '(org-ai request gptel use-package project-mode-line-tag js2-mode ledger-mode auctex auto-complete bbdb flycheck fountain-mode ivy markdown-mode org-journal org-roam org-transclusion web-mode writeroom-mode yaml-mode org htmlize wc-mode org-download apache-mode org-superstar highlight-indent-guides rjsx-mode gnuplot-mode font-lock-studio dired+ lua-mode fill-column-indicator ebib))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (ispell-dictionary . "english")
     (org-ascii-text-width . 70)
     (ispell-local-dictionary . english)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-indentation ((t (:foreground "firebrick"))))
 '(whitespace-space ((t (:foreground "lightgray")))))
