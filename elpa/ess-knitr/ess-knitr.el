;; Copyright (C) 2012 Simon Potter
;; Created: 22 May 2012
;; https://sjp.co.nz/posts/emacs-ess-knitr/

;; Define key-bindings for calling 'knit' and 'purl;
;; 'M-n r' - knit the current buffer
;; 'M-n u' - purl the current buffer
(define-key noweb-minor-mode-map "\M-nr" 'ess-swv-knit)
(define-key noweb-minor-mode-map "\M-nu" 'ess-swv-purl)

;; The following two functions call knit() and purl() on the current
;; buffer. The 'knitr' package is also attempted to be loaded because
;; the ess-swv-run-in-R function assumes that functions it calls are
;; already available (as is the case with Sweave & Stangle). This
;; could be fixed by modifying ESS code but is not necessary. By
;; calling the ESS function using a compound expression we get the
;; desired result.
(defun ess-swv-knit ()
  "Run knit on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "require(knitr) ; knit"))

(defun ess-swv-purl ()
  "Run purl on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "require(knitr) ; purl"))

(provide 'ess-knitr)
