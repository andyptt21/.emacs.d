;;; init.el --- Andy Patt's emacs configuration
;; This initialization file mostly provides support for writing literate R code with org-babel, Rmarkdown or Rnw in emacs
;; I also use org-mode a lot, and occasionally upload to my wordpress blog
;; The theme is a modified version of spacemacs light theme
;; I am currently running Emacs 25.3.1 on macOS Mojave Version 10.14.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom set stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 ;;'(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(ess-swv-pdflatex-commands (quote ("pdflatex" "texi2pdf" "make")))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(org-confirm-babel-evaluate nil)
 '(package-archives
   (quote
    (("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (spaceline-all-the-icons mode-icons all-the-icons org-journal pubmed fill-column-indicator auctex poly-org ess-view exec-path-from-shell ess-smart-underscore pdf-tools org-ref popup-complete auto-complete spaceline-config persp-mode spaceline openwith org2blog elpy poly-R poly-markdown ess diminish use-package spacemacs-theme magit org org-bullets markdown-mode markchars)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo"))))
 ;;'(mouse ((t (:background "black"))))
 ;;'(org-table ((t (:background "#D8C3E5" :foreground "#655370"))))
 )

(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "elisp" user-emacs-directory) load-path))
(update-load-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS
(use-package ess
  :ensure t
  :init (require 'ess-r-mode)
  :config (ess-toggle-underscore nil))
(add-to-list 'load-path "/elpa/ess-18.10.2")
;;(setq inferior-R-program-name "/usr/local/bin/R")
(setq ess-use-auto-complete t)
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)
(auto-complete-mode)

;; elpy (for python editing)
 (use-package elpy
   :ensure t
   :config (elpy-enable))

;; flycheck for python syntax checking
;; Suspending for now, seems to freeze up python buffers and not helpful
 ;; (use-package flycheck
 ;;   :config (when (require 'flycheck nil t)
 ;; 	    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
 ;; 	    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; Org mode
(use-package org
  :ensure org-plus-contrib
  :bind (("\C-c l" . org-store-link)
	 ("\C-c a" . org-agenda)
	 ("\C-c i" . org-toggle-inline-images))
  :config
  (setq org-tag-persistent-alist 
	'((:startgroup . nil)
	  ("LIPOSARCOMA" . ?h) 
	  ("PROTEOGENOMICS" . ?r)
	  ("BAYESIAN NETWORKS" . ?t)
	  ("LIFE" . ?y)
	  (:endgroup . nil)
	  (:startgroup . nil)
	  ("EASY" . ?e)
	  ("MEDIUM" . ?m)
	  ("HARD" . ?a)
	  (:endgroup . nil)
	  ("URGENT" . ?u)
	  ("KEY" . ?k)
	  ("BONUS" . ?b)
	  ("noexport" . ?x)  
	  )
	)
  (setq org-tag-faces
	'(
	  ("LIPOSARCOMA" . (:foreground "GoldenRod" :weight bold))
	  ("PROTEOGENOMICS" . (:foreground "Red" :weight bold))
	  ("BAYESIAN NETWORKS" . (:foreground "SkyBlue" :weight bold))
	  ("LIFE" . (:foreground "Purple" :weight bold))
	  ("EASY" . (:foreground "LimeGreen" :weight bold))  
	  ("MEDIUM" . (:foreground "OrangeRed" :weight bold))  
	  ("HARD" . (:foreground "OrangeRed" :weight bold))  
	  ("BONUS" . (:foreground "GoldenRod" :weight bold))
	  ("noexport" . (:foreground "LimeGreen" :weight bold))  
	  )
	)
  (progn
    (setq org-hide-emphasis-markers t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (python . t)
       (sql . t)))))
;;(setq org-startup-with-inline-images t)
;;(setq org-log-done )))

;; Org Ref
(use-package org-ref
  :after org
  :init
  (setq reftex-default-bibliography '("~/Documents/emacs_files/references.bib"))
  (setq org-ref-bibliography-notes "~/Documents/emacs_files/notes.org"
	org-ref-default-bibliography '("~/Documents/emacs_files/references.bib")
	org-ref-pdf-directory "~/Desktop/Papers_of_Interest/")

  (setq helm-bibtex-bibliography "~/Documents/emacs_files/references.bib")
  (setq org-latex-pdf-process
	'("pdflatex %f" "bibtex %b" "pdflatex %f" "pdflatex %f"))
  (setq helm-bibtex-library-path "~/Desktop/Papers_of_Interest/")

  (setq helm-bibtex-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))
  (setq helm-bibtex-notes-path "~/Documents/emacs_files/notes.org")
  :config
  (key-chord-define-global "uu" 'org-ref-cite-hydra/body)
  ;; variables that control bibtex key format for auto-generation
  ;; I want firstauthor-year-title-words
  ;; this usually makes a legitimate filename to store pdfs under.
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

;; poly-org mode for editing R blocks inline in org files
(use-package poly-org
  :ensure t
  :defer t)
;  :mode (("\\.org\\'" . poly-org-mode)
;	 ("\\.Org\\'" . poly-org-mode)))

;; Fancy org-bullets
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;spaceline
;; (use-package spaceline-config
;;   :ensure spaceline
;;   :config
;;   (setq powerline-default-separator 'wave
;;         spaceline-workspace-numbers-unicode t
;;         spaceline-window-numbers-unicode t)
;;   (spaceline-spacemacs-theme)
;;   (spaceline-info-mode))

(use-package persp-mode)

;; ess-view, REQUIRES "TAD" CSV VIEWER APP
(use-package ess-view
 :ensure t
 :config (setq ess-view--spreadsheet-program "/Applications/Tad.app/Contents/MacOS/Tad"))

;; Markdown-mode (needed for Rmarkdown)
(use-package markdown-mode
   :ensure t
   :commands (markdown-mode gfm-mode)
   :mode (("\.text\'" . markdown-mode)
	  ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)))

;; poly-R and poly-markdown also used for Rmarkdown 
(use-package poly-markdown
  :ensure t
  :pin melpa-stable
  :commands (poly-markdown-mode)
  :mode (("\\.rmd\\'" . poly-markdown-mode)
	 ("\\.Rmd\\'" . poly-markdown-mode)))
(use-package poly-R
  :ensure t
  :pin melpa-stable)

;;Magit
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;;Fill column indicator to keep code pretty. Toggle with 'fci-mode'
(use-package fill-column-indicator
  :ensure t
  :defer t
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "gainsboro")
  (setq fci-rule-use-dashes t))
;; Start a new journal entry with C-c C-j
(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/Documents/emacs_files/journal/"))

;; Cool icons for the spaceline
;; (use-package spaceline-all-the-icons
;;   :ensure t
;;   :after spaceline
;;   :config
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-toggle-all-the-icons-flycheck-status-info-off)
;;   (spaceline-toggle-all-the-icons-modified-off))

;; Download all icons needed for spaceline
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode)
  (setq doom-modeline-minor-modes t))

(use-package treemacs
  :ensure t)

(use-package treemacs-icons-dired
  :ensure t
  :config
  (treemacs-icons-dired-mode))


;; Can't get pdf-tools to install at the moment but I hear it's very good
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linking to my wordpress blog and OSC account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "~/.emacs.d/elisp/blog_and_osc_login.el") (load "~/.emacs.d/elisp/blog_and_osc_login.el") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Never show line numbers
(global-linum-mode nil)
;;(setq linum-format "%4d \u2502 ")

;; Set background to light gray
;;(set-background-color "#DCDCDC")
;;(set-face-attribute 'fringe nil :background "#DCDCDC" :foreground "#DCDCDC")
;;(set-face-background 'linum "#DCDCDC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-ivy)

;; Ensure the correct $PATH variables are inherited on Mac
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Command-S to save, C to copy, V to paste, etc.
(defun sj/copy-keys-from-keymap (from-map keys &optional to-map)
  "Copy the definitions of key sequences in `keys' from `from-map' to `to-map'.
A new keymap is created if `to-map' is nil.  `keys' should be a
list of the keys whose bindings are to be copied.  Each entry may
also be of the form (from-key . to-key) if the keys differ in the
two keymaps.
Example:
  (\"a\" [backspace]
   (\"v\"  . \"k\")
   ([?v] . [?\C-o])
   (\"\C-y\" . \"x\"))
The keymap will have `from-map's bindings for \"v\" on \"k\" and \"\C-o\",
and the binding for \"\C-y\" on \"x\". The bindings for \"a\" and [backspace]
will be copied as well."
  (let ((new-map (or to-map (make-sparse-keymap))))
    (dolist (entry keys)
      (let ((from-key (if (listp entry) (car entry) entry))
	    (to-key   (if (listp entry) (cdr entry) entry)))
	(define-key new-map to-key (lookup-key from-map from-key))))
     new-map))

;; Distinguish between various Emacs ports to OS X
(cond 
 ;; ns port
 ((boundp 'ns-version-string)
  (setq ns-antialias-text t
	ns-option-modifier 'meta)
  (define-key global-map [ns-drag-file] 'ns-find-file))
 ;; mac port
 ((boundp 'mac-carbon-version-string)
  (setq mac-command-modifier 'super
	mac-option-modifier  'meta)
  ;; Command-S to save, C to copy, V to paste, etc.
  (let ((keys '(("\C-x\C-s"    . [(super s)])
		("\C-w"        . [(super x)])
		("\M-w"        . [(super c)])
		("\C-y"        . [(super v)])
		([(control /)] . [(super z)]))))
(sj/copy-keys-from-keymap global-map keys global-map))))

;; Turn off excessive alarm bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; autocomplete paired brackets (don't autocomplete '<')
(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))
(setq electric-pair-preserve-balance nil)

;;enable parentheses matching
(show-paren-mode 1)

;; Re-load your buffers from the previous session on startup
(desktop-save-mode 1)

;; Prevent upcase-region (annoying when I'm spamming undo)
(put 'upcase-region 'disabled nil)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Automatically enter debug mode upon error
;;(setq debug-on-error t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Have locate use spotlight search (Mac only)
(setq locate-command "mdfind")

;; Make pdflatex the default Tex exporter
(setq latex-run-command "pdflatex")

;; Make Preview the default pdf viewer
;; (progn					
;;     (require 'openwith)                                                    
;;     (openwith-mode t)                                                      
;;     (setq openwith-associations '(("\\.pdf\\'" "/Applications/Preview.app/Contents/MacOS/Preview" (file))))          
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Open up your init file for editing
(global-set-key (kbd "C-c I") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))


;; Transpose buffer location with C-x 4 t
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

;; Display pdfs inline in org-mode
(setq image-file-name-extensions
   (quote
    ("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp")))
(setq org-image-actual-width 600)
(setq org-imagemagick-display-command "convert -density 600 \"%s\" -thumbnail \"%sx%s>\" \"%s\"")
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This
can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (unless refresh
    (org-remove-inline-images)
    (if (fboundp 'clear-image-cache) (clear-image-cache)))
  (save-excursion
    (save-restriction
      (widen)
      (setq beg (or beg (point-min)) end (or end (point-max)))
      (goto-char beg)
      (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                        (substring (org-image-file-name-regexp) 0 -2)
                        "\\)\\]" (if include-linked "" "\\]")))
            old file ov img)
        (while (re-search-forward re end t)
          (setq old (get-char-property-and-overlay (match-beginning 1)
                                                   'org-image-overlay)
        file (expand-file-name
                      (concat (or (match-string 3) "") (match-string 4))))
          (when (file-exists-p file)
            (let ((file-thumb (format "%s%s_thumb.png" (file-name-directory file) (file-name-base file))))
              (if (file-exists-p file-thumb)
                  (let ((thumb-time (nth 5 (file-attributes file-thumb 'string)))
                        (file-time (nth 5 (file-attributes file 'string))))
                    (if (time-less-p thumb-time file-time)
            (shell-command (format org-imagemagick-display-command
                           file org-image-actual-width org-image-actual-width file-thumb) nil nil)))
                (shell-command (format org-imagemagick-display-command
                                         file org-image-actual-width org-image-actual-width file-thumb) nil nil))
              (if (and (car-safe old) refresh)
                  (image-refresh (overlay-get (cdr old) 'display))
                (setq img (save-match-data (create-image file-thumb)))
                (when img
                  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                  (overlay-put ov 'display img)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put ov 'modification-hooks
                               (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

;; Add pretty comment boxes to code
(defun bjm-comment-box (b e)
"Draw a box comment around the region but arrange for the region to extend to at least the fill column. Place the point after the comment box."
(interactive "r")
(let ((e (copy-marker e t)))
  (goto-char b)
  (end-of-line)
  (insert-char ?  (- fill-column (current-column)))
  (comment-box b e 1)
  (goto-char e)
  (set-marker e nil)))
(global-set-key (kbd "C-c b b") 'bjm-comment-box)

;; Copy current line
(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
    kill-line, so see documentation of kill-line for how to use it including prefix
    argument and relevant variables.  This function works by temporarily making the
    buffer read-only."
  (interactive "P")
  (let ((buffer-read-only t)
	(kill-read-only-ok t))
    (kill-line arg)))
;; optional key binding
(global-set-key "\C-c\C-k" 'copy-line)

;; Insert R code blocks in org-mode with C-c R
(defun org-insert-source-block (name)
  "Asks name
Inserts org-mode source code snippet"
  (interactive "sname? ")
  (insert 
   (if (string= name "")
       "#+BEGIN_SRC R :session \"global\" :results output :exports both

#+END_SRC"
     (format "#+NAME: %s
#+header: :width 1000 :height 1000 :R-dev-args
#+BEGIN_SRC R :session \"global\" :file %s.png :results output graphics :export both

#+END_SRC" name name
)
     ) )
  (forward-line -1)
  (goto-char (line-end-position))
  ;;(org-edit-src-code)
  )
(define-key org-mode-map (kbd "\C-c R") 'org-insert-source-block)

;; Start up OSC interactive node
(defun start-interactive-node (arg)
  "Log in to OSC and request interactive node"
  (interactive "P")
  (shell)
  (process-send-string "shell" "ssh -Y -C osu8143@owens.osc.edu\n")
  (sleep-for 1)
  (process-send-string "shell" (concat (concat "qsub -I -X -l nodes=1:ppn=12 -l walltime=" (read-from-minibuffer "Walltime (hh:mm:ss):" "1:00:00")) " -A PCON0005\n"))
  (setq load-R (read-from-minibuffer "Load R (y/n)? "))
  (if (string= load-R "y")
      (progn
	(process-send-string "shell" "module load R\n")
	     (process-send-string "shell" "R\n")
	     (ess-remote "shell" "R"))
    (message "Done"))
  ;;this doesn't work
  ;;(dired "/ssh:osu8143@owens.osc.edu:/users/PAS1143/osu8143/")
  )

;; Insert R code blocks in Rmarkdown files with C-c R
(defun rmd-insert-source-block (name)
  "Asks name
Inserts Rmarkdown source code snippet"
  (interactive "sname? ")
  (insert 
   (if (string= name "")
       "```{r}

```"
     (format        "```{r, %s}

```" name
)
     ) )
  (forward-line -1)
  (goto-char (line-end-position))
  )
(define-key markdown-mode-map (kbd "\C-c R") 'rmd-insert-source-block)

(defun rmd-insert-header (title)
  "Asks title
Inserts Rmarkdown header and default options chunk"
  (interactive "stitle? ")
  (insert (format "---
title: %s
author: \"Andrew Patt\"
date: \"`r format(Sys.time(), '%d %B, %Y')`\"
output:
  tufte::tufte_handout:
    keep_tex: true
---

```{r,echo=FALSE}
knitr::opts_chunk$set(echo=FALSE, message=FALSE,warning=FALSE, 
    fig.height=6,fig.width = 12,fig.fullwidth = TRUE,tidy=TRUE)
```" title))
  )
(define-key markdown-mode-map (kbd "\C-c H") 'rmd-insert-header)

(defun rmd-send-chunk ()
  "Send current R chunk to ess process."
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode) 
       (pm-with-narrowed-to-span nil
         (goto-char (point-min))
         (forward-line)
         (ess-eval-region (point) (point-max) nil nil 'R)))) 
(define-key ess-mode-map (kbd "\C-c RET") 'rmd-send-chunk)
