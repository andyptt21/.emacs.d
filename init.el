;;: Commentary:
;;.emacs
;; (setq url-proxy-services '(("https" . "http://dtn01-e0:3128")
;;  			   ("http" . "http://dtn01-e0:3128")))

;;; Code:
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(diff-switches "-u")
 '(ess-swv-pdflatex-commands '("pdflatex" "texi2pdf" "make"))
 '(grep-find-command
   '("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
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
     ("???" . "#dc752f")))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-confirm-babel-evaluate nil)
 '(package-archives
   '(("melpa-stable" . "http://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(exec-path-from-shell major-mode-hydra reformatter flycheck mood-modeline mood-line ess-view-data csv-mode counsel treemacs-icons-dired treemacs doom-modeline poly-noweb org-plus-contrib spaceline-all-the-icons mode-icons all-the-icons org-journal pubmed fill-column-indicator auctex poly-org ess-view ess-smart-underscore pdf-tools org-ref popup-complete auto-complete spaceline-config persp-mode spaceline openwith org2blog elpy poly-R poly-markdown ess diminish use-package spacemacs-theme magit org org-bullets markdown-mode markchars))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef")))

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(load-theme 'doom-nord)
(normal-erase-is-backspace-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo")))))

;;use-package

;; ESS
(use-package ess
  :ensure t
  :init (require 'ess-r-mode)
  :config
  (ess-toggle-underscore nil)
  (add-to-list 'load-path "/elpa/ess-18.10.2")
  (setq inferior-R-program-name "/usr/local/bin/R")
  ;;(setq ess-use-auto-complete t)
  ;;(require 'auto-complete)
  ;;(require 'auto-complete-config)
  ;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
  ;;(ac-config-default)
  ;;(auto-complete-mode)
  (require 'ess-site)
  (ess-toggle-underscore nil)
  (setq ess-eval-visibly 'nowait))

;; elpy (for python editing)
 (use-package elpy
   :ensure t
   :config (elpy-enable))

(use-package flycheck
  :defer 2
  :diminish
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))

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
	  ("METABOSPAN" . ?r)
	  ("THESIS" . ?t)
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
	  ("METABOSPAN" . (:foreground "Red" :weight bold))
	  ("THESIS" . (:foreground "SkyBlue" :weight bold))
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
       (sql . t))))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (setq org-odt-preferred-output-format "docx"))

(with-eval-after-load 'org
  ; max newlines 
  (setcar (nthcdr 4 org-emphasis-regexp-components) 30)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

;; Org Ref
(use-package org-ref
  :after org
  :init
  (setq reftex-default-bibliography '("~/Documents/emacs_files/references.bib"))
  (setq org-ref-bibliography-notes "~/Documents/emacs_files/notes.org"
	org-ref-default-bibliography '("~/Documents/emacs_files/references.bib")
	org-ref-pdf-directory "~/Desktop/Papers_of_Interest/")

  (setq helm-completion-bibliography "~/Documents/emacs_files/references.bib")
  (setq org-latex-pdf-process
	'("pdflatex %f" "bibtex %b" "pdflatex %f" "pdflatex %f"))
  (setq helm-completion-library-path "~/Desktop/Papers_of_Interest/")

  (setq helm-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))
  (setq helm-completion-notes-path "~/Documents/emacs_files/notes.org")
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

;; Fancy org-bullets
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package persp-mode)

;; Start a new journal entry with C-c C-j
(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/Documents/emacs_files/journal/"))

(use-package counsel
  :ensure t)

(use-package polymode
  :ensure t
  :pin melpa-stable
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)) ; Markdown files
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode)) ; Sweave files
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode)) ; Sweave files
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)) ; RMarkdown files
  )

(use-package markdown-mode
   :ensure t
   :commands (markdown-mode gfm-mode)
   :mode (("\.text\'" . markdown-mode)
	  ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)))

(use-package poly-noweb
  :ensure t)

(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :ensure t)

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)


;; Download all icons needed for spaceline
(use-package all-the-icons
  :ensure t)

;; Doom modeline for cool, informative modeline
;; (use-package mood-line
;;   :ensure t
;;   :init
;;   (mood-line-mode))

(use-package doom-modeline
:ensure t
:defer t
:hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

;; Easier file access with treemacs
(use-package treemacs
  :ensure t
  :defer t)

;; Pretty icons for dired buffers
(use-package treemacs-icons-dired
  :ensure t
  :defer t
  :config
  (treemacs-icons-dired-mode))


(use-package csv-mode
  :ensure t)

(use-package reformatter
  :config
  (defconst Rscript-command "Rscript")
  (reformatter-define styler
    :program Rscript-command
    :args (list "--vanilla" "-e" "con <- file(\"stdin\")
out <- styler::style_text(readLines(con))
close(con)
out")
    :lighter " styler"))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(use-package hydra
  :bind (("C-c M" . hydra-merge/body)
         ("C-c f" . hydra-flycheck/body)
	 ("C-c w" . hydra-window/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust face)
    "Display an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-faicon (icon str &optional height v-adjust face)
    "Display an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon ':v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-fileicon (icon str &optional height v-adjust face)
    "Display an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-octicon (icon str &optional height v-adjust face)
    "Display an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str)))

;; Hydras
(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-octicon "mark-github" "Magit" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("RET" smerge-keep-current "current")
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("<" smerge-diff-base-upper "upper/base")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "redefine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill current"))))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(defhydra hydra-window (:color pink :hint nil :timeout 20)
  "
         Move                    Resize                      Swap              Split
╭─────────────────────────────────────────────────────────────────────────────────────────┐
         ^_<up>_^                    ^_C-<up>_^                      ^_M-<up>_^            [_v_]ertical
          ^^▲^^                         ^^▲^^                           ^^▲^^              [_h_]orizontal
 _<left>_ ◀   ▶ _<right>_    _C-<left>_ ◀   ▶ _C-<right>_    _M-<left>_ ◀   ▶ _M-<right>_
          ^^▼^^                         ^^▼^^                           ^^▼^^              ╭──────────┐
        ^_<down>_^                  ^_C-<down>_^                    ^_M-<down>_^           quit : [_SPC_]
"
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("h" split-window-below)
  ("v" split-window-right)
  ("C-<up>" hydra-move-splitter-up) 
  ("C-<down>" hydra-move-splitter-down)
  ("C-<left>" hydra-move-splitter-left)
  ("C-<right>" hydra-move-splitter-right)
  ("M-<up>" buf-move-up)
  ("M-<down>" buf-move-down)
  ("M-<left>" buf-move-left)
  ("M-<right>" buf-move-right)
  ("SPC" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linking to my wordpress blog and OSC account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "~/.emacs.d/elisp/blog_and_osc_login.el") (load "~/.emacs.d/elisp/blog_and_osc_login.el") nil)

;;Other stuff

;; Make ESS buffers appear in the RStudio configuration by default
;; (setq display-buffer-alist
;;       `(("^\\*R Dired"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . -1)
;;          (window-width . 0.5)
;;          (reusable-frames . nil))
;;         ("^\\*R"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . 1)
;;          (window-width . 0.5)
;;          (reusable-frames . nil))))

(setq inhibit-startup-screen t)


(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
           (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

;; (setq
;;    split-height-threshold 4
;;    split-width-threshold 40 
;;    split-window-preferred-function 'split-window-really-sensibly)

;; (menu-bar-mode -1)
(tool-bar-mode -1)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(set-face-attribute 'default nil :height 120)

(global-set-key (kbd "C-c I") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; Open up your init file for editing
(global-set-key (kbd "C-c p") (lambda() (interactive)(find-file "~/Documents/my-planner.org")))

;; Better buffer switching
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

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

(defun rmd-insert-header (type)
  "Asks title
Inserts Rmarkdown header and default options chunk"
  (interactive "spdf_or_html?")
  (insert
   (if (string= type "pdf")
   "---
title:
author: \"Andrew Patt\"
date: \"`r format(Sys.time(), '%d %B, %Y')`\"
output:
  pdf_document
---
```{r,echo=FALSE}
knitr::opts_chunk$set(echo=FALSE, message=FALSE,warning=FALSE, 
    fig.height=6,fig.width = 12,fig.fullwidth = TRUE,tidy=TRUE)
```"
      "---
title:
author: \"Andrew Patt\"
date: \"`r format(Sys.time(), '%d %B, %Y')`\"
output:
  html_document:
    code_folding: hide
    theme: cerulean
    highlight: tango
---
```{r,echo=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE, 
    fig.height=6,fig.width = 12,fig.fullwidth = TRUE,tidy=TRUE)
```")
  ) )
(define-key markdown-mode-map (kbd "\C-c H") 'rmd-insert-header)

;; (defun rmd-send-chunk ()
;;   "Send current R chunk to ess process."
;;   (interactive)
;;   (and (eq (oref pm/chunkmode :mode) 'r-mode) 
;;        (pm-with-narrowed-to-span nil
;;          (goto-char (point-min))
;;          (forward-line)
;;          (ess-eval-region (point) (point-max) nil nil 'R)))) 
;; (define-key ess-mode-map (kbd "\C-c RET") 'rmd-send-chunk)

(define-key ess-mode-map (kbd "\C-c \C-e") 'polymode-eval-chunk)

(fset 'eval-chunk-and-move
   [?\C-c ?\C-e ?\M-n ?\C-n ?\M-n ?\C-n])

(define-key ess-mode-map (kbd "\C-c RET") 'eval-chunk-and-move)

(global-set-key (kbd "C-c p") (lambda() (interactive)(find-file "~/planner.org")))

(eval-after-load "ansi-term"
  '(define-key ansi-term-raw-map (kbd "C-c C-y") 'term-paste))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(setq grep-command "grep -rnw ")

(add-hook 'image-mode-hook
  (lambda ()
    (auto-revert-mode)
    (auto-image-file-mode)))

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
(fset 'copy-and-paste-line
   [?\C-a ?\C-  ?\C-e escape ?w return ?\C-y])
(global-set-key (kbd "C-c y") 'copy-and-paste-line)

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
))))

(setq treemacs-show-hidden-files nil)

(which-function-mode 1)
(setq pandoc-binary "/usr/local/bin/pandoc")
(define-key ess-mode-map (kbd "\C-c \C-e") 'polymode-eval-chunk)

(fset 'eval-chunk-and-move
   [?\C-c ?\C-e ?\M-n ?\C-n ?\M-n ?\C-n])

(define-key ess-mode-map (kbd "\C-c RET") 'eval-chunk-and-move)
