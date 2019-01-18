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
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(ess-swv-pdflatex-commands (quote ("pdflatex" "texi2pdf" "make")))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(org-confirm-babel-evaluate nil)
 '(org-tag-faces (quote (("liposarcoma" . "yellow"))))
 '(package-archives
   (quote
    (("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (popup-complete auto-complete spaceline-config persp-mode spaceline flycheck openwith org2blog elpy poly-R poly-markdown ess ess-view diminish use-package spacemacs-theme magit org org-bullets markdown-mode markchars))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-function-call-face ((t (:foreground "#a65200"))))
 '(font-lock-comment-face ((t (:background "#D3D3D3"))))
 '(org-table ((t (:background "#D8C3E5" :foreground "#655370")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS
(use-package ess
  :defer t
  :init (require 'ess-site)
  :config (ess-toggle-underscore nil))
(setq ess-use-auto-complete t)

;; elpy (for python editing)
(use-package elpy
  :ensure t
  :config (elpy-enable))

;; flycheck for python syntax checking
 (use-package flycheck
   :config (when (require 'flycheck nil t)
 	    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
 	    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; Org mode
 (use-package org
   :ensure org-plus-contrib
   :bind (("\C-c l" . org-store-link)
	  ("\C-c a" . org-agenda)
	  ("\C-c i" . org-toggle-inline-images))
   :config (progn
	     (setq org-hide-emphasis-markers t)
	     (org-babel-do-load-languages
	      'org-babel-load-languages
	      '((R . t)
		(python . t)))
	     ;(setq org-startup-with-inline-images t)
	     (setq org-log-done t)))

;; Fancy org-bullets
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;spaceline
(use-package spaceline
  :ensure t)
(require 'spaceline-config)
(spaceline-spacemacs-theme)

(use-package persp-mode
  :ensure t)

;; ess-view, REQUIRES "TAD" CSV VIEWER APP
(use-package ess-view
 :defer t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linking to my wordpress blog and OSC account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "~/.emacs.d/elisp/blog_and_osc_login.el") (load "~/.emacs.d/elisp/blog_and_osc_login.el") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Always show line numbers
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

;; Set background to light gray
(set-background-color "#DCDCDC")
(set-face-attribute 'fringe nil :background "#DCDCDC" :foreground "#DCDCDC")
(set-face-background 'linum "#DCDCDC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off excessive alarm bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Open up your init file for editing
(global-set-key (kbd "C-c I") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; autocomplete paired brackets (don't autocomplete '<')
(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))
;; Re-load your buffers from the previous session on startup
;;(desktop-save-mode 1)

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

;; Make Preview the default pdf viewer
(progn					
    (require 'openwith)                                                    
    (openwith-mode t)                                                      
    (setq openwith-associations '(("\\.pdf\\'" "/Applications/Preview.app/Contents/MacOS/Preview" (file))))          
)

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
