;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linking to my wordpress blog and OSC account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org2blog
(use-package org2blog
  :config (setq org2blog/wp-blog-alist
      '(("wordpress"
         :url "https://thescientificshrimper.wordpress.com/xmlrpc.php"
         :username "andyptt21"
         :default-title "The Scientific Shrimper"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil))))

;; Connect to Owens (OSC)
(defun connect-remote ()
  (interactive)
  (dired "/ssh:pattac@biowulf.nih.gov:/home/pattac"))

(defun biowulf-term ()
    (interactive)
    (let ((default-directory "/ssh:pattac@biowulf.nih.gov:/home/pattac"))
      (shell)))

;; Pubmed for searching for bibtex references in emacs (API key is private)
;; Use pubmed-search to search, m to mark (u to unmark) and a to append
(use-package pubmed
  :ensure t
  :defer t
  :config
  (setq pubmed-api-key "93c80c131ce7466dc57c839a8a2f610b9308")
  (setq pubmed-default-bibtex-file "~/Documents/emacs_files/references.bib"))

